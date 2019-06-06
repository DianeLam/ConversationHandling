;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                       -*-
;;;; ------------------------------------------------------------------------
;;;; File name: im-agent.lsp
;;;;    System: Interaction Manager
;;;;    Author: Tom Hinrichs
;;;;   Created: November 16, 2007 13:57:12
;;;;   Purpose: Define the agent class
;;;; ------------------------------------------------------------------------
;;;;  $LastChangedDate: 2017-06-29 21:30:10 -0500 (Thu, 29 Jun 2017) $
;;;;  $LastChangedBy: hinrichs $
 
;;; Modiciations:
;;; 4/23/14 (TRH) moved primitive action definitions to primitive-operators.lsp.

(in-package :agents)

(defvar cl-user::*interaction-manager* nil)
(defvar cl-user::*im* nil "bind an instance of the test-im.")

(defclass interaction-manager (simple-agent)
  ((discourse
    :documentation "EA NLU discourse object, for holding state information"
    :initform nil
    :accessor discourse)
   (current-input-sentence
    :documentation "The surface form of the most recent input sentence"
    :initform ""
    :accessor current-input-sentence)
   (sentence-id
    :documentation "Stashed here to simpify metrics capture."
    :initform 0
    :accessor im::sentence-id)
   (current-system-utterance
    :documentation "The concept name of the latest system output utterance"
    :initform nil
    :accessor current-system-utterance)
   (player-piano
    :initform nil
    :accessor player-piano
    :documentation "The interaction player piano.")
   )
  (:documentation
   "The interaction-manager manages the interactions between the companion and the user."))


(defmethod interaction-manager ((agent interaction-manager))
  (name agent))

;;; Support EEG process viewer:
(defmethod sort-order ((agent interaction-manager)) 4)

(defvar cl-user::*use-nulex-lexicon* nil)

(defmethod initialize-agent :around ((agent interaction-manager)
                                     facilitator-uri kb-path)
  (declare (ignore facilitator-uri kb-path))
  (call-next-method) ;; default method creates a reasoner, which we need.
  (if (fire:open-ontology? fire::*kb*)
    (ea::setup-ea-from-nulex-ocyc) 
    (ea::setup-ea-from-comlex))
  (let ((reasoner (reasoner agent)))
    (fire::in-reasoner reasoner)
    (unless (fire::im-source-of reasoner)
      (fire::add-im-source reasoner))
    ;;; Create the discourse object
    ;;; N.B. We are currently defaulting the :context arg to init-discourse
    ;;; to nil, if we had some standard Mt to use as the starting point, we should
    ;;; put that in there.  Hopefully, this can be tweaked via assertions more easily
    ;;; elsewhere.
    (let ((discourse (ea::init-discourse
                      :reasoner reasoner)))
      (setf (discourse agent) discourse)
      ;; Stash in source also, so that accessors can grab it
      (setf (fire::discourse (fire::im-source-of reasoner)) discourse)
      (ea::setup-browser qrg:*default-port*)  ; start up the web server w/o browser (yet)
      (fire::with-eanlu-discourse (discourse agent) (initialize-session agent)))))


(defmethod subscribe-to-default-queries ((agent interaction-manager))
  ;;; No!  Cut this out (TRH, 6-16-13)
  ;;; deictic reference subscription
  ;;;  (subscribe-to-all agent 'd::(perceptual-info-source-data ?fact) #'new-facts-in-session)
  )

(defmethod new-facts-in-session% :after ((agent interaction-manager) msg reply)
  (declare (ignore msg))
  (setq reply (strip-forwarding-header reply))
  (when (eq (language reply) 'user::fire)
    (let ((facts (content reply))
          (performative (performative reply)))
      (debug-trace "~%~s IM facts:~%  ~s" performative facts))))

;;; Do *not* call ea:shutdown.  It can only confuse the kb reference counts when
;;; running locally.  EA no longer does any cleanup anyway.
(defmethod stop-agent :around ((agent interaction-manager) &optional immediately?)
  (declare (ignore immediately?))
  (finalize-session agent)   ; Right now, this is only defined for executives!
  (call-next-method))  ; make sure kb & communications are all shut down.

(defmethod advertise-services :after ((agent interaction-manager))
  (let ((msg-id (get-next-msg-id agent)))
    (advertise-for agent
      `(cl-user::achieve
        :in-reply-to ,msg-id
        :language cl-user::meta
        :content d::(interpret ?utterance ?string))
      msg-id)
    (setf cl-user::*interaction-manager* agent)
    (setf cl-user::*im* agent)
    agent
    ))


;;; Start it off with a task to reify the current session case:
(defmethod initialize-session ((agent interaction-manager))
  (let ((session-id (current-session agent))
        (domain (current-domain agent)))
    (when session-id
      (maybe-specialize-interpretation-context (session-context agent) domain)
;      (handler-bind ((error
;                         #'(lambda (condition)
;                             (declare (ignorable condition))
;                             (inform-user agent
;                                          (format nil "Unable to execute initial plan.  Flat files are probably not loaded: ~S" condition))
;                             (return-from initialize-session nil))))
      (user::with-auto-zoom-and-exit ((error-log-path "setupSessionCase-error-log")
                                      :exit nil)
        (execute agent `(d::actionSequence (d::TheList (d::setupIMSessionCase ,session-id))))))))

(defun maybe-specialize-interpretation-context (sc domain)
  (when domain
    (let ((language-mt (list 'data::LanguageMtFn domain))
          (grammar-mt (list 'data::GrammarMtFn domain))
          (interp-mt (list 'data::InterpretationMtFn domain))  ; This should be a last resort.
          (instr-mt '(data::InterpretationMtFn data::Instruction)))
      ;; Switch-in domain-specific vocabulary:
      (when (fire::ask-it (list 'data::genlMt language-mt 'data::EANLU))
        (setf parser::*language-mt* language-mt))
      ;; Augment existing grammar with domain specific grammar additions
      (when (mt-exists? grammar-mt)
        (parser::add-grammar-from-mt grammar-mt))
      (when (mt-exists? interp-mt)
        (fire::kb-store (list 'data::interpretationContext interp-mt) :mt sc))
      (when (fire::ask-it (list 'data::genlMt interp-mt instr-mt))
        (fire:kb-store (list 'data::interpretationContext instr-mt) :mt sc)))))


(defun mt-exists? (mt-name)
  ;; Hah! This can't be ask-it, because if the mt being queried is specified with a 
  ;; InterpretationMtFn functor, it will always return true!!
  (fire::retrieve-it `(d::isa ,mt-name d::Microtheory)))

;;; This is like inform-user, but goes to the interaction-manager transcript-window.
;;; Consequently, it's only defined for an interaction-manager agent.
;;; It's invoked from doRespond (as opposed to doAnnounce)
;;; Because of the way that user-communications combines messages to minimize traffic,
;;; we have to batch the utterance number and string into a single cons for transmission.
;;; Really ugly, but there's no alternative right now.
#-:web-ui
(defmethod tell-user ((from-agent interaction-manager) (utterance-number integer) (msg string))
  (add-user-comm-task from-agent
                      `(d::achieve
                        :language d::meta
                        :content (user::tell-user (,utterance-number ,msg)))))

;;; Intercept these calls to route them to the facilitator for uploading to the browser:
#+:web-ui
(defmethod tell-user ((from-agent interaction-manager) (utterance-number integer) (msg string))
  (send-facilitator from-agent
    `(cl-user::achieve
      :receiver cl-user::facilitator
      :language cl-user::fire
      :content (cl-user::tell-user (,utterance-number ,(remove #\Newline msg)))))) ; no linefeeds permitted in JSON.


(defun kiosk-create-user (id) ;;This function was added by the Conversation Handling (C.H.) team as a way to create a microtheory based on each of the unique Slack IDs or any IDs. 
  ;The microtheory will be used to store information and utterances uniqiue to the that ID.
 (let* ((id (intern id :data))
        (microtheory  id))
  (fire:kb-store `(data::isa ,id data::Agent-Generic) :mt microtheory)
  (fire:kb-store `(data::isa ,id data::NUPerson) :mt microtheory)))

(defun add-dialogue (id text) ;;This function was added by the C.H. team as a way to store all the utterances that a user states and this can then be expanded to use as a log or for personalized responses.
(fire:kb-store `(data::utteranceInDialogue ,id ,text) :mt id)
)

;;This function was added by the  Conversation Handling team as temporary hard code way to recognize users name. 
; (defun finds-name (id text) 
; 	(if (search "name is" text) (and (setq nameindex (+ 8 (search "name is" text))) (fire:kb-store `(data::fullName ,id ,(subseq text nameindex)) :mt id))
; 	(add-dialogue id text)))


(defmethod handle-achieve-request ((agent interaction-manager) 
                                   (command (eql 'd::interpret))
                                   content language msg)
  (declare (ignore language))  
  (with-on-error-fn ((make-general-error-fn agent msg))
    (spawn-agent-thread ("interpreting user utterance")
      'interpret-utterance agent msg (second content) (third content) (fourth content)))) ;C.H. included teh fourth content parameter so that we can have a way for the user id to be stored. This
;can be expanded to later include more parameters.


(defun interpret-utterance (agent msg utterance-number text user-id); C.H. included the "user-id" parameter so that the interpret utterances must have a user id for interpretation 
  (declare (ignore msg))
  (unless (and (numberp utterance-number) (stringp text))
    (error "ccl command interpret has changed to include an utterance number.  Need to update session-manager."))
  (setf (turn agent) utterance-number)
  (when user-id (kiosk-create-user user-id)) ;This when statement was added by C.H. team in order to do the create user function only when an user-id is passed in.
  (fire::with-eanlu-discourse (discourse agent)
    (let* ((context (session-context agent))
           (seq `(data::actionSequence
                  (data::TheList
                   (data::processUserUtterance ,context ,text)))))
      (execute agent seq)))) 



;;; browse-ea
;;;

(defmethod handle-achieve-request ((agent interaction-manager) 
                                   (command (eql 'user::browse-ea))
                                   content language msg)
  (declare (ignore content language msg))
  (let* ((reasoner (or (reasoner agent) fire:*reasoner*))
         (url (and reasoner (url-for-browse-ea agent))))
    (if reasoner
      (values url 'user::tell)
      (primitive-query-error agent "No reasoner open."))))

(defun url-for-browse-ea (agent)
  ;; Start ea:browse if it hasn't already been started and returns a URL that can
  ;; be used to browse the agent's working memory.
  (when (find-package :ea)
    ;(ea:browse)
    (ea::setup-browser qrg:*default-port*) ; Don't invoke the browser automatically
    ;; Now return the full url
    (let* ((host (maybe-external-hostname (agent-host agent)))
           (port ea::*browser-port*))
      (format nil "http://~A:~A/ea/ui" host port))))

(defmethod handle-achieve-request ((agent interaction-manager) 
                                   (command (eql 'user::inspect-parse))
                                   content language msg)
  (declare (ignore content language msg))
  (let* ((reasoner (or (reasoner agent) fire:*reasoner*))
         (url (and reasoner (url-for-inspect-parse agent))))
    (if reasoner
      (values url 'user::tell)
      (primitive-query-error agent "No reasoner open."))))

(defun url-for-inspect-parse (agent)
  ;; Start ea:browse if it hasn't already been started and returns a URL that can
  ;; be used to browse the agent's working memory.
  (when (find-package :ea)
    (ea::setup-browser qrg:*default-port*) ; don't invoke the browser automatically
    ;; Now return the full url
    (let* ((host (maybe-external-hostname (agent-host agent)))
           (port ea::*browser-port*))
      (format nil "http://~A:~A/ea/parse" host port))))


;;;
;;; Meta-query performative handling
;;;

;;; (interaction-diagnostics <session-id> ?diagnostics-list)

(defmethod do-primitive-query ((agent interaction-manager)
                               (performative (eql 'cl-user::ask-one))
                               (language (eql 'cl-user::meta))
                               msg
                               content)
  (let* ((response (or (response msg) :pattern))
         (reply-perf 'cl-user::tell)
         (pred (first content))
         (value nil))
    (cond ((and (eq pred 'user::interaction-diagnostics)
                (eq (second content) (current-session agent)))    ;; specify for eventual cross-session accumulation
           (let ((ut (counter-value agent 'd::userUtterances))    ;;(im::user-utterances agent))
                 (ta (counter-value agent 'd::totalAmbiguity))    ;;(im::total-ambiguity agent))
                 (ca (counter-value agent 'd::currentAmbiguity))  ;;(im::current-ambiguity agent))
                 (suc (counter-value agent 'd::successCount))     ;;(im::successes agent))
                 (fails (counter-value agent 'd::failureCount))   ;;(im::failures agent))
                 (fp (counter-value agent 'd::fragmentaryParses)) ;;(im::fragmentary-parses agent))
                 (sya (counter-value agent 'd::syntacticAmbiguities)) ;; (im::syntactic-ambiguities agent))
                 (drsf (counter-value agent 'd::drsLiftingFailureCount)) ;;(im::drs-lifting-failures agent))
                 (sema (counter-value agent 'd::semanticAmbiguityCount)) ;;(im::semantic-ambiguities agent))
                 (traf (counter-value agent 'd::translationFailureCount)) ;;(im::translation-failures agent))
                 (qaf (counter-value agent 'd::qaFailureCount))   ;;(im::qa-failures agent))
                 (metf (counter-value agent 'd::metonymyFailureCount))) ;;(im::metonymy-failures agent)))
             (setq value (list 'd::TheList ut ta ca suc fails fp sya drsf sema traf qaf metf))
             (if (eq response :pattern)
               (values (list pred (second content) value) reply-perf)
               (values value reply-perf))))
          (t
           (call-next-method)))))  ; let general facilities for simple-agent handle it

;;;
;;; Custom setters for maintaining statistics & metrics
;;;


;;;(defsetf im::current-ambiguity (interaction-manager) (new-value)
;;;  `(progn (setf (slot-value ,interaction-manager 'current-ambiguity) ,new-value)
;;;     (when (fire::open? (reasoner ,interaction-manager))
;;;       (fire::kb-store (list 'data::sentenceAmbiguity (im::sentence-id ,interaction-manager) ,new-value)
;;;         :mt (list 'data::MetadataContextFn (agents::current-session ,interaction-manager))))
;;;     ,new-value))




;;; just to shake down the plumbing:
(defun parrot-response (agent msg content)
  (send agent (sender msg) 
    `(cl-user::achieve
      :language cl-user::meta
      :content (cl-user::inform-user ,(format nil "Received ~s" content)))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
