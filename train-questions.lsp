;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: train-questions.lsp
;;;;    System: 
;;;;    Author: Crouse
;;;;   Created: November 26, 2018 12:16:12
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2019-04-12 13:38:00 -0500 (Fri, 12 Apr 2019) $
;;;;  $LastChangedBy: Crouse $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *full-question-set* 
  '(

 ;Previous trained question from previous classes/QRG group is folded in the 3 dots(if using Sublime) below       
    ((:comment . "Where is {person}'s office?")
     (:lf (and (officeLocation person123 office123)
               (psikiShowMap office123 office123))
          (isa person123 NUPerson)
          (isa office123 StringObject))
     (:settings :use-gen-antec-isas)
     (:texts 
      "Where is Professor Forbus's office?"
      "Where is Professor Forbus"
      "Where can I find Doctor Sood's office?"
      "Can you tell me where I can find Professor Forbus?"
      "Would you tell me where Professor Forbus' office is?"
      "Where does professor Forbus work?"
      "How can I find professor Forbus?"
      "How do I get to Professor Rubenstein's office?"
      "Is Jason Hartline's office in this building?"
      "Does Professor Horswill work here?"
      "Does Professor Horswill have an office here?"
      "Is Professor Tov's office here?"
      "Is Professor Tov's office in this building?"
      "Is Professor Tov's office on this floor?"
      ))
    
    ((:comment . "Asking the instructor of a course")
     (:lf (and (courseInstructor course123 person123)
               (likelyCourseTime course123 probable-quarter-of123)
               (psikiSayThis (courseInstructor course123 person123)))
          (isa person123 CSFaculty)
          (isa course123 NUCourse))
     (:settings . (:use-gen-antec-isas))
     (:texts 
     
      "Who is teaching eecs 371?"
      "Who is teaching eecs 359?"
      "Who is teaching eecs 361?"
      "Who teaches eecs 371?"
      "Who teaches eecs 359?"
      "Who teaches eecs 361?"
      "Do you know who teaches eecs 100?"
      "Who is the professor for eecs 371?"
      "Who is the professor for eecs 359?"
      "Who is the professor for eecs 361?"
      ))
    
    ((:comment . "Asking what someone teaches")
     (:lf (and (courseInstructor course123 person123)
               (likelyCourseTime course123 probable-quarter-of123)
               (psikiSayThis (courseInstructor course123 person123)))
          (isa person123 CSFaculty)
          (isa course123 NUCourse))
     (:settings . (:use-gen-antec-isas))
     (:texts 
      "What does Professor Horswill teach?"
      "What does Professor Sood teach?"
      "What does Willie Wilson teach?"
      "What class does Professor Horswill teach?"
      "What class does Professor Sood teach?"
      "What class does Willie Wilson teach?"
      ))
    
    ((:comment . "Who is teaching {course} this quarter?")
     (:lf (and (courseInstructor course123 person123)
               (likelyCourseTime course123 probable-quarter-of123)
               (psikiSayThis (courseInstructor course123 person123)))
          (isa person123 CSFaculty)
          (isa course123 NUCourse))
     (:settings . (:use-gen-antec-isas ))
     (:texts 
      "Who is teaching eecs 371 this quarter?"
      "Who is teaching eecs 359 this quarter?"
      "Who is teaching eecs 361 this quarter?"
      "Who is the professor for eecs 371 this quarter?"
      "Who is the professor for eecs 359 this quarter?"
      "Who is the professor for eecs 361 this quarter?"
      "Do you know the teacher for eecs 325 this quarter?"
      "Show me the professor for eecs 325 this quarter."
      "Tell me the name of the professor for eecs 214."
      "Is professor Forbus teaching eecs 214 this quarter?"
      "Do you know whether Professor Forbus is teaching eecs 371 this quarter?"
      "Do you know whether Professor Forbus is the professor for eecs 371 this quarter?"
      "Do you know if Professor Forbus is teaching eecs 371 this quarter?"
      "Do you know if Professor Forbus is the professor for eecs 371 this quarter?"
      ))
        
    ((:comment . "When is {course}?")
     (:lf (and (courseTimeString course123 time123)
               (likelyCourseTime course123 probable-quarter-of123)
               (psikiSayThis (courseTimeString course123 time123)))
          (isa course123 NUCourse)
          (isa time123 StringObject))
     (:settings . (:use-gen-antec-isas))
     (:texts 
      "When is eecs 371?"
      "When is eecs 359?"
      "When is eecs 361?"
      "What time is eecs 371?"
      "What time is eecs 359?"
      "What time is eecs 361?"
      "What days are eecs 214?"
      "Which days are eecs 214?"
      "Do you know when eecs 205 is?"
      "Does eecs 205 meet today?"
      ))

    ((:comment . "What's {person}'s email?")
     (:lf (emailOf person123 address123)
          (isa person123 NUPerson)
          (isa address123 StringObject))
     (:settings :use-gen-antec-isas)
     (:texts 
      "What is Douglas Downey's email?"
      "How can I contact Douglas Downey?"
      "Do you know Douglas Downey's email?"
      "Do you have Douglas Downey's email?"
      "Can you tell me Douglas Downey's email?"
      "Show me Douglas Downey's email."
      "Has Douglas Downey listed his email?"
      "Which email of Douglas Downey's do you have?"
      "Is there an email of Douglas Downey available?"
      "Do you have Douglas Downey's email lying around?"
      "Do you happen to have Douglas Downey's email?"
      "What is Douglas Downey's contact information?"
      "Do you know Douglas Downey's contact information?"
      "Do you have Douglas Downey's contact information?"
      "Can you tell me Douglas Downey's contact information?"
      "Show me Douglas Downey's contact information."
      "Has Douglas Downey listed his contact information?"
      "Which contact info of Douglas Downey's do you have?"
      "Is there contact information for Douglas Downey available?"
      "Do you have Douglas Downey's contact information lying around?"
      "Do you happen to have Douglas Downey's contact information?"
      ))
    
    ;; Questions about rooms
    ((:comment . "Where is {room}?")
     (:lf (and (psikiSayThis "The location is displayed on the map.")
               (prettyString loc123 string123)
               (psikiShowMap string123 string123))
          (isa loc123 NUMuddPlace)
          (isa string123 StringObject))
     (:settings :use-gen-antec-isas)
     (:texts 
      "Where is room 3011?"
      "Can you tell me how to find room 3011?"
      "I'm looking for room 3011?"
      "Where can I find room 3011?"
      "How can I get to room 3011?"
      ))
    
    
    ((:comment . "What's {person}'s webpage address?")
     (:lf 
      (personalWebsite person123 webpage123)
      (isa person123 NUPerson)
      (isa webpage123 StringObject)
      )
     (:texts 
      "What is Ken Forbus's webpage address?"
      "What is Anindya De's website?"
      "What is Jason Wilson's homepage?"
      "Could you tell me Ken Forbus's webpage?"
      "Do you know Ken Forbus's webpage?"
      "Show me Ken Forbus's homepage."
      "Where can I get the information about Professor Forbus?"
      "Show me Ken Forbus's information."
      ))
    
    ((:comment . "Who is in the {academic group}?")
     (:lf 
      (nuGroupMember group123 person123)
      (isa person123 NUPerson)
      (isa group123 NUGroup)
      )
     (:texts 
      "Who is in algorithms?"
      "Which faculty are in algorithms?"
      "Who are the algorithms faculty?"
      "Which professors are in algorithms?"
      "Who works in algorithms?"
      "Which group is anindya de in?"
      "Which students are in algorithms?"
      "Who does research in algorithms?"
      "Who works in algorithms?"
      "Who are the algorithms faculty?"
      "Does anindya de do research in algorithms?"
      "Is anindya de in algorithms?"
      "Is anindya de a algorithms professor?"
      "Is anindya de a algorithms faculty?"
      ))


;Question trained by the Conversation Handling Team using predicates ontologized by the Teaching Kiosk Team.
;Asking questions about who is an expert/speciliazes in a certain topic.
((:comment . "Who is an expert of {academic group}?")
    (:lf 
      (and (expertInAcademicTopic person123 topic123)
        (psikiSayThis (and "One expert is " person123)))
      (isa person123 NUPerson)
      (isa topic123 AcademicTopic)
      )
     (:texts 
      "Who is an expert of natural language processing?" 
      "Who is an expert of machine learning? " 
      "Who is an expert in natural language processing?"
      ))


;Question trained by the Conversation Handling Team using predicates ontologized by the Teaching Kiosk Team.
;Asking questions about who teaches classes about a certain topic.
((:comment . "Who teaches classes about {topic}?")
   (:lf (and (academicTopicOf course123 topic123) 
             (courseInstructor course123 person123)
             (psikiSayThis (courseInstructor course123 person123)))
        (isa person123 NUFaculty)
        (isa course123 NUCourse)
        (isa topic123 AcademicTopic))
   (:settings . (:use-gen-antec-isas))
   (:texts  
    "Who teaches classes about computer graphicss?" ;"graphics" is spelled with 2 "s" because graphics is mapped to the NUComputerGraphics group and through testing we found out that when two things map to one word it becomes ambiguous on how Companions should parse it   
    "Who teaches classes about natural language processing?"
    ))


;Question trained by the Conversation Handling Team using predicates ontologized by the Teaching Kiosk Team.
;Asking questions about what topic is similar to/what topic is in the same breadth as the topic asked about.
((:comment . "What topic is similar to {topic}?")
     (:lf 
            (and (subTopicOf stubtopic456 topic123 1)
            (subTopicOf  stubtopic456 topic678  1)
            (psikiSayThis topic678)
            (different topic678 topic123))
      (isa topic123 AcademicTopic )
      (isa topic678 AcademicTopic )
      (isa stubtopic456 AcademicTopic ))
     (:settings . (:use-gen-antec-isas))
     (:texts 
     
      "What topic is like graphicss?" 
      "What topic is like deep learning?" 
      "What are some topics in natural language processing?" 
      "What are some topics about natural language processing?"
      "What is a topic in natural language processing?" 
      "What is a topic in machine learning?" 
      "What is a topic in computer graphicss?" 
      "What is a topic in animation?"
      "What is a topic in hardware?"
      "What is a topic about hardware?" 
      ))
    
  ;;;;;;;;;;;;  This code was the beginning of a training case for handling a 
  ;;;;;;;;;;;;  declarative utterance.  The obstacle was in resolving person123 - 
  ;;;;;;;;;;;;  we were not sure how to capture the user ID passed in to the 
  ;;;;;;;;;;;;  Interaction Manager along with the utterance; one idea was to 
  ;;;;;;;;;;;;  concatenate the utterance and the user ID, and then adjust the
  ;;;;;;;;;;;;  EA NLU to interpret the user ID as such.  We did not have time
  ;;;;;;;;;;;;  to fully explore this, however.
  ;;;  
  ;;;  ((:comment . "A user declaring they are interested in a topic")
  ;;;  (:lf 
  ;;;  (and
  ;;;     (ist-information mt123 (isa person123 NUPerson))
  ;;;     (storeFact (interest person123 topic123) mt123)
  ;;;     (psikiSayThis  "Thanks for sharing, I will keep that in mind."))
  ;;;   (isa person123 NUPerson)
  ;;;   (isa topic123 AcademicTopic)
  ;;;   (isa mt123 Microtheory)
  ;;;   )
  ;;;  (:texts 
  ;;;   "I am interested in algorithms."
  ;;;   "I think cognitive systems are cool."
  ;;;   "I like studying algorithms."
  ;;;   "Graphics is an interesting field."
  ;;;   "I enjoy learning about graphics."
  ;;;   "I really like databases."
  ;;;   "Algorithms are fun."
  ;;;   "My favorite subject is AI."
  ;;;   "I like working with reasoning."
  ;;;   "HCI is cool."
  ;;;   "I'm pretty into machine learning."
  ;;;   ))

  
    
    ))

(setf *min-question-set* 
  '(
        
    ; ((:comment . "Where is {person}'s office?")
    ;  (:lf (and (officeLocation person123 office123)
    ;            (psikiShowMap office123 office123))
    ;       (isa person123 NUPerson)
    ;       (isa office123 StringObject))
    ;  (:settings :use-gen-antec-isas)
    ;  (:texts 
    ;   "Where is Professor Forbus's office?"
    ;   "Where is Professor Forbus"
    ;   ;"Where can I find Doctor Sood's office?"
    ;   ;"Can you tell me where I can find Professor Forbus?"
    ;   ;"Would you tell me where Professor Forbus' office is?"
    ;   ;"Where does professor Forbus work?"
    ;   "How can I find professor Forbus?"
    ;   ;"How do I get to Professor Rubenstein's office?"
    ;   ;"Is Jason Hartline's office in this building?"
    ;   "Does Professor Horswill work here?"
    ;   ;"Does Professor Horswill have an office here?"
    ;   "Is Professor Tov's office here?"
    ;   ;"Is Professor Tov's office in this building?"
    ;   ;"Is Professor Tov's office on this floor?"
    ;   ))
    
    ; ((:comment . "Asking the instructor of a course")
    ;  (:lf (and (courseInstructor course123 person123)
    ;            (likelyCourseTime course123 probable-quarter-of123)
    ;            (psikiSayThis (courseInstructor course123 person123)))
    ;       (isa person123 CSFaculty)
    ;       (isa course123 NUCourse))
    ;  (:settings . (:use-gen-antec-isas))
    ;  (:texts 
    ;   "Who is teaching eecs 371?"
    ;   ;"Who is teaching eecs 359?"
    ;   ;"Who is teaching eecs 361?"
    ;   ;"Who teaches eecs 371?"
    ;   ;"Who teaches eecs 359?"
    ;   ;"Who teaches eecs 361?"
    ;   ;"Do you know who teaches eecs 100?"
    ;   "Who is the professor for eecs 371?"
    ;   ;"Who is the professor for eecs 359?"
    ;   ;"Who is the professor for eecs 361?"
    ;   ))
    
    ; ((:comment . "Asking what someone teaches")
    ;  (:lf (and (courseInstructor course123 person123)
    ;            (likelyCourseTime course123 probable-quarter-of123)
    ;            (psikiSayThis (courseInstructor course123 person123)))
    ;       (isa person123 CSFaculty)
    ;       (isa course123 NUCourse))
    ;  (:settings . (:use-gen-antec-isas))
    ;  (:texts 
    ;   "What does Professor Horswill teach?"
    ;   ;"What does Professor Sood teach?"
    ;   ;"What does Willie Wilson teach?"
    ;   ;"What class does Professor Horswill teach?"
    ;   ;"What class does Professor Sood teach?"
    ;   ;"What class does Willie Wilson teach?"
    ;   ))
    
    ; ((:comment . "Who is teaching {course} this quarter?")
    ;  (:lf (and (courseInstructor course123 person123)
    ;            (likelyCourseTime course123 probable-quarter-of123)
    ;            (psikiSayThis (courseInstructor course123 person123)))
    ;       (isa person123 CSFaculty)
    ;       (isa course123 NUCourse))
    ;  (:settings . (:use-gen-antec-isas))
    ;  (:texts 
    ;   "Who is teaching eecs 371 this quarter?"
    ;   ;"Who is teaching eecs 359 this quarter?"
    ;   ;"Who is teaching eecs 361 this quarter?"
    ;   "Who is the professor for eecs 371 this quarter?"
    ;   ;"Who is the professor for eecs 359 this quarter?"
    ;   ;"Who is the professor for eecs 361 this quarter?"
    ;   ;"Do you know the teacher for eecs 325 this quarter?"
    ;   ;"Show me the professor for eecs 325 this quarter."
    ;   ;"Tell me the name of the professor for eecs 214."
    ;   ;"Is professor Forbus teaching eecs 214 this quarter?"
    ;   "Do you know whether Professor Forbus is teaching eecs 371 this quarter?"
    ;   ;"Do you know whether Professor Forbus is the professor for eecs 371 this quarter?"
    ;   ;"Do you know if Professor Forbus is teaching eecs 371 this quarter?"
    ;   ;"Do you know if Professor Forbus is the professor for eecs 371 this quarter?"
    ;   ))
        
    ; ((:comment . "When is {course}?")
    ;  (:lf (and (courseTimeString course123 time123)
    ;            (likelyCourseTime course123 probable-quarter-of123)
    ;            (psikiSayThis (courseTimeString course123 time123)))
    ;       (isa course123 NUCourse)
    ;       (isa time123 StringObject))
    ;  (:settings . (:use-gen-antec-isas))
    ;  (:texts 
    ;   "When is eecs 371?"
    ;   ;"When is eecs 359?"
    ;   ;"When is eecs 361?"
    ;   "What time is eecs 371?"
    ;   ;"What time is eecs 359?"
    ;   ;"What time is eecs 361?"
    ;   ;"What days are eecs 214?"
    ;   ;"Which days are eecs 214?"
    ;   ;"Do you know when eecs 205 is?"
    ;   ;"Does eecs 205 meet today?"
    ;   ))

    ; ((:comment . "What's {person}'s email?")
    ;  (:lf (emailOf person123 address123)
    ;       (isa person123 NUPerson)
    ;       (isa address123 StringObject))
    ;  (:settings :use-gen-antec-isas)
    ;  (:texts 
    ;   "What is Douglas Downey's email?"
    ;   "How can I contact Douglas Downey?"
    ;   ;"Do you know Douglas Downey's email?"
    ;   ;"Do you have Douglas Downey's email?"
    ;   ;"Can you tell me Douglas Downey's email?"
    ;   "Show me Douglas Downey's email."
    ;   "Has Douglas Downey listed his email?"
    ;   ;"Which email of Douglas Downey's do you have?"
    ;   ;"Is there an email of Douglas Downey available?"
    ;   ;"Do you have Douglas Downey's email lying around?"
    ;   ;"Do you happen to have Douglas Downey's email?"
    ;   "What is Douglas Downey's contact information?"
    ;   ;"Do you know Douglas Downey's contact information?"
    ;   ;"Do you have Douglas Downey's contact information?"
    ;   ;"Can you tell me Douglas Downey's contact information?"
    ;   ;"Show me Douglas Downey's contact information."
    ;   ;"Has Douglas Downey listed his contact information?"
    ;   ;"Which contact info of Douglas Downey's do you have?"
    ;   ;"Is there contact information for Douglas Downey available?"
    ;   ;"Do you have Douglas Downey's contact information lying around?"
    ;   ;"Do you happen to have Douglas Downey's contact information?"
    ;   ))
    
    ; ;; Questions about rooms
    ; ((:comment . "Where is {room}?")
    ;  (:lf (and (psikiSayThis "The location is displayed on the map.")
    ;            (prettyString loc123 string123)
    ;            (psikiShowMap string123 string123))
    ;       (isa loc123 NUMuddPlace)
    ;       (isa string123 StringObject))
    ;  (:settings :use-gen-antec-isas)
    ;  (:texts 
    ;   "Where is room 3011?"
    ;   "Can you tell me how to find room 3011?"
    ;   "I'm looking for room 3011?"
    ;   "Where can I find room 3011?"
    ;   "How can I get to room 3011?"
    ;   ))
    
    
    ; ((:comment . "What's {person}'s webpage address?")
    ;  (:lf 
    ;   (personalWebsite person123 webpage123)
    ;   (isa person123 NUPerson)
    ;   (isa webpage123 StringObject)
    ;   )
    ;  (:texts 
    ;   "What is Ken Forbus's webpage address?"
    ;   "What is Anindya De's website?"
    ;   "What is Jason Wilson's homepage?"
    ;   ;"Could you tell me Ken Forbus's webpage?"
    ;   ;"Do you know Ken Forbus's webpage?"
    ;   ;"Show me Ken Forbus's homepage."
    ;   ;"Where can I get the information about Professor Forbus?"
    ;   ;"Show me Ken Forbus's information."
    ;   ))
    
    ; ((:comment . "Who is in the {academic group}?")
    ;  (:lf 
    ;   (nuGroupMember group123 person123)
    ;   (isa person123 NUPerson)
    ;   (isa group123 NUGroup)
    ;   )
    ;  (:texts 
    ;   "Who is in algorithms?"
    ;   ;"Which faculty are in algorithms?"
    ;   ;"Who are the algorithms faculty?"
    ;   ;"Which professors are in algorithms?"
    ;   ;"Who works in algorithms?"
    ;   ;"Which group is anindya de in?"
    ;   ;"Which students are in algorithms?"
    ;   ;"Who does research in algorithms?"
    ;   ;"Who works in algorithms?"
    ;   ;"Who are the algorithms faculty?"
    ;   "Does anindya de do research in algorithms?"
    ;   ;"Is anindya de in algorithms?"
    ;   ;"Is anindya de a algorithms professor?"
    ;   ;"Is anindya de a algorithms faculty?"
    ;   ))

    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
