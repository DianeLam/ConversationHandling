# ConversationHandling

### Conversation Handler group: 
#### We have augmented Companions to support interactions over Slack.  Our contribution is threefold: 
##### 1) We extended the current Interaction Manager to distinguish between utterances from different individuals by extracting a user-ID from the KQML achieve interpret message (provided, of course, that the KQML achieve interpret message sent to the Interaction Manager includes a user-ID as the fourth parameter of the :content specification).  This allows the Interaction Manager to receive messages from Slack users via Pythonian (as per the agent code from the Slack Front-End team).
##### 2) We expanded Companions' analogical question answering capability by developing additional utterance cases (using predicates newly ontologized by the Teaching Kiosk team).
##### 3) We adjusted the response plans to enable Companions to route its responses to utterances back to the Pythonian agent which had sent the original utterances (in this case, the Kio agent developed by the Slack Front-End team).

### Setup:
#### Within the QRG repo replace the files im-agent.lsp (path: QRG_CI_2019\qrg\companions\v1\interaction-manager\v2) and train-questions.lsp (path QRG_CI_2019\qrg\companions\v1\kiosk\questions) with the files in this repo.
#### Startup Companions from Source (through lisp/allegro environment). Using commands (qrg:require-module "companions" :companions), (qrg:compile-sys (qrg:make-qrg-path "planb") :planb), and (start-companion :scheme :mixed) respectively.
#### In the browser that popped up, click start session, and then click kiosk.
#### Once the kiosk domain is running, enter (achieve :receiver interaction-manager :content (setupKioskQAForIM)) into Companion's Command tab
#### In the Allegro/Lisp listener run the command (compile-sys “ea/v8” :ea)
#### Then compile all the files under ../companions/v1/kiosk/  using Ctrl+U in the listener
#### Run the command (ea:setup)
#### Finally run (train-test-qa-w-all) 
#### Once that finishes running, you can ask questions in the Interaction tab (or by messaging @Kio in the CS_Conversational_Interfaces_2019 workspace on Slack, if the "Kio" Pythonian agent has been instantiated in the Companions environment)


