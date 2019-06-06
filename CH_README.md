# ConversationHandling

### Conversation Handler group: 
#### We extended the current IM to handle utterances with user-IDs to serve as a connection between the slackbot (created by Slack Front-End team) and Companions. We also trained multiple questions of the 3 different types using newly predicates that were ontologized by the Teaching Kiosk Team. The end result is an end-to-end connection between the slackbot <-> Companions.

### Setup:
#### Within the QRG repo replace the files im-agent.lsp (path: QRG_CI_2019\qrg\companions\v1\interaction-manager\v2) and train-questions.lsp (path QRG_CI_2019\qrg\companions\v1\kiosk\questions) with the files in this repo.
#### Startup Companions from Source (through lisp/allegro environment). Using commands (qrg:require-module "companions" :companions), (qrg:compile-sys (qrg:make-qrg-path "planb") :planb), and (start-companion :scheme :mixed) respectively.
#### In the browser that popped up, click start session, and then click kiosk.
#### Once the kiosk domain is running, enter (achieve :receiver interaction-manager :content (setupKioskQAForIM)) into Companion's Command tab
#### In the Allegro/Lisp listener run the command (compile-sys “ea/v8” :ea)
#### Then compile all the files under ../companions/v1/kiosk/  using Ctrl+U in the listener
#### Run the command (ea:setup)
#### Finally run (train-test-qa-w-all) 

#### Add/replace files in the QRG directory.
