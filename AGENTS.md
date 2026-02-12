# Règles de travail de l'agent

À chaque modification effectuée dans ce dépôt :

1. Compiler localement la version statique du prototype Elm (au minimum `www/src/Main.elm` vers `docs/main.js`).
2. Faire un commit Git avec un message en français, descriptif et concis.
3. Pousser les changements sur le dépôt distant (`origin`) et la branche courante.

En cas d'échec de compilation, de commit ou de push, l'agent doit l'indiquer explicitement dans sa réponse.
