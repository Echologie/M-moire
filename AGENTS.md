# Règles de travail de l'agent

## Validation adaptée aux changements

- Compiler localement le prototype Elm uniquement lorsque les changements affectent les sources Elm, les dépendances ou la configuration Elm, ou les commandes de compilation. Utiliser `npm run build:main` (au minimum `www/src/Main.elm` vers `docs/main.js`).
- Pour les seuls changements de bibliographie, de PDF, de manifeste, de documentation ou de consignes (`AGENTS.md`), ne pas lancer de compilation Elm. Vérifier les fichiers concernés : cohérence du manifeste, noms et tailles des fichiers, liens et format, selon la modification.
- Pour les changements de données, de HTML, de CSS ou de JavaScript, effectuer les validations et la copie des ressources de publication nécessaires, sans recompiler Elm si ses sources, ses dépendances et sa configuration restent inchangées.
- Une compilation peut aussi être lancée sur demande explicite de l'utilisateur ou pour vérifier l'installation du compilateur.

## Compilation sur le téléphone Android

Elm 0.19.1 est installé globalement via le paquet communautaire `@lydell/elm`, compatible Linux ARM64. Le stockage partagé (`/sdcard` ou `/storage/emulated/0`) ne prend pas en charge le verrouillage utilisé par Elm. Lorsqu’une compilation est nécessaire, copier `www/elm.json` et `www/src/` dans un répertoire temporaire Linux, puis compiler depuis ce répertoire avec `ELM_HOME=/tmp/memoire-elm-cache`. Ne pas recopier le dossier `elm-stuff` du téléphone. Après une compilation réussie, recopier le JavaScript produit vers `docs/main.js` et `www/main.js` si les fichiers publiés doivent être actualisés. Pour un simple contrôle de l’installation, conserver la sortie dans le répertoire temporaire.

## Commit et synchronisation

Après les validations pertinentes pour les changements :

1. Faire un commit Git avec un message en français, descriptif et concis.
2. Pousser les changements sur le dépôt distant (`origin`) et la branche courante.

En cas d'échec d'une validation requise, du commit ou du push, l'indiquer explicitement dans la réponse. Une compilation non nécessaire ne constitue pas une validation manquante et ne doit pas bloquer la synchronisation.
