- opam install lwt irc-client
- configurer/hacker en lisant `coucoubot.ml`, `commands.ml`
- make
- git init factoids
- ./coucoubot.native
- coucou

### Modules utiles

- coucoubot.ml : le fichier principal

- Commands : liste des commandes de coucoubot (de la forme !quelquechose)
  (regarder commands.ml)

- Core / Signal : interaction de base avec le serveur IRC. Signal permet
  d'enregistrer des callbacks lors de la réception de messages.
  (voir core.mli et signal.mli)

- Social : garde un registre "nick -> informations" et les met à jour. On peut
  étendre le type Social.contact pour mémoriser de nouvelles informations.
  Il faut alors probablement rajouter des callbacks dans social.ml

- Talk : contient des bases de phrases de différents types; facile à étendre
  (regarder talk.mli et talk.ml)

- ...
