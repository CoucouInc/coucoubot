# Coucoubot

le bot qui fait «coucou!»
Coucou coucou

## Build

- `opam install lwt irc-client yojson containers sequence uri cohttp lambdasoup atdgen`
- configurer/hacker en lisant `src/coucoubot.ml`, `src/commands.ml`
- `make`
- `./coucoubot.native`
- enjoy the coucou!

### Modules utiles

- `src/coucoubot.ml` : le fichier principal

- `Commands` : liste des commandes de coucoubot (de la forme !quelquechose)
  (regarder commands.ml)

- `Core` / `Signal` : interaction de base avec le serveur IRC. Signal permet
  d'enregistrer des callbacks lors de la réception de messages.
  (voir core.mli et signal.mli)

- `Social` : garde un registre "nick -> informations" et les met à jour. On peut
  étendre le type Social.contact pour mémoriser de nouvelles informations.
  Il faut alors probablement rajouter des callbacks dans social.ml

- `Factoids`: les factoides qu'on aime tous.

- `Talk` : contient des bases de phrases de différents types; facile à étendre
  (regarder talk.mli et talk.ml)

- ...
