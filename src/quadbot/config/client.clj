 (ns quadbot.config.client
      (:require [quadbot.persistence :as dao]))

 (def freenode {:name "irc.freenode.net" :port 6667})
 (def user {:name "#quadron bot" :nick "quadbot"})
 (def admins #{"ThaDon" "quadbot"})
 (def cmdprefix "~")
 (def version {:number "1.0" :client_name "QuadBot"})

