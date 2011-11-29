 (ns quadbot.utils.reactor
   (:use quadbot.config.client))

 ;; checks to see if the user passed in is an Admin
 (defn admin? [userName]
   (not (empty? (admins userName))))

 ;;strips the quadbot username from the front of a message
 (defn strip-user [msg]
   (let [pattrn (re-pattern (str "^" (:nick user) ":(.*)"))]
    (if (re-matches pattrn msg)
      (.trim (second (re-find pattrn msg)))
      msg)))

 ;;removes the command prefix which is used to alert quadbot to respond without mentioning quadbot
 (defn strip-cmd-prefix [msg]
   (if (re-matches (re-pattern (str "^" cmdprefix ".*")) msg)
     ;;lop off the cmdprefix, assuming it's one char
     (apply str (rest msg))
     msg))

 ;;creates a message targeted at a channel
 (defn create-msg [msgMap msg]
   ;; if this was a message which was /msg'd to us, respond via a private message directly to the user
   ;; who sent it
   (if (= (:channel msgMap) (:nick user))
    (str "PRIVMSG " (:user msgMap) " :" msg)
    (str "PRIVMSG " (:channel msgMap) " :" msg)))

 ;;creates a message targeted at a channel which mentions the user who asked quadbot something
 (defn create-mention [msgMap msg]
  (str (create-msg msgMap (str (:user msgMap) ": " msg))))

