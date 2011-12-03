 (ns quadbot.utils.reactor
   (:use quadbot.config.client))

 ;;takes an incomming message and parses it out into it's aggregates
 (defn parseMsg [msg]
    (zipmap [:user :channel :auth :message] (rest (re-find #"^:(.*)!.*\sPRIVMSG\s(.*)\s:(\+|-)(.*)$" msg))))

 ;; checks to see if the user who sent the message has authed to NickServ
 (defn authed? [msgMap]
   (= (:auth msgMap) "+"))

 ;; checks to see if the user passed who sent the message is an Admin
 (defn admin? [msgMap]
   (and (authed? msgMap) (not (empty? (admins (:user msgMap))))))

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

 (defn get-vars-used [msg]
   ;; "set" to get a unique list, use "flatten" and "seq" such that "hash-map" works
   ;; for: "Hello ${1} how are you? Would you like to try some ${2}?" wind up with: {"1" "${1}", "2" "${2}", ...}
   (apply hash-map (flatten (seq (set (map reverse (re-seq #"\$\{(\w+)\}" msg)))))))

 (defn create-args-set [args]
   (into {} (map-indexed (fn [idx itm] [(str "${" (+ 1 idx) "}") itm]) args)))

 (defn interpolate-vars
    [string mappings] 
    (let [substitute-mapping 
          (fn [string mapping] 
            (let [pattern (java.util.regex.Pattern/quote (name (key mapping))) 
                  pattern (re-pattern pattern) 
                  matcher (re-matcher pattern string) 
                  replacement (java.util.regex.Matcher/quoteReplacement (str (val mapping)))] 
              (.replaceAll matcher replacement)))] 
      (reduce substitute-mapping string (seq mappings))))

