 (ns quadbot.client
      (:import (java.net Socket)
                           (java.io PrintWriter InputStreamReader BufferedReader))
      (:use clojure.contrib.command-line))
   
 (def freenode {:name "irc.freenode.net" :port 6667})
 (def user {:name "#quadron bot" :nick "quadbot" :join "#test-cwt"})
 (def admins #{"ThaDon"})

 (declare conn-handler)

 (defn isAdmin? [userName]
   (not (empty? (admins userName))))

 (defn connect [server]
      (let [socket (Socket. (:name server) (:port server))
                     in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
                     out (PrintWriter. (.getOutputStream socket))
                     conn (ref {:in in :out out})]
             (doto (Thread. #(conn-handler conn)) (.start))
             conn))

 (defn write [conn msg]
      (doto (:out @conn)
             (.println (str msg "\r"))
             (.flush)))

 (defn parseMsg [msg]
    (zipmap [:user :channel :message] (rest (re-find #"^:(.*)!.*\sPRIVMSG\s(.*)\s:(.*)$" msg))))

 (defn createMention [msgMap msg]
  (str "PRIVMSG " (:channel msgMap) " :" (:user msgMap) ": " msg))

 (defn stripUser [msg]
   (.trim (second (re-find (re-pattern (str "^" (:nick user) ":(.*)")) msg))))

 (defn reactToMsg [msgMap]
   (let [msg (.trim (stripUser (.trim (:message msgMap))))]
     (cond
       ;; parse a command out of :message and return something to write out to the client
       (and (isAdmin? (:user msgMap)) (re-matches #"^please quit$" msg))
       "QUIT"
       true ;; default
       (createMention msgMap "Sorry, I didn't gr0k what you said..."))))

;;
;; Decides on an appropriate handler, and writes the handler's output to the connection
;;
 (defn messageHandler [conn msgMap]
   ;; need to make this so the handlers can pass back a sequence of lines to write instead of just one
  (write conn (cond
      ;;add more actions here, for when the bot isn't specifically mentioned in a message
      ;;
      ;;If the bot was specifically mentioned, find out what the user wants
      ;;i.e. quadbot: how old are you?
      (re-matches (re-pattern (str "^" (:nick user) ":.*")) (.trim (:message msgMap))) 
      (reactToMsg msgMap))))

 (defn conn-handler [conn]
      (while 
            (nil? (:exit @conn))
            (let [msg (.readLine (:in @conn))]
                    (println msg)
                    (cond 
                             (re-find #"^ERROR :Closing Link:" msg) 
                             (dosync (alter conn merge {:exit true}))
                             (re-find #":.*\sPRIVMSG\s.*" msg)
                             (messageHandler conn (parseMsg msg))
                             (re-find #"^PING" msg)
                             (write conn (str "PONG "  (re-find #":.*" msg)))))))

 (defn login [conn user nickservpwd]
      (write conn (str "NICK " (:nick user)))
      (write conn (str "USER " (:nick user) " 0 * :" (:name user)))
      (write conn (str "PRIVMSG NickServ :IDENTIFY " (:nick user) " " nickservpwd))
      (write conn (str "JOIN " (:join user))))

;; main entry point from command-line
 (defn -main [& args]
  (let [irc (connect freenode)]
    (with-command-line args
          "QuadBot"
          [[pwd "Password for NickServ registration" 1]
           remaining]
          (login irc user pwd)
        )))
   
;;(def irc (connect freenode))
;; ;;(login irc user)
;; ;; ;;(write irc "JOIN #test-cwt")
;; ;; ;; ;;(write irc "QUIT")
;; ;; ;; ;;
