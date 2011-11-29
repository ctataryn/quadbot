 (ns quadbot.client
      (:import (java.net Socket)
               (java.io PrintWriter InputStreamReader BufferedReader))
      (:use clojure.contrib.command-line)
      (:use quadbot.reactor)
      (:use quadbot.config.client))

 (declare conn-handler)


 ;;connects to the IRC server
 (defn connect [server]
      (let [socket (Socket. (:name server) (:port server))
                     in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
                     out (PrintWriter. (.getOutputStream socket))
                     conn (ref {:in in :out out})]
             (doto (Thread. #(conn-handler conn)) (.start))
             conn))

 ;;writes a message out to the IRC server
 (defn write [conn msg]
      (doto (:out @conn)
             (.println (str msg "\r"))
             (.flush)))

 ;;writes mulitple messages to the IRC server
 (defn write-multiple [conn messages]
  (let [vecparam (if (vector? messages) messages (vector messages))] 
    (doseq [msg vecparam] (write conn msg))))

 ;;takes an incomming message and parses it out into it's aggregates
 (defn parseMsg [msg]
    (zipmap [:user :channel :message] (rest (re-find #"^:(.*)!.*\sPRIVMSG\s(.*)\s:(.*)$" msg))))

;;
;; Decides on an appropriate handler, and writes the handler's output to the connection
;;
 (defn message-handler [conn msgMap]
   ;; need to make this so the handlers can pass back a sequence of lines to write instead of just one
  (write-multiple conn (cond
      ;;add more actions here, for when the bot isn't specifically mentioned in a message
      ;;
      ;;If the bot was specifically mentioned, the cmdprefix was used or the bot was /msg'd then 
      ;;find out what the user wants
      ;;i.e. quadbot: fact is factoid
      ;;Or: ~fact is factoid
      ;;OR: /msg quadbot fact
      (or (re-matches (re-pattern (str "^" (:nick user) ":.*")) (.trim (:message msgMap))) 
          (re-matches (re-pattern (str "^" cmdprefix ".*")) (.trim (:message msgMap)))
          (= (:channel msgMap) (:nick user)))
      (let [msg (reactToMsg msgMap)]
        (do
          (println (str "SENT: " msg))
           msg)))))

 (defn conn-handler [conn]
      (while 
            (nil? (:exit @conn))
            (let [msg (.readLine (:in @conn))]
                    (println (str "RECEIVED: " msg))
                    (cond 
                             (re-find #"^ERROR :Closing Link:" msg) 
                             (dosync (alter conn merge {:exit true}))
                             (re-find #":.*\sPRIVMSG\s.*" msg)
                             (message-handler conn (parseMsg msg))
                             (re-find #"^PING" msg)
                             (write conn (str "PONG "  (re-find #":.*" msg)))))))

 (defn login [conn user nickservpwd channels]
      (write-multiple conn [
                            (str "NICK " (:nick user))
                            (str "USER " (:nick user) " 0 * :" (:name user))
                            (str "PRIVMSG NickServ :IDENTIFY " (:nick user) " " nickservpwd)])
      (doseq [chan channels] (write conn (str "JOIN " chan))))

;; main entry point from command-line
 (defn -main [& args]
  (let [irc (connect freenode)]
    (with-command-line args
          "QuadBot"
          [[pwd "Password for NickServ registration" 1]
           [chans "Channels to join" 1]
           remaining]
          (login irc user pwd (clojure.string/split chans #","))
        )))
   
;;(def irc (connect freenode))
;; ;;(login irc user)
;; ;; ;;(write irc "JOIN #test-cwt")
;; ;; ;; ;;(write irc "QUIT")
;; ;; ;; ;;
