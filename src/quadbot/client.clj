 (ns quadbot.client
      (:import (java.net Socket)
                           (java.io PrintWriter InputStreamReader BufferedReader))
      (:use clojure.contrib.command-line)
      (:require [quadbot.persistence :as dao]))
   
 (def freenode {:name "irc.freenode.net" :port 6667})
 (def user {:name "#quadron bot" :nick "quadbot" :join "#test-cwt"})
 (def admins #{"ThaDon"})
 (def cmdprefix "~")

 (declare conn-handler)

 ;; checks to see if the user passed in is an Admin
 (defn isAdmin? [userName]
   (not (empty? (admins userName))))

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

 ;;takes an incomming message and parses it out into it's aggregates
 (defn parseMsg [msg]
    (zipmap [:user :channel :message] (rest (re-find #"^:(.*)!.*\sPRIVMSG\s(.*)\s:(.*)$" msg))))

 ;;creates a message targeted at a channel
 (defn createMsg [msgMap msg]
   ;; if this was a message which was /msg'd to us, respond via a private message directly to the user
   ;; who sent it
   (if (= (:channel msgMap) (:nick user))
    (str "PRIVMSG " (:user msgMap) " :" msg)
    (str "PRIVMSG " (:channel msgMap) " :" msg)))

 ;;creates a message targeted at a channel which mentions the user who asked quadbot something
 (defn createMention [msgMap msg]
  (str (createMsg msgMap (str (:user msgMap) ": " msg))))

 ;;strips the quadbot username from the front of a message
 (defn stripUser [msg]
   (let [pattrn (re-pattern (str "^" (:nick user) ":(.*)"))]
    (if (re-matches pattrn msg)
      (.trim (second (re-find pattrn msg)))
      msg)))

 ;;removes the command prefix which is used to alert quadbot to respond without mentioning quadbot
 (defn stripCmdPrefix [msg]
   (if (re-matches (re-pattern (str "^" cmdprefix ".*")) msg)
     ;;lop off the cmdprefix, assuming it's one char
     (apply str (rest msg))
     msg))

 ;;add more condition branches to this method in order to react to messages targeted at quadbot
 ;;from a user who either mentioned quadbot or used the cmdPrefix
 (defn reactToMsg [msgMap]
   (let [msg (.trim (stripCmdPrefix (stripUser (.trim (:message msgMap)))))]
     (println (str "MSG AFTER STRIPPING: " msg))
     (cond
       ;; parse a command out of msg and return something to write out to the client

       ;; check if they are trying to add a factoid
       (re-matches #"^[^\s]+\sis\s.+$" msg)
       (let [matches (re-find #"^([^\s]+)\sis\s(.+)$" msg)
             fact (second matches)
             definition (nth matches 2)]
         (do
           (dao/insert-factoid (:user msgMap) fact definition)
           (createMention msgMap (str "Ok " (:user msgMap) " I'll remember about " fact))))

       ;; retrieve a factoid
       (re-matches #"^[^\s]+$" msg)
       (let [matches (re-find #"^([^\s]+)$" msg)
             fact (second matches) ]
         (createMsg msgMap (:ANSWER (dao/retrieve-factoid fact))))
       ;;should have a way to tell if a factoid doesn't exist and then respond with the :else below

       ;; delete a factoid
       (and (isAdmin? (:user msgMap)) (re-matches #"^forget\s([^\s]+)$" msg))
       (let [matches (re-find #"^forget\s([^\s]+)$" msg)
             fact (second matches)]
         (do
           (dao/delete-factoid fact)
           (createMention msgMap (str "What's '" fact "'? ;)"))))

       ;; ask quadbot to join another channel
       (and (isAdmin? (:user msgMap)) (re-matches #"^join\s#([^\s]*)$" msg))
       (let [matches (re-find #"^join\s#([^\s]*)$" msg)
             chan (second matches)]
           (str "JOIN #" chan))

       ;; tell the bot to quit
       (and (isAdmin? (:user msgMap)) (re-matches #"^please quit$" msg))
       "QUIT"

       ;; tell the bot to leave the chan
       (and (isAdmin? (:user msgMap)) (re-matches #"^please leave$" msg))
       (str "PART " (:channel msgMap))

       :else ;; default
       (createMention msgMap "Sorry, I didn't gr0k what you said..."))))

;;
;; Decides on an appropriate handler, and writes the handler's output to the connection
;;
 (defn messageHandler [conn msgMap]
   ;; need to make this so the handlers can pass back a sequence of lines to write instead of just one
  (write conn (cond
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
                             (messageHandler conn (parseMsg msg))
                             (re-find #"^PING" msg)
                             (write conn (str "PONG "  (re-find #":.*" msg)))))))

 (defn login [conn user nickservpwd channels]
      (write conn (str "NICK " (:nick user)))
      (write conn (str "USER " (:nick user) " 0 * :" (:name user)))
      (write conn (str "PRIVMSG NickServ :IDENTIFY " (:nick user) " " nickservpwd))
      (apply (fn [chan] (write conn (str "JOIN " chan))) channels))

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
