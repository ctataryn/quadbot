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

(defn createMsg [msgMap msg]
  (str "PRIVMSG " (:channel msgMap) " :" msg))

 (defn createMention [msgMap msg]
  (str (createMsg msgMap (str (:user msgMap) ": " msg))))

 (defn stripUser [msg]
   (let [pattrn (re-pattern (str "^" (:nick user) ":(.*)"))]
    (if (re-matches pattrn msg)
      (.trim (second (re-find pattrn msg)))
      msg)))

 (defn stripCmdPrefix [msg]
   (if (re-matches (re-pattern (str "^" cmdprefix ".*")) msg)
     ;;lop off the cmdprefix, assuming it's one char
     (apply str (rest msg))
     msg))

 (defn reactToMsg [msgMap]
   (let [msg (.trim (stripCmdPrefix (stripUser (.trim (:message msgMap)))))]
     (cond
       ;; parse a command out of msg and return something to write out to the client

       ;; check if they are trying to add a factoid
       (re-matches #"^\w+\sis\s.+$" msg)
       (let [matches (re-find #"^(\w+)\sis\s(.+)$" msg)
             fact (second matches)
             definition (nth matches 2)]
         (do
           (dao/insert-factoid (:user msgMap) fact definition)
           (createMention msgMap (str "Ok " (:user msgMap) " I'll remember about " fact))))

       ;; retrieve a factoid
       (re-matches #"^\w+$" msg)
       (let [matches (re-find #"^(\w+)$" msg)
             fact (second matches) ]
         (createMsg msgMap (:ANSWER (dao/retrieve-factoid fact))))

       ;; delete a factoid
       (and (isAdmin? (:user msgMap)) (re-matches #"^forget\s(\w+)$" msg))
       (let [matches (re-find #"^forget\s(\w+)$" msg)
             fact (second matches)]
         (do
           (dao/delete-factoid fact)
           (createMention msgMap (str "What's '" fact "'? ;)"))))
       
       ;; tell the bot to quit
       (and (isAdmin? (:user msgMap)) (re-matches #"^please quit$" msg))
       "QUIT"
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
      ;;If the bot was specifically mentioned, or the cmdprefix was used find out what the user wants
      ;;i.e. quadbot: fact is factoid
      ;;Or: ~fact is factoid
      (or (re-matches (re-pattern (str "^" (:nick user) ":.*")) (.trim (:message msgMap))) 
          (re-matches (re-pattern (str "^" cmdprefix ".*")) (.trim (:message msgMap)))) 
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

 (defn login [conn user nickservpwd channels]
      (write conn (str "NICK " (:nick user)))
      (write conn (str "USER " (:nick user) " 0 * :" (:name user)))
      (write conn (str "PRIVMSG NickServ :IDENTIFY " (:nick user) " " nickservpwd))
      (map (fn [chan] (write conn (str "JOIN " chan))) channels))

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
