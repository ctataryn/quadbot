 (ns quadbot.reactor
      (:use quadbot.utils.reactor)
      (:use quadbot.config.client)
      (:require [quadbot.persistence :as dao]))

 (declare react-to-factoid-set)
 (declare react-to-factoid-get)
 (declare react-to-tell)
 (declare react-to-forget)
 (declare react-to-join)
 (declare react-to-quit)
 (declare react-to-leave)

 ;;add more condition branches to this method in order to react to messages targeted at quadbot
 ;;from a user who either mentioned quadbot or used the cmdPrefix
 (defn reactToMsg [msgMap]
   (let [msg (.trim (strip-cmd-prefix (strip-user (.trim (:message msgMap)))))]
     (println (str "MSG AFTER STRIPPING: " msg))
     (cond
       ;; parse a command out of msg and return something to write out to the client

       ;; check if they are trying to add a factoid
       (re-matches #"^[^\s]+\sis\s.+$" msg)
       (let [matches (re-find #"^([^\s]+)\sis\s(.+)$" msg)
             fact (second matches)
             definition (nth matches 2)]
         (react-to-factoid-set msgMap fact definition))

       ;; retrieve a factoid
       (re-matches #"^[^\s]+$" msg)
       (let [matches (re-find #"^([^\s]+)$" msg)
             fact (second matches)]
         (react-to-factoid-get msgMap fact))

       ;; Like the factoid retrieval but actually mentions the person 
       ;; usage: ~tell someNick about fact
       ;; reponse: someNick: fact is answer
       (re-matches #"^tell\s[^\s]+\sabout\s[^\s]+$" msg)
       (let [matches (re-find #"^tell\s([^\s]+)\sabout\s([^\s]+)$" msg)
             who (second matches)
             fact (last matches)]
         (react-to-tell msgMap who fact))

       ;; delete a factoid
       (and (admin? (:user msgMap)) (re-matches #"^forget\s([^\s]+)$" msg))
       (let [matches (re-find #"^forget\s([^\s]+)$" msg)
             fact (second matches)]
         (react-to-forget msgMap fact))

       ;; ask quadbot to join another channel
       (and (admin? (:user msgMap)) (re-matches #"^join\s#([^\s]*)$" msg))
       (let [matches (re-find #"^join\s#([^\s]*)$" msg)
             chan (second matches)]
           (react-to-join msgMap chan))

       ;; tell the bot to quit
       (and (admin? (:user msgMap)) (re-matches #"^please quit$" msg))
       (react-to-quit msgMap)

       ;; tell the bot to leave the chan
       (and (admin? (:user msgMap)) (re-matches #"^please leave$" msg))
       (react-to-leave msgMap)

       :else ;; default
       (create-mention msgMap "Sorry, I didn't gr0k what you said..."))))

;;
;; reaction functions below
;;
 (defn react-to-factoid-set [msgMap fact definition]
  (do
    (dao/insert-factoid (:user msgMap) fact definition)
    (create-mention msgMap (str "Ok " (:user msgMap) " I'll remember about " fact))))

 (defn react-to-factoid-get [msgMap fact]
   (let [response (:ANSWER (dao/retrieve-factoid fact))]
     (if (empty? response)
              (create-mention msgMap (str "Sorry, I don't know about " fact))
              (create-msg msgMap response))))

 (defn react-to-tell [msgMap who fact]
   (let [response (:ANSWER (dao/retrieve-factoid fact))]
    (if (empty? response)
      (create-mention msgMap (str "Sorry, I don't know about " fact))
      (create-mention {:user who :channel (:channel msgMap)} (str fact " is " response)))))

 (defn react-to-forget [msgMap fact]
   (do
     (dao/delete-factoid fact)
     (create-mention msgMap (str "What's '" fact "'? ;)"))))

 (defn react-to-join [msgMap chan]
  [(str "JOIN #" chan)
   (create-msg {:user (:user msgMap) :channel (str "#" chan)} (str "Hi, I was asked to join this channel by " (:user msgMap)))])

 (defn react-to-quit [msgMap]
   [(create-mention msgMap "So long, farewell, auf Wiedersehen, goodbye...") 
    "QUIT"])

 (defn react-to-leave [msgMap]
   [(create-mention msgMap (str "Ok " (:user msgMap) " I'll be going now...")) 
    (str "PART " (:channel msgMap))])
