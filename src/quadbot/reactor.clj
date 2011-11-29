 (ns quadbot.reactor
      (:use quadbot.utils.reactor)
      (:use quadbot.config.client)
      (:require [quadbot.persistence :as dao]))

 ;;TODO take each reaction and make it it's own function

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
             fact (second matches)
             response (:ANSWER (dao/retrieve-factoid fact))]
         (if (empty? response)
            (createMention msgMap (str "Sorry, I don't know about " fact))
            (createMsg msgMap response)))

       ;; Like the factoid retrieval but actually mentions the person 
       ;; usage: ~tell someNick about fact
       ;; reponse: someNick: fact is answer
       (re-matches #"^tell\s[^\s]+\sabout\s[^\s]+$" msg)
       (let [matches (re-find #"^tell\s([^\s]+)\sabout\s([^\s]+)$" msg)
             who (second matches)
             fact (last matches)
             response (:ANSWER (dao/retrieve-factoid fact))]
         (if (empty? response)
           (createMention msgMap (str "Sorry, I don't know about " fact))
           (createMention {:user who :channel (:channel msgMap)} (str fact " is " response))))

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
           [(str "JOIN #" chan)
            (createMsg {:user (:user msgMap) :channel (str "#" chan)} (str "Hi, I was asked to join this channel by " (:user msgMap)))])

       ;; tell the bot to quit
       (and (isAdmin? (:user msgMap)) (re-matches #"^please quit$" msg))
       [(createMention msgMap "So long, farewell, auf Wiedersehen, goodbye...") 
        "QUIT"]

       ;; tell the bot to leave the chan
       (and (isAdmin? (:user msgMap)) (re-matches #"^please leave$" msg))
       [(createMention msgMap (str "Ok " (:user msgMap) " I'll be going now...")) 
        (str "PART " (:channel msgMap))]

       :else ;; default
       (createMention msgMap "Sorry, I didn't gr0k what you said..."))))

