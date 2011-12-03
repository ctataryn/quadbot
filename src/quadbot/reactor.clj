 (ns quadbot.reactor
      (:use quadbot.utils.reactor)
      (:use quadbot.config.client)
      (:require [clojure.string :as str])
      (:require [clj-time.core :as date])
      (:require [clj-time.coerce :as dateconv])
      (:require [quadbot.persistence :as dao]))

 (declare factoids)
 (declare react-to-factoid-set)
 (declare react-to-factoid-get)
 (declare react-to-tell)
 (declare react-to-info)
 (declare react-to-forget)
 (declare react-to-join)
 (declare react-to-quit)
 (declare react-to-leave)
 (declare react-to-karma)
 (declare karma-too-soon?)

 ;;add more condition branches to this method in order to react to messages targeted at quadbot
 ;;from a user who either mentioned quadbot or used the cmdPrefix
 (defn reactToMsg [msgMap]
   (let [msg (.trim (strip-cmd-prefix (strip-user (.trim (:message msgMap)))))]
     (println (str "MSG AFTER STRIPPING: " msg))
     (cond
       ;; parse a command out of msg and return something to write out to the client

       ;; report karma. Note: in it's current state, this needs to go before factoid reactors 
       ;; or else it won't match
       (re-matches #"^karma\s([\w]+)$" msg)
       (let [matches (re-find #"^karma\s([\w]+)$" msg)
             what (second matches)]
           (react-to-karma msgMap what (fn [what] what)))

       ;; increment or decrement karma. Note: in it's current state, this needs to go before factoid reactors 
       ;; or else it won't match
       (re-matches #"^[\w]+(\+\+|--)$" msg)
       (let [matches (re-find #"^([\w]+)(\+\+|--)$" msg)
             what (second matches)
             direction   (last matches)]
         ;; same user trying to update a karma-level for something in the same direction within a minute?
         (if (karma-too-soon? (:user msgMap) what direction)
           (create-mention msgMap "Whoa there speedy, give your fingers a rest for a while...")
           (if (= direction "++")
             (react-to-karma msgMap what inc)
             (react-to-karma msgMap what dec))))

       ;; check if they are trying to add a factoid
       (re-matches #"^[^\s]+\sis\s.+$" msg)
       (let [matches (re-find #"^([^\s]+)\sis\s(.+)$" msg)
             fact (second matches)
             definition (nth matches 2)]
         (react-to-factoid-set msgMap fact definition))

       ;; retrieve help for a command
       ;; note this needs to go before the factoid retrieval
       (re-matches #"^help\s*[^\s]*$" msg)
       (let [matches (re-find #"^help\s*([^\s]*)$" msg)
             fact (second matches)]
         (if (empty? fact)
         (react-to-factoid-get msgMap "help:help" nil)
         (react-to-factoid-get msgMap (str "help:" fact) nil)))

       ;; Like the factoid retrieval but actually mentions the person 
       ;; usage: ~tell someNick about fact
       ;; reponse: someNick: fact is answer
       (re-matches #"^tell\s[^\s]+\sabout\s[^\s]+$" msg)
       (let [matches (re-find #"^tell\s([^\s]+)\sabout\s([^\s]+)$" msg)
             who (second matches)
             fact (last matches)]
         (react-to-tell msgMap who fact))
       
       ;; get the raw factoid with variables not interpolated
       (re-matches #"^info\s[^\s]+$" msg)
       (let [matches (re-find #"^info\s([^\s]+)$" msg)
             fact (second matches)]
         (react-to-info msgMap fact))

       ;; delete a factoid
       (and (admin? msgMap) (re-matches #"^forget\s([^\s]+)$" msg))
       (let [matches (re-find #"^forget\s([^\s]+)$" msg)
             fact (second matches)]
         (react-to-forget msgMap fact))

       ;; ask quadbot to join another channel
       (and (admin? msgMap) (re-matches #"^join\s#([^\s]*)$" msg))
       (let [matches (re-find #"^join\s#([^\s]*)$" msg)
             chan (second matches)]
           (react-to-join msgMap chan))
 
       ;; set help for a command
       (and (admin? msgMap) (re-matches #"^help\s[^\s]+\sis\s.+$" msg))
       (let [matches (re-find #"^help\s([^\s]+)\sis\s(.+)$" msg)
             fact (second matches)
             definition (nth matches 2)]
         (react-to-factoid-set msgMap (str "help:" fact) definition))
       
       ;; tell the bot to quit
       (and (admin? msgMap) (re-matches #"^please quit$" msg))
       (react-to-quit msgMap)

       ;; tell the bot to leave the chan
       (and (admin? msgMap) (re-matches #"^please leave$" msg))
       (react-to-leave msgMap)
       
       ;; retrieve a factoid -- generally this should be the last match as it's the greediest
       (re-matches #"[^\s]+.*" msg)
       (let [matches (re-seq #"[^\s]+" msg)
             fact (first matches)
             args (rest matches)]
         (react-to-factoid-get msgMap fact args))

       :else ;; default
       (create-mention msgMap "Sorry, I didn't gr0k what you said..."))))

;;
;; reaction functions below
;;
 (defn react-to-factoid-set [msgMap fact definition]
  (if (and (not (admin? msgMap)) (factoids fact))
    (create-mention msgMap (str "Sorry, " (:user msgMap) " that factoid has been locked"))
    (do 
      (dao/insert-factoid (:user msgMap) (str/lower-case fact) definition)
      (create-msg msgMap (str "Ok " (:user msgMap) ", I'll remember about " fact)))))

 (defn react-to-factoid-get [msgMap fact args]
   (let [response (:ANSWER (dao/retrieve-factoid (str/lower-case fact)))]
     (if (empty? response)
      (create-mention msgMap (str "Sorry, I don't know about " fact))
      (let [arg-set (create-args-set args)]
        (create-msg msgMap 
          ;; the following will interpolate all ${...} with passed to the factoid
          (let [interpolated-msg (interpolate-vars ;;interpolate ${who}
                                    (interpolate-vars response arg-set) ;; interpolate ${1}, ${2}... ${N}
                                  {"${who}" (:user msgMap)} )]
            (if (re-matches #".*\$\{\w+\}.*" interpolated-msg) ;; still vars to be interpolated?
              (str "Hmmm, I think you're missing a parameter, type ~info " fact " to find out which parameters are needed")
              interpolated-msg)))))))

 (defn react-to-info [msgMap fact]
   (let [response (dao/retrieve-factoid (str/lower-case fact))]
     (create-mention msgMap
       (if (empty? response)
        (str "Sorry, I don't know about " fact)
         (let [answer (:ANSWER response)
               created-by (:CREATED_BY response)
               created-on (:CREATED_ON response)]
         (str answer " -- created by: " created-by " on " created-on))))))

 (defn react-to-tell [msgMap who fact]
   (let [response (:ANSWER (dao/retrieve-factoid (str/lower-case fact)))]
    (if (empty? response)
      (create-mention msgMap (str "Sorry, I don't know about " fact))
      (create-mention {:user who :channel (:channel msgMap)} (interpolate-vars response {"${who}" who})))))

 (defn react-to-forget [msgMap fact]
   (do
     (dao/delete-factoid (str/lower-case fact))
     (create-mention msgMap (str "What's '" fact "'? ;)"))))

 (defn react-to-join [msgMap chan]
  [(str "JOIN #" chan)
   (create-msg {:user (:user msgMap) :channel (str "#" chan)} (str "Hi, I was asked to join this channel by " (:user msgMap)))])

 (defn react-to-quit [msgMap]
   [(create-msg msgMap "So long, farewell, auf Wiedersehen, goodbye...") 
    "QUIT"])

 (defn react-to-leave [msgMap]
   [(create-mention msgMap (str "Ok " (:user msgMap) " I'll be going now...")) 
    (str "PART " (:channel msgMap))])

 ;; checks to see if this user is updating karma on something too soon after they already did
 (defn karma-too-soon? [who what direction]
   (let [result (dao/last-karma-update what)]
     (if (empty? result)
       false
       (let [last-who (:WHO result)
             last-update (dateconv/from-long (. (:UPDATED_ON result) getTime)) ;;get it from a TimeStamp to a DateTime, result is in UTC
             last-direction (:LAST_DIRECTION result)]
         (if (and  ;; were they the last person to update this karma-level? If so, same direction and less than a minute ago?
               (date/before? (date/now) (date/plus last-update (date/minutes 1)))
               (= last-who who)
               (= last-direction direction))
           true
           false)))))

 (defn react-to-karma [msgMap what f]
   (if (= what (:user msgMap))
     (create-mention msgMap "fap...fap...fap...")
     (create-msg msgMap (str what " has a karma level of " (dao/do-with-karma (:user msgMap) what f) ", " (:user msgMap)))))

;;
;; Use this initialization function before starting quadbot up for the first time
;;
 (def factoids {
           "help:help" "type: \"~help [command]\" for details on how to use each command.  Type \"~help commands\" for a listing of commands available"
           "help:commands" "get-factoid, set-factoid, tell-factoid, info, quit, leave, join, help"
           "help:get-factoid" "~fact - retrieves a factoid if fact exists"
           "help:set-factoid" "~fact is answer - sets a factoid to a specific answer, overwritting any previous fact"
           "help:tell-factoid" "~tell user about fact - same as get-factoid except it directs the factoid at a specific user"
           "help:info" "~info fact - retrieves the raw factoid without attempting argument interpolation"
           "help:karma" "type: ~karma [something] to report the karma-level for [something] --- ~something++ or ~something-- to increment or decrement the karma-level for  [something] respectively"
           "help:quit" "~please quit - asks quadbot to quit IRC if the user issuing the command is an Admin"
           "help:leave" "~please leave - asks quadbot to leave the channel the command was issued in if the user issuing the command is an Admin"
           "help:join" "~join #channel - asks quadbot to join a channel if the user issuing the command is an Admin"
           "ping" "pong"
           "features" "Thanks for the suggestion, feel free to contribute that feature here: https://github.com/ctataryn"
           "tl;dr" "Too long;didn't read"
           "tias" "Try it and see. You learn much more by experimentation than by asking without having even tried."
           "hi" "Hi, I'm quadbot an IRC bot written in Clojure.  Feel free to contribute to me: https://github.com/ctataryn/quadbot"
           "quadbot" "Hi, I'm quadbot an IRC bot written in Clojure.  Feel free to contribute to me: https://github.com/ctataryn/quadbot"})

 (defn init-quadbot []
   (do
     (dao/invoke-with-connection dao/drop-tables)
     (dao/invoke-with-connection dao/create-tables)
     (doseq [entry factoids]
       (react-to-factoid-set {:user "quadbot" :auth "+"} (key entry) (val entry)))))
