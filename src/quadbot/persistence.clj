(ns quadbot.persistence
      (:use [korma.db])
      (:use [korma.core])
      (:use [clojure.test])
      (:require [clojure.java.jdbc :as sql]))

(def dbspec  {:classname   "org.h2.Driver" 
             :subprotocol "h2" 
             :subname "~/.quadbot/db/quadbot"
             :user     "sa"
             :password ""})

(defdb mydb dbspec)
(defentity factoid)
(defentity karma)

(defn create-tables
  "Create a factoid table"
  []
  (do 
    (sql/create-table
      "factoid"
      [:id          "IDENTITY" "NOT NULL" "PRIMARY KEY"]
      [:created_by  "VARCHAR(255)"]
      [:fact        "VARCHAR(255)"]
      [:answer      "VARCHAR"]
      [:created_on  "TIMESTAMP" "NOT NULL" "DEFAULT CURRENT_TIMESTAMP"])
    (sql/do-commands "CREATE INDEX FACTIDX ON factoid(fact)")
    (sql/create-table
      "karma"
      [:id          "IDENTITY" "NOT NULL" "PRIMARY KEY"]
      [:what        "VARCHAR(255)"]
      [:value       "INT"])
    (sql/do-commands "CREATE INDEX KARMAIDX ON karma(what)")))
    
(defn drop-tables
  "Drop factoid table"
  []
  (sql/drop-table
    "factoid"))

(defn get-identity
  "Gets the last identity generated by the db"
  []
  (sql/with-query-results rs ["select IDENTITY()"]))

;; Invokes a function with a connection inside a transaction
;; (invoke-with-connection drop-tables)
;; (invoke-with-connection create-tables)
(defn invoke-with-connection [f]
  (sql/with-connection
     dbspec
     (sql/transaction
       (f))))
  
;returns the id of the factoid which was created
(defn insert-factoid [nick fact answer] 
  "(insert factoid) returns a map of {:SCOPE_IDENTITY() <number>} we want the number"
    (invoke-with-connection #(second (first (insert factoid
      (values {:created_by nick :fact fact :answer answer}))
      ))))


(defn delete-factoid [fact]
  (invoke-with-connection #(delete factoid (where {:fact [= fact]}))))

(defn retrieve-factoid [fact]
  (first (invoke-with-connection #(select factoid (where {:fact [= fact]}) (order :created_on :DESC)))))


 (defn do-with-karma [what f]
  (invoke-with-connection 
      #(let [result (select karma (fields :value) (where {:what [= what]}))
             value (if (empty? result) 0 (:VALUE (first result)))
             newval (f value)]
         (if (empty? result)
            (insert karma (values {:what what :value newval}))
            (update karma
                    (set-fields {:value newval})
                    (where {:what [= what]}))) 
         newval)))

 (defn increment-karma [what]
  (do-with-karma what inc))

 (defn decrement-karma [what]
  (do-with-karma what dec))
