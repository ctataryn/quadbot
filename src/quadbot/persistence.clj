(ns quadbot.persistence
      (:use [korma.db])
      (:use [korma.core])
      (:use [clojure.test]))

(def dbspec  {:classname   "org.h2.Driver" 
             :subprotocol "h2" 
             :subname "~/.quadbot/db/quadbot"
             :user     "sa"
             :password ""})

(defdb mydb dbspec)
(defentity users)

(defn create-tables
  "Create a users table"
  []
  (clojure.java.jdbc/create-table
    "users"
    [:first "varchar(255)"]
    [:last  "varchar(255)"]
    ))

(defn drop-tables
  "Drop users table"
  []
  (clojure.java.jdbc/drop-table
    "users"))

;; (invoke-with-connection drop-tables)
;; (invoke-with-connection create-tables)
(defn invoke-with-connection [f]
  (clojure.java.jdbc/with-connection
     dbspec
     (clojure.java.jdbc/transaction
       (f))))
  
;(deftest insert-select
;         (is (= (do 
;            (-> (insert "users")
;                  (values {:first "chris" :last "granger"}))
;            (select users) ) "bleh")))
(defn insert-users [] 
  (insert users
    (values {:first "john" :last "doe"})))


