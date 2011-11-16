(defproject quadbot "1.0.0-SNAPSHOT"
  :description "Quadbot: An IRC bot written in Clojure"
  :main quadbot.client
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [com.h2database/h2 "1.3.161"]
                 [korma "0.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]])
