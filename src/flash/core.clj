(ns flash.core
  (:require [clojure.pprint :as p]))

(def cards-file "resources/public/cards.clj")
(defn load-cards [] (load-string (slurp cards-file)))
(defn save-cards [cards] (spit cards-file (p/pprint cards)))
(defn show-cards [cards] (p/pprint cards))

; temporary
(def cards (load-cards))

(defn -main [& args]
 (println "hello world"))
