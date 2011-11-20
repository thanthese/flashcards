(ns flash.core)

(def cards
  [{:category "test"
    :question "a"
    :answer 1
    :consecutive-correct 0
    :last-answered-time 0
    :active? true}])

(defn -main [& args]
 (println "hello world"))


(slurp "resources/public/cards.clj")
