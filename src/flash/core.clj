(ns flash.core)

(def sample-initial-board
   {:session-num 0
    :cards [{:consecutive-correct 0
             :next-session 0
             :question "a"
             :answer 1}
            {:consecutive-correct 0
             :next-session 0
             :question "b"
             :answer 2}]})

(defn -main [& args]
 (println "hello world"))
