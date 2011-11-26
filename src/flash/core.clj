(ns flash.core
  (:require [clojure.pprint :as p]))

(defn have-not-learned [card]
  (<= (:consecutive-correct card) 1))

(def new-ones-limit 20)

;;; end of constants ;;;

; cards IO
(def cards-file "resources/public/cards.clj")
(defn load-cards [] (load-string (slurp cards-file)))
(defn save-cards [cards] (spit cards-file (vec cards)))
(def cards (load-cards))  ; for development only

; for all, return the number of milliseconds
; in the given number of that unit
(defn hours  [h] (* h 1000 60 60))
(defn days   [d] (* d (hours 24)))
(defn months [m] (* m (days 30)))
(defn years  [y] (* y (days 365)))

(def consecutive-correct->rest-period
  {0 0
   1 (hours 8)
   2 (days 1)
   3 (days 2)
   4 (days 4)
   5 (days 8)
   6 (days 16)
   7 (months 1)
   8 (months 2)
   9 (months 4)
   10 (months 8)
   11 (years 1)
   12 (years 2)
   13 (years 4)
   14 (years 8)
   15 (years 16)
   16 (years 32)
   17 (years 64)})

(defn now [] (.getTime (java.util.Date.)))

(defn due? [card]
  (>= (now) (:answer-at card)))

(defn implicit-category [card]
  (cond (not (:active? card)) :inactive
        (due? card) :to-ask
        :else :not-due))

(defn cards->stacks [cards]
  (group-by implicit-category cards))

(defn stacks->cards [stacks]
  (apply concat (vals stacks)))

(defn activate [cards]
  (for [card cards]
    (assoc card :active? true)))

(defn introduce-new-cards [stacks]
  (let [not-learned-count (count (filter have-not-learned (:to-ask stacks)))
        to-move (max 0 (- new-ones-limit not-learned-count))]
    (-> stacks
      (update-in [:inactive] (partial drop to-move))
      (update-in [:to-ask] concat (activate (take to-move (:inactive stacks))))
      (update-in [:to-ask] shuffle))))

(defn -main [& args] (println "hello world"))  ; this will come into play later

(defn ask [card]
  (println "Q: " (:question card))
  (println (:answer card))
  (print "A: ")
  (let [answer (read-line)]
    (condp = answer
      (str (:answer card)) :right
      ":q" :quit
      ":h" :help
      :wrong)))

(defn show-help []
  (println "The help message will eventually go here."))

(defn main [cards]
  (loop [stacks (introduce-new-cards (cards->stacks cards))]
    (do
      (p/pprint stacks)
      (if (empty? (:to-ask stacks))
        (do
          (save-cards (stacks->cards stacks))
          (println "You have learned everything!"))
        (let [card (first (:to-ask stacks))]
          (condp = (ask card)
            :help (do
                    (show-help)
                    (recur stacks))
            :quit (do
                    (save-cards (stacks->cards stacks))
                    (println "Have a great day!"))
            :right (do
                     (println "Right!")
                     (let [altered-card (-> card
                                          (assoc :answer-at
                                                 (+ (now)
                                                    (consecutive-correct->rest-period
                                                      (:consecutive-correct card))))
                                          (update-in [:consecutive-correct] inc))]
                       (if (due? altered-card)
                         (recur (assoc stacks :to-ask
                                       (shuffle (conj (rest (:to-ask stacks))
                                                      altered-card))))
                         (recur (-> stacks
                                  (assoc :to-ask (rest (:to-ask stacks)))
                                  (assoc :not-due (conj (:not-due stacks)
                                                        altered-card)))))))
            :wrong (do
                     (println "Wrong")
                     (recur (assoc stacks :to-ask
                                   (conj (rest (:to-ask stacks))
                                         (assoc card :consecutive-correct 0)))))))))))
