(ns flash.core
  (:require [clojure.pprint :as p])
  (:gen-class))

(defn learned-for-today [card]
  (> (:consecutive-correct card) 1))

(def new-ones-limit 20)

;;; end of constants ;;;

; cards IO
(def cards-file "resources/public/cards.clj")
(defn load-cards [] (load-string (slurp cards-file)))
(defn save-cards [cards] (spit cards-file (vec cards)))

; for DEVELOPMENT only
;(def cards (load-cards))
;(def stacks (cards->stacks cards))

; for all, return the number of milliseconds
; in the given number of that unit
(defn hours  [h] (* h 1000 60 60))
(defn days   [d] (* d (hours 24)))
(defn months [m] (* m (days 30)))
(defn years  [y] (* y (days 365)))

(def consecutive-correct->rest-period
  { 0 0
    1 (hours   8)
    2 (days    1)
    3 (days    2)
    4 (days    4)
    5 (days    8)
    6 (days   16)
    7 (months  1)
    8 (months  2)
    9 (months  4)
   10 (months  8)
   11 (years   1)
   12 (years   2)
   13 (years   4)
   14 (years   8)
   15 (years  16)
   16 (years  32)
   17 (years  64)})

(defn now [] (.getTime (java.util.Date.)))

(defn due? [card]
  (>= (now) (:answer-at card)))

(defn implicit-category [card]
  (cond (not (:active? card))  :inactive
        (due? card)            :to-ask
        :else                  :not-due))

(defn cards->stacks [cards]
  (group-by implicit-category cards))

(defn stacks->cards [stacks]
  (apply concat (vals stacks)))

(defn activate [cards]
  (for [card cards]
    (assoc card :active? true)))

(defn introduce-new-cards [stacks]
  (let [not-learned-count (count (remove learned-for-today (:to-ask stacks)))
        to-move (max 0 (- new-ones-limit not-learned-count))]
    (-> stacks
      (update-in [:inactive] (partial drop to-move))
      (update-in [:to-ask] concat (activate (take to-move (:inactive stacks))))
      (update-in [:to-ask] shuffle))))

(defn mark-card-right [card]
  (-> card
    (assoc :answer-at (+ (now)
                         (consecutive-correct->rest-period
                           (:consecutive-correct card))))
    (update-in [:consecutive-correct] inc)))

(defn mark-card-wrong [card]
  (assoc card :consecutive-correct 0))

(defn archive-card [stacks card]
  (-> stacks
    (update-in [:to-ask] (comp vec rest))
    (update-in [:not-due] conj card)))

(defn shuffle-card-in [stacks card]
  (-> stacks
    (assoc-in [:to-ask 0] card)
    (update-in [:to-ask] shuffle)))

(defn show-help []
  (println "The help message will eventually go here."))

(defn show-stats [stacks]
  (println)
  (println "  " (count (:to-ask stacks)) "In play")
  (println "  " (count (remove learned-for-today (:to-ask stacks)))
           "Not-yet-learned")
  (println "  " (count (:inactive stacks)) "Inactive")
  (println "  " (count (:not-due stacks)) "Not due")
  (println))

(defn ask [card]
  (println)
  (println "Q: " (:question card))
  (println)
  (println "A: ")
  (println)
  (let [answer (read-line)]
    (condp = answer
      ":q"                  :quit
      ":h"                  :help
      ":s"                  :stats
      (str (:answer card))  :right
      :wrong)))

(defn main [cards]
  (loop [stacks (introduce-new-cards (cards->stacks cards))]
    (show-stats stacks)
    (if (empty? (:to-ask stacks))
      (do
        (save-cards (stacks->cards stacks))
        (println "You have learned everything! Take a break."))
      (let [card (first (:to-ask stacks))]
        (condp = (ask card)
          :help (do
                  (show-help)
                  (recur stacks))
          :quit (do
                  (save-cards (stacks->cards stacks))
                  (println "Catch ya later."))
          :stats (do
                   (show-stats stacks)
                   (recur stacks))
          :wrong (do
                   (println "Wrong")
                   (recur (update-in stacks [:to-ask 0] mark-card-wrong)))
          :right (do
                   (println "Right!")
                   (let [altered-card (mark-card-right card)]
                     (if (learned-for-today altered-card)
                       (recur (archive-card    stacks altered-card))
                       (recur (shuffle-card-in stacks altered-card))))))))))

(defn -main [& args] (main (load-cards)))
