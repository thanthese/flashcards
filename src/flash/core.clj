(ns flash.core
  (:require [clojure.pprint :as p])
  (:gen-class))

(def cards-file "resources/public/cards.clj")
(def considered-known-at-num-correct 3)
(def new-ones-batch-size 10)

;;; end of constants ;;;

(defn load-cards [] (load-string (slurp cards-file)))
(defn save-cards [cards] (spit cards-file (vec cards)))

(defn implicit-category [card]
  (cond (not (:active? card))  :inactive
        (due? card)            :to-ask
        :else                  :not-due))

(defn cards->stacks [cards]
  (group-by implicit-category cards))

(defn stacks->cards [stacks]
  (apply concat (vals stacks)))

(defn considered-known [card]
  (>= (:consecutive-correct card)
      considered-known-at-num-correct))

(def hours-8 (* 1000 60 60 8))  ; in milliseconds

(defn consecutive-correct->rest-period [n]
  (let [m (inc (- n considered-known-at-num-correct))]
    (if (< m 0)
      0
      (* hours-8 (int (Math/pow 2 m))))))

(defn now [] (.getTime (java.util.Date.)))

(defn due? [card]
  (>= (now) (:answer-at card)))

(defn introduce-new-cards [stacks]
  (let [not-learned-count (count (remove considered-known (:to-ask stacks)))]
    (if (>= not-learned-count new-ones-batch-size)
      stacks
      (let [categories-list (distinct (map :category (:inactive stacks)))
            cat-pick (first (shuffle categories-list))
            card-picks (take new-ones-batch-size
                             (filter (fn [card] (= cat-pick (:category card)))
                                     (:inactive stacks)))]
        (update-in
          (reduce (fn [s c]
                    (-> s
                      (update-in [:inactive] (partial remove (partial = c)))
                      (update-in [:to-ask] conj (assoc c :active? true))))
                  stacks
                  card-picks)
          [:to-ask]
          shuffle)))))

(defn mark-card-right [card]
  (-> card
    (assoc :answer-at (+ (now)
                         (consecutive-correct->rest-period
                           (:consecutive-correct card))))
    (update-in [:consecutive-correct] inc)))

(defn mark-top-card-wrong [stack]
  (assoc-in stack [:to-ask 0 :consecutive-correct] 0))

(defn archive-card [stacks card]
  (-> stacks
    (update-in [:to-ask] (comp vec rest))
    (update-in [:not-due] conj card)))

(defn shuffle-card-in [stacks card]
  (-> stacks
    (assoc-in [:to-ask 0] card)
    (update-in [:to-ask] shuffle)))

(defn import-new [stacks]
  (println "not yet implemented"))

(defn show-help []
  (println "The help message will eventually go here."))

(defn show-stats [stacks]
  (println)
  (println "  " (count (:to-ask stacks)) "In play")
  (println "  " (count (remove considered-known (:to-ask stacks)))
           "Not-yet-learned")
  (println "  " (count (:inactive stacks)) "Inactive")
  (println "  " (count (:not-due stacks)) "Not due")
  (println))

(defn ask [card]
  (println)
  (println "Category: " (:category card))
  (println)
  (println (:question card))
  (println)
  (let [answer (read-line)]
    (condp = answer
      ":q"                  :quit
      ":h"                  :help
      ":i"                  :import
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
          :import (import-new stacks)
          :wrong (do
                   (println)
                   (println "XXXXXXXXXXXXX")
                   (println "XX  Wrong  XX")
                   (println "XXXXXXXXXXXXX")
                   (recur (mark-top-card-wrong stacks)))
          :right (do
                   (println)
                   (println "Right!")
                   (println)
                   (let [altered-card (mark-card-right card)]
                     (if (considered-known altered-card)
                       (recur (introduce-new-cards
                                (archive-card stacks altered-card)))
                       (recur (shuffle-card-in stacks altered-card))))))))))

(defn -main [& args] (main (load-cards)))
