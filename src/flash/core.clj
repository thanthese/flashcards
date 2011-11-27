(ns flash.core
  (:require [clojure.pprint :as p])
  (:require [clojure.string :as str])
  (:gen-class))

(def cards-file "resources/public/cards.clj")
(def considered-known-at-num-correct 3)
(def new-ones-batch-size 10)

;;; end of constants ;;;

(defn load-cards [] (load-string (slurp cards-file)))
(defn save-cards [cards] (spit cards-file (vec cards)))

(defn now [] (.getTime (java.util.Date.)))

(defn due? [card]
  (>= (now) (:answer-at card)))

(defn implicit-category [card]
  (cond (not (:active? card))  :inactive
        (due? card)            :to-ask
        :else                  :not-due))

(defn cards->stacks [cards]
  (let [groups (group-by implicit-category cards)]
    {:inactive (get groups :inactive ())
     :to-ask   (get groups :to-ask   ())
     :not-due  (get groups :not-due  ())}))

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
  (let [file-name (do
                    (println "The import file should have the format:
                             Q1
                             A1
                             Q2
                             A2
                             ...")
                    (println)
                    (println "Enter full file path: ")
                    (read-line))
        category (do
                   (println "Enter category of new cards: ")
                   (read-line))
        new-cards (for [[q a] (partition 2 (str/split
                                             (slurp file-name)
                                             #"\n"))]
                    {:category category
                     :question q
                     :answer a
                     :consecutive-correct 0
                     :answer-at 0
                     :active? false})]
    (-> stacks
      (update-in [:to-ask] concat new-cards)
      (update-in [:to-ask] vec))))

(defn show-help []
  (println "\nHelp:\n  :q quit\n  :h this help message\n  :i import a question/answer file"))

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
  (show-help)
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
          :import (recur (import-new stacks))
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
