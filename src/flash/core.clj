(ns flash.core
  (:require [clojure.pprint :as p])
  (:require [clojure.string :as str])
  (:gen-class))

(def cards-file "resources/public/cards.clj")
(def considered-known-at-num-correct 3)
(def new-ones-batch-size 10)

;;; end of constants ;;;

; for DEVELOPMENT only
; (def stacks (cards->stacks (load-cards)))

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
                    (str/trim (read-line)))
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
  (println "
Help:
  :q quit
  :h this help message
  :h detailed statistics
  :i import a question/answer file
"))

(defn elapsed-time [starting-time]
  (let [total-seconds (int (/ (- (now) starting-time) 1000.0))
        minutes (int (/ total-seconds 60.0))
        seconds (int (- total-seconds (* 60.0 minutes)))]
    (str minutes "m " seconds "s")))

(defn show-stats [stacks starting-time]
  (println "------------------------------------------------------------")
  (println)
  (println "  Elapsed time: " (elapsed-time starting-time))
  (println)
  (println " " (count (:to-ask stacks)) "In play")
  (println " " (count (remove considered-known (:to-ask stacks)))
           "Not-yet-learned")
  (println " " (count (:inactive stacks)) "Inactive")
  (println " " (count (:not-due stacks)) "Not due"))

(defn show-details-stats [stacks]
  (println "------------------------------------------------------------")
  (println)
  (println "Active cards and their scores:")
  (println)
  (doseq [card (sort-by #(vec (map % [:consecutive-correct :question]))
                        (:to-ask (cards->stacks (load-cards))))]
    (println " " (:consecutive-correct card) (:question card)))
  (println ))

(defn ask [card]
  (println)
  (println "  Categry : " (:category card))
  (println "  Score   : " (:consecutive-correct card))
  (println)
  (println (:question card))
  (println)
  (let [answer (str/lower-case (read-line))]
    (condp = answer
      ":q" :quit
      ":h" :help
      ":i" :import
      ":s" :stats
      (str/lower-case (str (:answer card))) :right
      :wrong)))

(defn main [cards]
  (let [starting-time (now)]
    (show-help)
    (loop [stacks (introduce-new-cards (cards->stacks cards))]
      (show-stats stacks starting-time)
      (if (empty? (:to-ask stacks))
        (do
          (save-cards (stacks->cards stacks))
          (println "You have learned everything! Take a break."))
        (let [card (first (:to-ask stacks))]
          (condp = (ask card)
            :help (do
                    (show-help)
                    (recur stacks))
            :stats (do
                     (show-details-stats stacks)
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
                     (println)
                     (println "Correct answer: " (:answer card))
                     (println)
                     (recur (mark-top-card-wrong stacks)))
            :right (do
                     (println)
                     (println "  ( Right )")
                     (println)
                     (let [altered-card (mark-card-right card)]
                       (if (considered-known altered-card)
                         (recur (introduce-new-cards
                                  (archive-card stacks altered-card)))
                         (recur (shuffle-card-in stacks altered-card)))))))))))

(defn -main [& args] (main (load-cards)))
