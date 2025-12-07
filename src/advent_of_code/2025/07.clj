(ns advent-of-code.2025.07
  "Day 7: Laboratories

  See https://adventofcode.com/2025/day/7"
  (:require [advent-of-code.utils :refer [input]]))

(def diagram
  (input 2025 7))

(defn splitters
  [line]
  (set (keep-indexed #(when (= \^ %2) %1) line)))

(defn split-and-count
  [[beams splits] line]
  (let [splitter? (splitters line)]
    (reduce (fn [[beams splits] i]
              (if (splitter? i)
                [(-> beams
                     (disj i)
                     (conj (inc i) (dec i)))
                 (inc splits)]
                [beams splits]))
            [beams splits]
            beams)))

(defn split-and-count-all
  [diagram]
  (second
   (reduce split-and-count
           [#{(.indexOf (first diagram) "S")} 0]
           (rest diagram))))

(time
 (assert
  (= 1687
     (split-and-count-all diagram))))

(defn quantum-split
  [beams line]
  (let [splitter? (splitters line)]
    (reduce (fn [beams [i cnt]]
              (if (and (pos? cnt) (splitter? i))
                (-> beams
                    (assoc i 0)
                    (update (inc i) (fnil + 0) cnt)
                    (update (dec i) (fnil + 0) cnt))
                beams))
            beams
            beams)))

(defn quantum-split-all
  [diagram]
  (->> (reduce quantum-split
               {(.indexOf (first diagram) "S") 1}
               (rest diagram))
       vals
       (reduce + 0)))

(time
 (assert
  (= 390684413472684
     (quantum-split-all diagram))))
