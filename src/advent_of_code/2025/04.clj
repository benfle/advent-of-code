(ns advent-of-code.2025.04
  "Day 4: Printing Department

  See https://adventofcode.com/2025/day/4"
  (:require [advent-of-code.utils :refer [input]]))

(def locations
  (input 2025 4 #(into [] %)))

(defn positions
  []
  (for [row (range (count locations))
        col (range (count (first locations)))]
    [row col]))

(defn neighbors
  [[r c]]
  (->> (for [row [-1 0 1]
             col [-1 0 1]
             :when (not (and (zero? row) (zero? col)))]
         [row col])
       (mapv (fn [[dr dc]] [(+ r dr) (+ c dc)]))))

(defn roll-at?
  [locations pos]
  (= \@ (get-in locations pos \.)))

(defn accessible?
  [locations pos]
  (and (roll-at? locations pos)
       (->> (neighbors pos)
            (filter #(roll-at? locations %))
            (count)
            (> 4))))

(time
 (assert
  (= 1569
     (count (filter #(accessible? locations %)
                    (positions))))))

(defn remove-at
  [locations pos]
  (assoc-in locations pos \x))

(defn remove-rolls
  [locations]
  (reduce #(if (accessible? locations %2)
             (remove-at %1 %2)
             %1)
          locations
          (positions)))

(defn fixed-point
  [f x]
  (->> (iterate f x)
       (reduce #(if (= %1 %2) (reduced %1) %2))))

(defn total-removed
  [locations]
  (->> (positions)
       (filter #(= \x (get-in locations %)))
       (count)))

(time
 (assert
  (= 9280
     (->> locations
          (fixed-point remove-rolls)
          (total-removed)))))
