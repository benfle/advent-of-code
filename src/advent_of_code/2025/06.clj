(ns advent-of-code.2025.06
  "Day 6: Trash Compactor

  See https://adventofcode.com/2025/day/6"
  (:require [clojure.string :as str]
            [advent-of-code.utils :refer [input]]))

(defn solve-problem
  [problem]
  (case (last problem)
    \* (reduce * 1 (butlast problem))
    \+ (reduce + 0 (butlast problem))))

(defn solve-worksheet
  [worksheet]
  (reduce + 0 (map solve-problem worksheet)))

(defn parse-worksheet
  "The worksheet as a seq of problems."
  [lines parse-problem]
  (let [empty? #(every? #{\ } %)]
    (->> lines
         (apply mapv vector)
         (partition-by empty?)
         (remove #(empty? (first %)))
         (map parse-problem))))

(def worksheet
  (input 2025 6 identity))

(defn parse-problem-1
  [columns]
  (->> columns
       (apply mapv vector)
       (map #(or (parse-long (str/trim (apply str %)))
                 (first %)))))

(time
 (assert
  (= 5346286649122
     (solve-worksheet (parse-worksheet worksheet parse-problem-1)))))

(defn parse-problem-2
  [columns]
  (conj (mapv #(parse-long (str/trim (apply str (butlast %))))
              columns)
        (last (first columns))))

(time
 (assert
  (= 10389131401929
     (solve-worksheet (parse-worksheet worksheet parse-problem-2)))))
