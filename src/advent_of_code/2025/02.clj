(ns advent-of-code.2025.02
  "Day 2: Gift Shop

  See https://adventofcode.com/2025/day/2"
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [advent-of-code.utils :refer [input]]))

(defn normalize-id-range
  "The seq of ranges covering the given range and whose bounds have the same number of digits.

  This normalization simplifies the implementation of the other functions.

  Example: [1 99] -> [[1 9] [10 99]]"
  [[start end]]
  (map (fn [d]
         [(max start (long (math/pow 10 (dec d))))
          (min end (dec (long (math/pow 10 d))))])
       (range (count (str start))
              (inc (count (str end))))))

(defn parse-id-ranges
  [line]
  (->> (str/split line #",")
       (map #(mapv parse-long (str/split % #"-")))
       (mapcat normalize-id-range)
       (doall)))

(def id-ranges
  (first (input 2025 2 parse-id-ranges)))

(defn patterns
  "The possible patterns of size n for this range."
  [[start end] n]
  (range (parse-long (subs (str start) 0 n))
         (inc (parse-long (subs (str end) 0 n)))))

(defn invalid-id
  "Repeat the pattern to generate an invalid id of the given size."
  [pattern size]
  (->> pattern
       (repeat (quot size (count (str pattern))))
       (apply str)
       (parse-long)))

(defn invalid-ids
  "The invalid IDs in the range."
  [[start end :as range] pattern-sizes]
  (let [size (count (str start))]
    (when (< 1 size)
      (->> (pattern-sizes size)
           (mapcat (fn [pattern-size]
                     (->> (patterns range pattern-size)
                          (map #(invalid-id % size)))))
           (filter #(<= start % end))
           set))))

(defn part1-pattern-sizes
  "For Part 1 the pattern has a fixed size of half the size of the ID."
  [size]
  (when (even? size)
    [(/ size 2)]))

(time
 (assert (= 20223751480
            (->> id-ranges
                 (mapcat #(invalid-ids % part1-pattern-sizes))
                 (reduce + 0)))))

(defn part2-pattern-sizes
  "For part2 the pattern can be any divisor of the size of the ID."
  [n]
  (filter #(zero? (mod n %))
          (range 1 (inc (/ n 2)))))

(time
 (assert (= 30260171216
            (->> id-ranges
                 (mapcat #(invalid-ids % part2-pattern-sizes))
                 (reduce + 0)))))
