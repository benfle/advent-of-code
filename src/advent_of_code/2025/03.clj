(ns advent-of-code.2025.03
  "Day 3: Lobby

  See https://adventofcode.com/2025/day/3"
  (:require [advent-of-code.utils :refer [input]]))

(def banks
  (input 2025 3 str))

(defn max-s
  [s1 s2]
  (if (or (nil? s1)
          (< (int (first s1)) (int (first s2))))
    s2
    s1))

(defn max-joltage
  [bank n]
  (loop [n n
         digits (seq bank)
         max-digits []]
    (if (zero? n)
      (parse-long (apply str max-digits))
      (let [[next & xs] (->> (iterate rest digits)
                             (take-while #(<= n (count %)))
                             (reduce max-s nil))]
        (recur (dec n)
               xs
               (conj max-digits next))))))

(time
 (assert 16887
         (->> banks
              (map #(max-joltage % 2))
              (reduce + 0))))

(time
 (assert 167302518850275
         (->> banks
              (map #(max-joltage % 12))
              (reduce + 0))))
