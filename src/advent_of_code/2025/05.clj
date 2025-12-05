(ns advent-of-code.2025.05
  "Day 5: Cafeteria

  See https://adventofcode.com/2025/day/5"
  (:require [advent-of-code.utils :refer [input]]))

(defn merge-range
  [ranges [n1 n2 :as range]]
  (if (not (seq ranges))
    [range]
    (let [[[m1 m2 :as hd] & xs] ranges]
      (cond
        (< m2 (dec n1)) (cons hd (merge-range xs range))
        (< n2 (dec m1)) (cons range ranges)
        :else (reduce merge-range [[(min m1 n1) (max m2 n2)]] xs)))))

(def ingredients
  (->> (input 2025 5 #(mapv parse-long (re-seq #"\d+" %)))
       (reduce (fn [ingredients nums]
                 (case (count nums)
                   2 (update ingredients :fresh merge-range nums)
                   1 (update ingredients :available conj (first nums))
                   ingredients))
               {:fresh []
                :available []})))

(defn fresh?
  [ingredients ingredient]
  (some (fn [[min max]]
          (<= min ingredient max))
        (:fresh ingredients)))

(time
 (assert
  (= 770
     (->> (:available ingredients)
          (filter #(fresh? ingredients %))
          (count)))))

(time
 (assert
  (= 357674099117260
     (->> (:fresh ingredients)
          (map (fn [[min max]]
                 (inc (- max min))))
          (reduce + 0)))))
