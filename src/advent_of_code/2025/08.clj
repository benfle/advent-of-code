(ns advent-of-code.2025.08
  "Day 8: Playground

  See https://adventofcode.com/2025/day/8"
  (:require [clojure.math :refer [sqrt pow]]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [advent-of-code.utils :refer [input]]))

(def junction-boxes
  (input 2025 8 #(map parse-long (re-seq #"\d+" %))))

(defn distance
  [[x1 y1 z1] [x2 y2 z2]]
  (sqrt
   (+ (pow (- x2 x1) 2)
      (pow (- y2 y1) 2)
      (pow (- z2 z1) 2))))

(defn closest-boxes
  [boxes]
  (->> (combo/combinations boxes 2)
       (sort-by #(apply distance %))))

(defn connect
  [circuits connection]
  (reduce (fn [circuits circuit]
            (let [new-circuit (set/intersection (first circuits) circuit)]
              (if (seq new-circuit)
                (cons (set/union (first circuits) circuit)
                      (rest circuits))
                (cons (first circuits)
                      (cons circuit (rest circuits))))))
          [(set connection)]
          circuits))

(time
 (assert
  (= 330786
     (->> (closest-boxes junction-boxes)
          (take 1000)
          (reduce connect [])
          (map count)
          set
          (sort-by -)
          (take 3)
          (reduce * 1)))))

;; ~20s

(defn last-connection
  [total circuits connection]
  (let [new-circuits (connect circuits connection)]
    (if (= total (count (first new-circuits)))
      (reduced connection)
      new-circuits)))

(defn extension-cable-size
  [boxes]
  (->> (closest-boxes boxes)
       (reduce #(last-connection (count boxes) %1 %2) [])
       (map first)
       (reduce * 1)))

(time
 (assert
  (= 3276581616
     (extension-cable-size junction-boxes))))

;; ~20s
