(ns advent-of-code.2025.01
  "Day 1: Secret Entrance

  See https://adventofcode.com/2025/day/1"
  (:require [clojure.string :as str] :reload-all
            [clojure.edn :as edn]
            [advent-of-code.utils :refer [input]]))

(defn parse-rotation
  [s]
  (let [op (case (first s)
             \L -
             \R +)]
    (op (parse-long (subs s 1)))))

(def rotations (input 2025 1 parse-rotation))

(defn part1-rf
  [[pos cnt] rot]
  (let [new-pos (mod (+ pos rot) 100)]
    [new-pos (if (zero? new-pos) (inc cnt) cnt)]))

(time
 (assert (= 1007
            (->> rotations
                 (reduce part1-rf [50 0])
                 (second)))))

(defn part2-rf
  [[pos cnt] rot]
  (let [new-pos (+ pos rot)
        cycles (if (pos? new-pos)
                 (quot new-pos 100)
                 (+ (quot (- new-pos) 100)
                    (if (zero? pos) 0 1)))]
    [(mod new-pos 100) (+ cnt cycles)]))

(time
 (assert (= 5820
            (->> rotations
                 (reduce part2-rf [50 0])
                 (second)))))
