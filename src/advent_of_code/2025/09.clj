(ns advent-of-code.2025.09
  "Day 9: Movie Theater

  See https://adventofcode.com/2025/day/9"
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code.utils :refer [input]]))

(def tiles
  (input 2025 9 #(map parse-long (re-seq #"\d+" %))))

;; Part 1

(defn area
  [[[x1 y1] [x2 y2]]]
  (* (inc (abs (- y2 y1)))
     (inc (abs (- x2 x1)))))

(defn max-area
  [[r1 a1] [r2 a2]]
  (if (< a1 a2)
    [r2 a2]
    [r1 a1]))

(time
 (assert
  (= 4776100539
     (->> (combo/combinations tiles 2)
          (map (fn [rect]
                 [rect (area rect)]))
          (reduce max-area)
          second))))

;; Part 2

(defn line-merge
  [l1 l2]
  (let [[s1 e1] (sort l1)
        [s2 e2] (sort l2)]
    (cond
      (= l1 l2) nil
      (= e1 s2) [[s1 e2]]
      (= s1 s2) [[e1 e2]]
      :else     [l1 l2])))

(defn merge-lines
  [lines]
  (reduce (fn [lines line]
            (if (not (seq lines))
              [line]
              (concat (butlast lines)
                      (line-merge (last lines) line))))
          []
          lines))

(defn build-rects
  [[y1 y2] lines]
  (map (fn [[x1 x2]]
         [[x1 y1] [x2 y2]])
       lines))

(defn tiles-lines
  [[[_ y1] [_ y2] :as tiles]]
  (partition 2
             (if (= y1 y2)
               (conj tiles (first tiles))
               (into (subvec tiles 1) (take 2 tiles)))))

(defn tile-rectangles
  [tiles]
  (->> (tiles-lines tiles)
       (map (fn [[[x1 y1] [x2 y2]]]
              (assert (= y1 y2))
              [y1 [(min x1 x2) (max x1 x2)]]))
       (reduce (fn [acc [y line]]
                 (update acc y (fnil conj []) line))
               {})
       (sort-by key)
       (reduce (fn [[prev-y prev-lines rects] [y new-lines]]
                 [y
                  (merge-lines (sort (concat prev-lines new-lines)))
                  (concat rects (build-rects [prev-y y] prev-lines))]))
       last))

(defn intersect?
  [[[x11 y11] [x12 y12]]
   [[x21 y21] [x22 y22]]]
  (and (<= (min x11 x12) (dec (max x21 x22)))
       (>= (dec (max x11 x12)) (min x21 x22))
       (<= (min y11 y12) (dec (max y21 y22)))
       (>= (dec (max y11 y12)) (min y21 y22))))

(defn intersection
  [[[x11 y11] [x12 y12] :as r1]
   [[x21 y21] [x22 y22] :as r2]]
  (when (intersect? r1 r2)
    [[(max (min x11 x12) (min x21 x22)) (max (min y11 y12) (min y21 y22))]
     [(min (max x11 x12) (max x21 x22)) (min (max y11 y12) (max y21 y22))]]))

(defn size
  [[[x1 y1] [x2 y2]]]
  (* (- (max x1 x2) (min x1 x2))
     (- (max y1 y2) (min y1 y2))))

(defn red-or-green?
  [tiles-rects rectangle]
  (= (size rectangle)
     (->> tiles-rects
          (map #(intersection % rectangle))
          (remove nil?)
          (map size)
          (reduce + 0))))

(defn red-or-green-rectangles
  [tiles]
  (let [tiles-rects (tile-rectangles tiles)]
    (->> (combo/combinations tiles 2)
         (map (fn [rect]
                [rect (area rect)]))
         (sort-by (comp - second))
         (map (fn [[rect area]]
                [rect area (red-or-green? tiles-rects rect)])))))

(defn max-red-or-green-rectangle
  [tiles]
  (->> (red-or-green-rectangles tiles)
       (drop-while #(not (last %)))
       first))

(time
 (max-red-or-green-rectangle tiles))

;; ~ 50 secs
