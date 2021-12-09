(ns aoc2021.days.day9
  (:require [aoc2021.util.input :as inp]
            [clojure.set :as sets]
            [clojure.string :as strs]))

(def day9-grid
  (->> "day9"
       (inp/get-input-lines)
       (map #(strs/split % #""))
       (map #(map (fn [x] (Long/parseLong x)) %))
       (lazy-seq)))

(defn grid-get [grid [x y]]
  (nth (nth grid y nil) x Long/MAX_VALUE))

(defn low-point? [grid [x y :as cl]]
  (let [this    (grid-get grid cl)
        checked (list [x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y])]
    (reduce
      (fn [b-acc loc]
        (if (< this (grid-get grid loc)) b-acc (reduced false)))
      true
      checked)))

(defn basin?? [num]
  (not= num 9))

; Based on a modified flood-fill algorithm.
(defn basin [grid [x y :as cl]]
  (if (basin?? (grid-get grid cl))
    (loop [[cx cy :as cl] (list x y)
           locations   #{}
           queue       ()]
      (let [this    (grid-get grid cl)
            checked (list [cx (dec cy)] [(inc cx) cy] [cx (inc cy)] [(dec cx) cy])
            n-queue (reduce
                      (fn [nq loc]
                        (let [val (grid-get grid loc)]
                          (if (and (not= val Long/MAX_VALUE) (basin?? val) (< this val))
                            (cons loc nq)
                            nq)))
                      queue
                      checked)]
        (if (empty? n-queue)
          (sets/union #{cl} locations)
          (recur (first n-queue) (sets/union #{cl} locations) (next n-queue)))))
    nil))

(defn risk-level [grid cl]
  (inc (grid-get grid cl)))

(defn find-low-points [grid]
  (let [locations  (lazy-seq (for [x (range 0 (count (first grid)))
                                   y (range 0 (count grid))]
                               (list x y)))
        low-points (filter #(low-point? grid %) locations)]
    low-points))

(defn part1 []
  (let [low-points (find-low-points day9-grid)]
    (reduce (fn [acc loc] (+ acc (risk-level day9-grid loc))) 0 low-points)))

(defn part2 []
  (let [low-points  (find-low-points day9-grid)
        basins      (map #(basin day9-grid %) low-points)
        sizes       (map count basins)
        [a b c & _] (sort #(compare %2 %1) sizes)]
    (* a b c)))


