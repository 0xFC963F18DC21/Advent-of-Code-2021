(ns aoc2021.days.day9
  (:require [aoc2021.util.input :as inp]
            [clojure.string :as strs]))

(def day9-grid
  (->> "day9"
       (inp/get-input-lines)
       (map #(strs/split % #""))
       (map #(map (fn [x] (Long/parseLong x)) %))
       (lazy-seq)))

(defn grid-get [grid [x y]]
  (nth (nth grid y nil) x Long/MAX_VALUE))

(defn neighbours [[x y]]
  (list [x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y]))

(defn low-point? [grid cl]
  (let [this    (grid-get grid cl)]
    (reduce
      (fn [b-acc loc]
        (if (< this (grid-get grid loc)) b-acc (reduced false)))
      true
      (neighbours cl))))

(defn basin?? [num]
  (not= num 9))

(defn basin [grid cl]
  (if (basin?? (grid-get grid cl))
    (loop [cl'         cl
           locations   ()
           stack       ()]
      (let [this    (grid-get grid cl')
            n-stack (reduce
                      (fn [ns loc]
                        (let [val (grid-get grid loc)]
                          (if (and (not (some #{loc} (concat locations stack)))
                                   (not= val Long/MAX_VALUE) (basin?? val)
                                   (< this val))
                            (cons loc ns)
                            ns)))
                      stack
                      (neighbours cl'))]
        (if (empty? n-stack)
          (cons cl' locations)
          (recur (first n-stack) (cons cl' locations) (next n-stack)))))
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
