(ns aoc2021.days.day15
  (:require [aoc2021.util.input :as inp]
            [clojure.string :as strs]))

(def day15-grid
  (->> "day15"
       (inp/get-input-lines)
       (map #(apply vector %))
       (mapv #(mapv (fn [n] (Long/parseLong (str n))) %))
       (apply vector)))

(def day15-small-grid
  (->> "day15-small"
       (inp/get-input-lines)
       (map #(apply vector %))
       (mapv #(mapv (fn [n] (Long/parseLong (str n))) %))
       (apply vector)))

(defn diagonal-movement [y]
  (map list (range 0 y) (reverse (range 0 y))))

(defn diagonal-iteration [sy]
  (filter (fn [[x y]] (and (< x sy) (< y sy))) (apply concat (map diagonal-movement (range 0 (inc (* 2 sy)))))))

(defn safe-2d-aget [arr y x sy sx not-found]
  (if (or (< x 0) (< y 0) (>= x sx) (>= y sy))
    not-found
    (aget arr y x)))

(defn risk-sum [grid]
  (let [sy     (count grid)
        sx     (count (first grid))
        sumarr (to-array-2d grid)]
    (aset sumarr 0 0 0)
    (doseq [[x y] (diagonal-iteration sy)]
      (if (and (= x 0) (= y 0))
        nil
        (let [pl  (safe-2d-aget sumarr y (dec x) sy sx Long/MAX_VALUE)
              pu  (safe-2d-aget sumarr (dec y) x sy sx Long/MAX_VALUE)
              cur (safe-2d-aget sumarr y x sy sx Long/MAX_VALUE)
              mn  (min pl pu)]
          (aset sumarr y x (+ mn cur)))))
    (println (java.util.Arrays/deepToString sumarr))
    (aget sumarr (dec sy) (dec sx))))

(defn part1 []
  (risk-sum day15-small-grid))
