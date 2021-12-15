(ns aoc2021.days.day15
  (:require [aoc2021.util.input :as inp])
  (:import (java.util PriorityQueue Comparator)))

(def day15-grid
  (->> "day15"
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

(defn risk-sum [grid exxxxpand]
  (let [dummy   2147483647
        sy      (count grid)
        sx      (count (first grid))
        usx     (* sx (if exxxxpand 5 1))
        usy     (* sy (if exxxxpand 5 1))
        add     (fn [x y] (+ (long (/ x sx)) (long (/ y sy))))
        nrm     (fn [a]   (mod a sx))
        sub     (fn [v] (if (> v 9) (- v 9) v))
        gg      (fn [x y]
                  (let [x' (nrm x)
                        y' (nrm y)
                        v  ((grid y') x')
                        a  (+ (add x y) v)]
                    (sub a)))
        sumarr  (to-array-2d (repeat usy (repeat usx dummy)))
        p-queue (PriorityQueue.
                  (reify
                    Comparator
                    (compare [_ [x1 y1 v1] [x2 y2 v2]]
                      (let [dx1   (- (dec usx) x1)
                            dy1   (- (dec usy) y1)
                            dx2   (- (dec usx) x2)
                            dy2   (- (dec usy) y2)]
                        (- (+ dx1 dy1 v1) (+ dy2 dx2 v2))))))]
    (.offer p-queue (list 0 0 0))
    (aset sumarr 0 0 0)
    (while (not (.isEmpty p-queue))
      (let [[x y _] (.poll p-queue)
            go (fn [x' y' cr']
                 (let [p (safe-2d-aget sumarr y' x' usy usx nil)
                       s (lazy-seq (list (+ cr' (gg x' y'))))]
                   (if (and (not (nil? p)) (> p (first s)))
                     (do
                       (aset sumarr y' x' (first s))
                       (.offer p-queue (list x' y' (first s)))))))
            cr (aget sumarr y x)]
        (if (and (= x (dec usx)) (= y (dec usy)))
          (.clear p-queue)
          (do
            (go (dec x) y cr)
            (go x (dec y) cr)
            (go (inc x) y cr)
            (go x (inc y) cr)))))
    (aget sumarr (dec usy) (dec usx))))

(defn part1 []
  (risk-sum day15-grid false))

(defn part2 []
  (risk-sum day15-grid true))
