(ns aoc2021.days.day5
  (:require [aoc2021.util.input :as inp]
            [aoc2021.util.misc :as misc]
            [clojure.string :as str]))

(def day5-line-segments
  (->> "day5"
       (inp/get-input-lines)
       (map #(str/split % #" -> |,"))
       (map (fn [l] (map #(Long/parseLong %) l)))
       (map (fn [[x1 y1 x2 y2]] (list (list x1 y1) (list x2 y2))))))

(defn is-planar [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn range' [st en]
  (let [sgn (misc/signum (- en st))]
    (if (= 0 sgn)
      (-> st (list) (lazy-seq))
      (-> (+ st sgn) (range' en) (conj st) (lazy-seq)))))

(defn pair-range [[s1 s2] [e1 e2]]
  (map list (range' s1 e1) (range' s2 e2)))

(defn create-line [[[x1 y1 :as fst] [x2 y2 :as snd]]]
  (cond
    (= x1 x2) (map (fn [i] {(list x1 i) 1}) (range' y1 y2))
    (= y1 y2) (map (fn [i] {(list i y1) 1}) (range' x1 x2))
    :else     (map (fn [p] {p 1}) (pair-range fst snd))))

(defn count-intersections [raw-segments planar-only?]
  (let [used-segments   (if planar-only? (filter is-planar raw-segments) raw-segments)
        lines           (apply concat (map create-line used-segments))
        points-of-lines (reduce #(merge-with + %1 %2) {} lines)
        intersections   (filter (fn [[_ c]] (> c 1)) points-of-lines)]
    (count intersections)))

(defn part1 []
  (count-intersections day5-line-segments true))

(defn part2 []
  (count-intersections day5-line-segments false))

