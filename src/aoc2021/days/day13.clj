(ns aoc2021.days.day13
  (:require [aoc2021.util.input :as inp]
            [clojure.java.io :as io]
            [clojure.string :as strs]))

(defn generate-info [[dots folds]]
  (list
    (reduce
      #(let [[sx sy] (strs/split %2 #",")]
         (conj %1 (list (Long/parseLong sx) (Long/parseLong sy))))
      #{}
      dots)
    (reduce
      #(let [[_ axis v] (re-find #".+([xy])=(.+)" %2)]
         (conj %1 (list (first axis) (Long/parseLong v))))
      []
      folds)))

(def day13-info
  (->> "day13"
       (inp/get-input-lines)
       (filter #(not (empty? %)))
       (partition-by #(< (count %) 10))
       (generate-info)
       (lazy-seq)))

(defn gen-fold [[axis v] [x y :as p]]
  (case axis
    \x (if (< x v) p (list (- v (- x v)) y))
    \y (if (< y v) p (list x (- v (- y v))))
    :Invalid-axis!))

(defn fold-once [grid fold]
  (reduce (fn [cur p] (conj cur (gen-fold fold p))) #{} grid))

(defn write-grid [grid]
  (let [max-x (reduce max 0 (map first grid))
        max-y (reduce max 0 (map second grid))
        rx    (range 0 (inc max-x))
        ry    (range 0 (inc max-y))
        sb    (StringBuilder.)]
    (io/delete-file "outputs/day31-part2.txt" :Does-not-exist-yet!)
    (doseq [y ry
            x rx]
      (if (contains? grid (list x y)) (.append sb \u25A0) (.append sb \space))
      (if (= x max-x) (.append sb \newline)))
    (spit "outputs/day13-part2.txt" (.toString sb))))

(defn part1 []
  (let [[grid [first & _]] day13-info]
    (count (fold-once grid first))))

(defn part2 []
  (let [[grid folds] day13-info]
    (write-grid (reduce fold-once grid folds))))
