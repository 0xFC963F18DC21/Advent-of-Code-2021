(ns aoc2021.days.day1
  (:require [aoc2021.util.input :as util]))

(defn part1 []
  (let [asInts (util/get-parsed-input-lines "day1" #(Integer/parseInt %1))
        zipped (map vector asInts (drop 1 asInts))]
    (count (filter (fn [[a b]] (> b a)) zipped))))

(defn part2 []
  (let [asInts (util/get-parsed-input-lines "day1" #(Integer/parseInt %1))
        zipped (map vector asInts (drop 1 asInts) (drop 2 asInts))
        summed (map #(apply + %1) zipped)
        paired (map vector summed (drop 1 summed))]
    (count (filter (fn [[a b]] (> b a)) paired))))
