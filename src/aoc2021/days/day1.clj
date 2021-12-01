(ns aoc2021.days.day1
  (:require [aoc2021.util.input :as util]))

(def day1-ints
  (util/get-parsed-input-lines "day1" #(Integer/parseInt %1)))

(defn part1 []
  (let [zipped (map vector day1-ints (drop 1 day1-ints))]
    (count (filter (fn [[a b]] (> b a)) zipped))))

(defn part2 []
  (let [zipped (map vector day1-ints (drop 1 day1-ints) (drop 2 day1-ints))
        summed (map #(apply + %1) zipped)
        paired (map vector summed (drop 1 summed))]
    (count (filter (fn [[a b]] (> b a)) paired))))
