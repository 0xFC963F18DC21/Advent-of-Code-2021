(ns aoc2021.days.day1
  (:require [aoc2021.util.input :as inp]))

(def day1-ints
  (inp/get-parsed-input-lines "day1" #(Integer/parseInt %1)))

(defn ^Long count-snd-lager [pairs]
  (count (filter (fn [[a b]] (> b a)) pairs)))

(defn ^Long part1 []
  (let [zipped (map vector day1-ints (drop 1 day1-ints))]
    (count-snd-lager zipped)))

(defn ^Long part2 []
  (let [zipped (map vector day1-ints (drop 1 day1-ints) (drop 2 day1-ints))
        summed (map #(apply + %1) zipped)
        paired (map vector summed (drop 1 summed))]
    (count-snd-lager paired)))
