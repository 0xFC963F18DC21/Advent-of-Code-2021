(ns aoc2021.days.day6
  (:require [aoc2021.util.input :as util]
            [clojure.string :as str]))

(defn fish-to-map [fishes]
  (reduce
    (fn [acc fish] (merge-with + acc {fish (count (filter #(= % fish) fishes))}))
    {}
    '(0 1 2 3 4 5 6 7 8)))

(def day6-fish
  (->> "day6"
       (util/get-input-file)
       (filter #(and (not= \newline %) (not= \return %)))
       (apply str)
       (#(str/split % #","))
       (map #(Long/parseLong %))
       (fish-to-map)))

(defn simulate [fishes]
  (reduce
    (fn [acc [fish cnt]]
      (if (= fish 0)
        (merge-with + acc {8 cnt 6 cnt})
        (merge-with + acc {(dec fish) cnt})))
    {}
    fishes))

(defn count-fish [fishes]
  (reduce (fn [acc [_ c]] (+ acc c)) 0 fishes))

(defn part1 []
  (->> day6-fish
       (iterate simulate)
       (drop 80)
       (first)
       (count-fish)))

(defn part2 []
  (->> day6-fish
       (iterate simulate)
       (drop 256)
       (first)
       (count-fish)))

