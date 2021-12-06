(ns aoc2021.days.day6
  (:require [aoc2021.util.input :as util]
            [clojure.string :as str]))

(defn fish-to-map [fishes]
  (reduce
    (fn [acc fish] (merge-with + acc {fish (bigint (count (filter #(= % fish) fishes)))}))
    {}
    '(0 1 2 3 4 5 6 7 8)))

(def day6-fish
  (->> "day6"
       (util/get-input-file)
       (filter #(and (not= \newline %) (not= \return %)))
       (apply str)
       (#(str/split % #","))
       (map bigint)
       (fish-to-map)))

(defn simulate [fishes days]
  (if (> days 0)
    (recur
      (reduce
        (fn [acc [fish cnt]]
          (if (= fish 0)
            (merge-with + acc {8 cnt 6 cnt})
            (merge-with + acc {(dec fish) cnt})))
        {}
        fishes)
      (dec days))
    fishes))

(defn count-fish [fishes]
  (reduce (fn [acc [_ c]] (+ acc c)) 0N fishes))

(defn part1 []
  (-> day6-fish
      (simulate 80)
      (count-fish)))

(defn part2 []
  (-> day6-fish
      (simulate 256)
      (count-fish)))

