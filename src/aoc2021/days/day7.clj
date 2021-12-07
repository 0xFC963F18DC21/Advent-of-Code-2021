(ns aoc2021.days.day7
  (:require [aoc2021.util.input :as util]
            [clojure.string :as str]))

(def day7-crabs
  (->> "day7"
       (util/get-input-file)
       (filter #(and (not= \newline %) (not= \return %)))
       (apply str)
       (#(str/split % #","))
       (map #(Long/parseLong %))
       (lazy-seq)))

(defn linear-fuel-rate [steps]
  steps)

(defn triangle-fuel-rate [steps]
  (/ (* steps (inc steps)) 2))

(defn fuel-consumption [crabs f-strat mean]
  (reduce
    (fn [acc crab]
      (let [diff (- crab mean)]
        (+ acc (f-strat (Math/abs diff)))))
    0
    crabs))

(defn min-dist-consumption [f-strat]
  (let [mn (apply min day7-crabs)
        mx (apply max day7-crabs)]
    (->> (range mn (inc mx))
         (map #(list % (fuel-consumption day7-crabs f-strat %)))
         (apply min-key #(nth % 1)))))

(defn part1 []
  (min-dist-consumption linear-fuel-rate))

(defn part2 []
  (min-dist-consumption triangle-fuel-rate))
