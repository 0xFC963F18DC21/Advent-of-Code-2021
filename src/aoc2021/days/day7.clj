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
    (fn [acc ^Long crab]
      (let [diff (- crab mean)]
        (+ acc (f-strat (Math/abs diff)))))
    0
    crabs))

(defn min-dist-consumption [f-strat]
  (let [srt    (sort day7-crabs)
        cnt    (count day7-crabs)
        mean   (long (/ (apply + day7-crabs) cnt))
        mn-1   (dec mean)
        median (nth srt (/ cnt 2))
        med-1  (nth srt (dec (/ cnt 2)))]
    (apply min-key
           #(nth % 1)
           (map #(list % (fuel-consumption day7-crabs f-strat %))
                (list mean mn-1 median med-1)))))

(defn part1 []
  (min-dist-consumption linear-fuel-rate))

(defn part2 []
  (min-dist-consumption triangle-fuel-rate))
