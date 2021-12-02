(ns aoc2021.days.day2
  (:require [clojure.string :as str]
            [aoc2021.util.input :as util]))

(def day2-pairs
  (util/get-parsed-input-lines
    "day2"
    (fn [s]
      (let [[cmd n] (str/split s #" ")]
        (list cmd (Integer/parseInt n))))))

(defn new-sub-data [[dist depth aim] [cmd n]]
  (if aim
    (case cmd
      "forward" (list (+ dist n) (+ depth (* aim n)) aim)
      "down" (list dist depth (+ aim n))
      "up" (list dist depth (- aim n)))
    (case cmd
      "forward" (list (+ dist n) depth)
      "down" (list dist (+ depth n))
      "up" (list dist (- depth n)))))

(defn get-path-sum [commands]
  (reduce new-sub-data '(0 0) commands))

(defn get-path-sum-aimed [commands]
  (reduce new-sub-data '(0 0 0) commands))

(defn part1 []
  (let [[ds dp] (get-path-sum (doall day2-pairs))]
    (* ds dp)))

(defn part2 []
  (let [[ds dp] (get-path-sum-aimed (doall day2-pairs))]
    (* ds dp)))
