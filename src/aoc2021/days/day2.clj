(ns aoc2021.days.day2
  (:require [clojure.string :as str]
            [aoc2021.util.input :as util]))

(def day2-pairs
  (util/get-parsed-input-lines
    "day2"
    (fn [s]
      (let [[cmd n] (str/split s #" ")]
        (list cmd (Long/parseLong n))))))

(defn new-sub-data [[dist p2depth p1depth-p2aim] [cmd n]]
  (case cmd
    "forward" (list (+ dist n) (+ p2depth (* p1depth-p2aim n)) p1depth-p2aim)
    "down" (list dist p2depth (+ p1depth-p2aim n))
    "up" (list dist p2depth (- p1depth-p2aim n))))

(defn get-path-sum [commands]
  (reduce new-sub-data '(0 0 0) commands))

(defn part1 []
  (let [[ds _ dp] (get-path-sum (doall day2-pairs))]
    (* ds dp)))

(defn part2 []
  (let [[ds dp] (get-path-sum (doall day2-pairs))]
    (* ds dp)))
