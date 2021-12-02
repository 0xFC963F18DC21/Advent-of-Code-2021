(ns aoc2021.days.day2
  (:require [clojure.string :as str]
            [aoc2021.util.input :as util]))

(def day2-pairs
  (util/get-parsed-input-lines
    "day2"
    (fn [s]
      (let [[cmd n] (str/split s #" ")]
        (list cmd (Integer/parseInt n))))))

(defn get-path-sum [[[cmd n] & rest] [dist depth]]
  (let [fwd (lazy-seq (list (+ dist n) depth))
        dp+ (lazy-seq (list dist (+ depth n)))
        dp- (lazy-seq (list dist (- depth n)))]
    (case cmd
      "forward" (if rest (recur rest fwd) fwd)
      "down" (if rest (recur rest dp+) dp+)
      "up" (if rest (recur rest dp-) dp-))))

(defn get-path-sum-aimed [[[cmd n] & rest] [dist depth aim]]
  (let [fwd (lazy-seq (list (+ dist n) (+ depth (* aim n)) aim))
        dp+ (lazy-seq (list dist depth (+ aim n)))
        dp- (lazy-seq (list dist depth (- aim n)))]
    (case cmd
      "forward" (if rest (recur rest fwd) fwd)
      "down" (if rest (recur rest dp+) dp+)
      "up" (if rest (recur rest dp-) dp-))))

(defn part1 []
  (let [[ds dp] (get-path-sum (doall day2-pairs) '(0 0))]
    (* ds dp)))

(defn part2 []
  (let [[ds dp & _] (get-path-sum-aimed (doall day2-pairs) '(0 0 0))]
    (* ds dp)))

