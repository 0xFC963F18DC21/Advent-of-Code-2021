(ns aoc2021.days.day18
  (:require [aoc2021.util.input :as inp]))

(def day18-numbers
  (->> "day18"
       (inp/get-input-lines)
       (map read-string)))

(defn explode-helper [n1 n2]
  (cond
    (and (nil? n1) (nil? n2))       (throw (Exception. "Two nil values given."))
    (nil? n1)                       n2
    (nil? n2)                       n1
    (and (vector? n1) (vector? n2)) [n1 n2]
    (vector? n1)                    (let [[a b] n1] [a (explode-helper b n2)])
    (vector? n2)                    (let [[a b] n2] [(explode-helper n1 a) b])
    :else                           (+ n1 n2)))

(defn explode
  ([pair?] (first (explode pair? 0)))
  ([pair? depth]
   (if (number? pair?)
     [pair? 0 0]
     (let [[a b]            pair?
           [exp-a al ar]    (explode a (inc depth))
           [exp-b' bl' br'] (explode (explode-helper ar b) (inc depth))]
       (cond
         (>= depth 4) [0 a b]
         :else [[(explode-helper exp-a bl') exp-b'] al br'])))))

(defn split-num [n]
  (if (< n 10)
    n
    (let [d (long (/ n 2))]
      [d (if (odd? n) (inc d) d)])))

(defn split-one
  ([pair?] (first (split-one pair? false)))
  ([pair? no-more?]
   (if no-more?
     pair?
     (if (number? pair?)
       (let [spl (split-num pair?)]
         (if (vector? spl)
           [spl true]
           [spl false]))
       (let [[a b]         pair?
             [spl-a nm-a] (if no-more? [a true] (split-one a no-more?))
             [spl-b nm-b] (if nm-a [b true] (split-one b nm-a))]
         [[spl-a spl-b] nm-b])))))

(defn add-sn [sn-pair1 sn-pair2]
  (loop [result   [sn-pair1 sn-pair2]
         exploded (explode result)
         splitted (split-one result)]
    (cond
      (not= exploded result) (recur exploded (explode exploded) (split-one exploded))
      (not= splitted result) (recur splitted (explode splitted) (split-one splitted))
      :else                  result)))

(defn add-coll-sn [sn-coll]
  (reduce add-sn sn-coll))

(defn magnitude [sn]
  (if (number? sn)
    sn
    (let [[a b] sn]
      (+ (* 3 (magnitude a)) (* 2 (magnitude b))))))

(defn part1 []
  (magnitude (add-coll-sn day18-numbers)))

(defn part2 []
  (->> (for [x day18-numbers
             y day18-numbers]
         (magnitude (add-sn x y)))
       (apply max)))
