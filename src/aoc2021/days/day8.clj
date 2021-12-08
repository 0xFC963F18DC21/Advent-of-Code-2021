(ns aoc2021.days.day8
  (:require [aoc2021.util.input :as inp]
            [clojure.set :as sets]
            [clojure.string :as strs]))

(def day8-sequences
  (-> "day8"
      (inp/get-parsed-input-lines #(strs/split % #" \| "))
      (#(map (fn [[in out]] (list (strs/split in #" ") (strs/split out #" "))) %))
      (lazy-seq)))

(def segment-count
  {2 1
   3 7
   4 4
   5 (list 2 3 5)
   6 (list 0 6 9)
   7 8})

(defn count-1s-4s-7s-8s [coll]
  (->> coll
       (filter #(not (coll? (get segment-count (count %)))))
       (count)))

; Every line contains mappings for all digits. We just need to figure out what they are.
; 0 is a superset of 7, but NOT a superset of 4
; 2 is not a superset of any digit.
; 3 is a superset of 7, and the intersection between 4 and 3 is 3 segments.
; 5 is a subset of 9, and is not the superset of any other number.
; 6 is a superset of 5
; 9 is a superset of 4 UNION 7
(defn set-match [str1 str2]
  (= (set str1) (set str2)))

(defn strict-superset? [set1 set2]
  (and (not= set1 set2) (sets/superset? set1 set2)))

(defn strict-subset? [set1 set2]
  (and (not= set1 set2) (sets/subset? set1 set2)))

(defn determine-string [digit found possibilities]
  (case digit
    0 (first (filter #(let [as-set (set %)]
                        (and (strict-superset? as-set (set (get found 7)))
                             (not (strict-superset? as-set (set (get found 4))))
                             (= 6 (count as-set))))
                     possibilities))
    2 (first (filter #(let [as-set (set %)]
                        (and (not (strict-subset? as-set (set (get found 9))))
                             (= 4 (count (sets/intersection as-set (set (get found 3)))))))
                     possibilities))
    3 (first (filter #(let [as-set (set %)]
                        (and (strict-superset? as-set (set (get found 7)))
                             (= 3 (count (sets/intersection as-set (set (get found 4)))))))
                     possibilities))
    5 (first (filter #(let [as-set (set %)]
                        (and (strict-subset? as-set (set (get found 9)))
                             (= 4 (count (sets/intersection as-set (set (get found 3)))))))
                     possibilities))
    6 (first (filter #(let [as-set (set %)]
                        (and (strict-superset? as-set (set (get found 5)))
                             (not= % (get found 9))))
                     possibilities))
    9 (first (filter #(let [as-set (set %)]
                        (and (strict-superset? as-set (sets/union (set (get found 7)) (set (get found 4))))
                             (= 6 (count as-set))))
                     possibilities))))

(defn multi-pass-determine [inputs]
  ; Attempt to determine the digit mapping for the easy digits.
  (let [[found remaining] (reduce
                            (fn [[fsf rem] input]
                              (let [res (get segment-count (count input))]
                                (if (coll? res)
                                  (list fsf (conj rem input))
                                  (list (merge fsf {res input}) rem))))
                            (list nil [])
                            (sort #(compare (count %1) (count %2)) inputs))
        everything        (reduce
                            #(merge %1 {%2 (determine-string %2 %1 remaining)})
                            found
                            (list 0 3 9 5 6 2))]
    everything))

(defn determine-output [digits d-map]
  (->> (map
         #(first (first (filter (fn [[_ ds]] (set-match % ds)) d-map)))
         digits)
       (apply str)
       (Long/parseLong)))

(defn part1 []
  (->> day8-sequences
       (map #(nth % 1))
       (map count-1s-4s-7s-8s)
       (apply +)))

(defn part2 []
  (->> day8-sequences
       (map (fn [[ins outs]] (determine-output outs (multi-pass-determine ins))))
       (apply +)))
