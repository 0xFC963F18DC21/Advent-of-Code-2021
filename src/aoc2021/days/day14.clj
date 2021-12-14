(ns aoc2021.days.day14
  (:require [aoc2021.util.input :as inp]
            [clojure.string :as strs]))

(def day14-rules
  (->> "day14"
       (inp/get-input-lines)
       (filter #(not (empty? %)))
       (partition-by #(< (count %) 8))
       ((fn [[[init] rules]]
          (list (reduce #(merge-with + %1 {%2 1}) {} (map str init (next init)))
                (reduce
                  #(let [[pair [r']] (strs/split %2 #" -> ")]
                     (assoc %1 pair (fn [[f s] qty]
                                      (list {(str f r') qty} {(str r' s) qty}))))
                  {}
                  rules)
                (first init))))
       (lazy-seq)))

(defn process [pairs rules]
  (reduce
    (fn [acc [k v]]
      (let [[p1 p2] ((get rules k) k v)]
        (merge-with + acc p1 p2)))
    {}
    pairs))

(defn third [coll]
  (first (next (next coll))))

(defn freqs-after-n [pairs rules fst n]
  (let [polymer    (nth (iterate #(process % rules) pairs) n)
        with-first (merge-with + {fst 1} (reduce (fn [acc [[_ s] v]] (merge-with + acc {s v})) {} polymer))
        sorted     (sort-by val with-first)
        least      (val (first sorted))
        most       (val (last sorted))]
    (- most least)))

(defn part1 []
  (freqs-after-n (first day14-rules) (second day14-rules) (third day14-rules) 10))

(defn part2 []
  (freqs-after-n (first day14-rules) (second day14-rules) (third day14-rules) 40))
