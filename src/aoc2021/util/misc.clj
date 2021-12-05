(ns aoc2021.util.misc)

(defn in
  "Checks if any of the items in items is in coll."
  [coll & items]
  (some (zipmap items (repeat true)) coll))

(defn transpose
  "Transposes a collection of collections."
  [coll-of-colls]
  (apply map list coll-of-colls))

(defn signum
  "Gets the sign of a number. -1 = negative. 0 = number is zero. 1 = number is positive."
  [n]
  (cond
    (> n 0) 1
    (< n 0) -1
    :else 0))
