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

(defmacro trace
  "Prints the result of an expression, and returns it. Useful for debugging."
  [expr]
  `(let [macro-result# ~expr]
     (println macro-result#)
     macro-result#))

(defn non-nil?
  "Returns true if an item is non-nil. Returns false if nil"
  [x]
  (-> x (nil?) (not)))

(defn filter-map
  "Filters a map, and returns the new map created by filtering the map."
  [f map']
  (->> map'
       (filter f)
       (into {})))

(defn TODO
  "Acts as a bit of aggressive todo marker."
  [& _]
  (throw (UnsupportedOperationException. "Please implement this function.")))
