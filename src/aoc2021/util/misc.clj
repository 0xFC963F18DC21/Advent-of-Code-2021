(ns aoc2021.util.misc)

(defn in
  "Checks if any of the items in items is in coll."
  [coll & items]
  (some (zipmap items (repeat true)) coll))
