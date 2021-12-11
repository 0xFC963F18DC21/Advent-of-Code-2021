(ns aoc2021.days.day11
  (:require [aoc2021.util.input :as inp]
            [clojure.string :as strs]))

(def day11-grid
  (->> "day11"
       (inp/get-input-lines)
       (mapv #(strs/split % #""))
       (mapv (fn [l] (mapv #(Long/parseLong %) l)))))

(defn increment-all [grid]
  (mapv #(mapv inc %) grid))

(defn neighbours [grid x y]
  (let [my (count grid)
        mx (count (grid 0))]
    (filter
      #(and (not= (list x y) %)
            (<= 0 (first %)) (<= 0 (second %))
            (< (first %) mx) (< (second %) my))
      (for [xx (range (dec x) (+ x 2))
            yy (range (dec y) (+ y 2))]
        (list xx yy)))))

(defn get-at [grid x y]
  ((grid y) x))

(defn set-at [grid x y v]
  (assoc grid y (assoc (grid y) x v)))

(defn flash-at [grid x y]
  (if (or (= '* (get-at grid x y)) (< (get-at grid x y) 10))
    (list grid false)
    (let [ns     (neighbours grid x y)
          marked (set-at grid x y '*)]
      (list
        (reduce
          (fn [m [nx ny]]
            (let [cv (get-at grid nx ny)]
              (if (not= cv '*)
                (set-at m nx ny (inc (get-at m nx ny)))
                m)))
          marked
          ns)
        true))))

(defn flash-all [grid]
  (loop [cgrid grid
         ofs   0]
    (let [fs (atom 0)
          post-flash (reduce
                       (fn [cgrid' [x y]]
                         (let [[ngrid f?] (flash-at cgrid' x y)]
                           (if f?
                             (do (swap! fs inc) ngrid)
                             ngrid)))
                       cgrid
                       (for [xx (range 0 (count (grid 0))) yy (range 0 (count grid))] (list xx yy)))]
      (if (> @fs 0)
        (recur post-flash (+ ofs @fs))
        (list post-flash ofs)))))

(defn clear-flashes [grid]
  (mapv (fn [l] (mapv #(if (= % '*) 0 %) l)) grid))

(defn simulate [grid steps]
  (loop [cgrid grid
         i     0
         fs    0]
    (if (>= i steps)
      fs
      (let [[f-ed fs'] (flash-all (increment-all cgrid))]
        (recur (clear-flashes f-ed) (inc i) (+ fs' fs))))))

(defn sync? [grid fs]
  (= fs (* (count grid) (count (grid 0)))))

(defn simulate-until-sync [grid]
  (loop [cgrid grid
         steps 0
         fs    0]
    (if (sync? cgrid fs)
      steps
      (let [[f-ed fs'] (flash-all (increment-all cgrid))
            cleared  (clear-flashes f-ed)]
        (recur cleared (inc steps) fs')))))

(defn part1 []
  (simulate day11-grid 100))

(defn part2 []
  (simulate-until-sync day11-grid))

