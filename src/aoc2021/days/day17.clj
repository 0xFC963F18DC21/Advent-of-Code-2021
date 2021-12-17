(ns aoc2021.days.day17
  (:require [aoc2021.util.input :as inp]
            [aoc2021.util.misc :as misc]
            [clojure.set :as sets]))

(def day17-points
  (->> "day17"
       (inp/get-input-file)
       (re-find #"x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)")
       (next)
       (map #(Long/parseLong %))
       ((fn [[x1 y1 x2 y2]] (list (list x1 y1) (list x2 y2))))
       (lazy-seq)))

(defn in-range [v v-min v-max]
  (and (<= v-min v) (<= v v-max)))

(defn xi-can-hit-range [xi x-min x-max]
  (reduce
    (fn [[x s] vx]
      (if (in-range x x-min x-max)
        (reduced
          (set
            (reduce
              #(if (in-range (+ x (reduce + 0 (range %2 (inc vx)))) x-min x-max)
                 (if (= %2 0)
                   (conj %1 :INF (inc (first %1)))
                   (conj %1 (inc (first %1))))
                 (reduced %1))
              (list s)
              (reverse (range 0 (inc vx))))))
        (if (and (= vx 0) (not (in-range x x-min x-max)))
          (reduced false)
          (list (+ x vx) (inc s)))))
    (list 0 0)
    (iterate #(- % (misc/signum %)) xi)))

(defn yi-can-hit-range [yi y-min y-max]
  (reduce
    (fn [[y s] vy]
      (if (in-range y y-min y-max)
        (reduced
          (set
            (reduce
              #(if (in-range (+ y (reduce + 0 (range %2 (inc vy)))) y-min y-max)
                 (conj %1 (inc (first %1)))
                 (reduced %1))
              (list s)
              (iterate dec vy))))
        (if (< y y-min)
          (reduced false)
          (list (+ y vy) (inc s)))))
    (list 0 0)
    (iterate dec yi)))

(defn part1 []
  (let [[[_ _] [y-min _]] day17-points
        ym                (Math/abs ^long y-min)
        um                (dec ym)]
    (/ (* um ym) 2)))

(defn part2 []
  (let [[[x-min x-max] [y-min y-max]] day17-points
        axmx (Math/abs ^long x-max)
        aymn (Math/abs ^long y-min)
        wsvx (filter second (map #(list % (xi-can-hit-range % x-min x-max)) (range (- axmx) (inc axmx))))
        wsvy (filter second (map #(list % (yi-can-hit-range % y-min y-max)) (range (- aymn) (inc aymn))))]
    (->> (for [[vx sxs] wsvx
               [vy sys] wsvy]
           (if (or (and (contains? sxs :INF) (>= (apply min sys) (apply min (filter #(not= :INF %) sxs))))
                   (not (empty? (sets/intersection sxs sys))))
             (list vx vy)
             nil))
         (filter #(not (nil? %)))
         (count))))
