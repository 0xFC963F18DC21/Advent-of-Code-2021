(ns aoc2021.days.day20
  (:require [aoc2021.util.input :as inp])
  (:import (java.util.concurrent Executors)))

(defn to-image [coll-of-strs]
  (->> coll-of-strs
       (reduce
         (fn [[c m] l]
           (list
             (inc c)
             (merge m {c (into {} (for [i (range 0 (count l))] [i (nth l i)]))})))
         (list 0 {}))
       (second)))

(def day20-info
  (->> "day20"
       (inp/get-input-lines)
       (partition-by count)
       (filter #(not (empty? (first %))))
       ((fn [[bs los]] [(first bs) (to-image los) false]))
       ((fn [[bs img rih]] {:IEA bs :Image img :RestIs# rih :Bounds [0 0 (dec (count (img 0))) (dec (count img))]}))))

(def threads
  (Executors/newFixedThreadPool
    (.availableProcessors (Runtime/getRuntime))))

(defn rest-is-what [info]
  (if (info :RestIs#)
    (= \# (nth (info :IEA) 511))
    (= \# (nth (info :IEA) 0))))

(defn get-pixel [info x y]
  (let [place (if (get info :RestIs#) \# \.)]
    (get (get (get info :Image) y) x place)))

(defn get-nine [info x y]
  (->> (for [y' (range (dec y) (+ y 2))
             x' (range (dec x) (+ x 2))]
         (get-pixel info x' y'))
       (apply str)))

(defn to-num [nine]
  (Long/parseLong (apply str (map #(if (= % \#) 1 0) nine)) 2))

(defn gen-task [info image-atom y xmn xmx]
  (fn []
    (let [iea (get info :IEA)]
      (swap! image-atom merge {y (into {}
                                       (for [x (range xmn (inc xmx))]
                                         [x (nth iea (to-num (get-nine info x y)))]))}))))

(defn enhance [info]
  (let [new-image                          (atom {})
        new-rih                            (rest-is-what info)
        [oxmn oymn oxmx oymx]              (get info :Bounds)
        [nxmn nymn nxmx nymx :as n-bounds] [(dec oxmn) (dec oymn) (inc oxmx) (inc oymx)]
        tasks                              (for [y (range nymn (inc nymx))] (gen-task info new-image y nxmn nxmx))
        futures                            (.invokeAll threads tasks)]
    (doseq [future futures] (.get future))
    {:IEA (get info :IEA) :Image @new-image :RestIs# new-rih :Bounds n-bounds}))

(defn count-pixels [info]
  (let [image (get info :Image)
        r-2-c (map (fn [[_ m]] (reduce (fn [acc [_ p]] (if (= p \#) (inc acc) acc)) 0 m)) image)]
    (reduce + 0 r-2-c)))

(defn enhance-n [n info]
  (loop [i     n
         info' info]
    (if (= i 0)
      info'
      (recur (dec i) (enhance info')))))

(defn part1 []
  (->> day20-info
       (enhance-n 2)
       (count-pixels)))

(defn part2 []
  (->> day20-info
       (enhance-n 50)
       (count-pixels)))
