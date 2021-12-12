(ns aoc2021.days.day12
  (:require [aoc2021.util.input :as inp]
            [clojure.set :as sets]
            [clojure.string :as strs]))

(defn create-or-get-node [map name]
  (if (contains? map name)
    {name (map name)}
    {name
     {:Type    (if (= (strs/upper-case name) name) :Big :Small)
      :Visited 0
      :Paths   (atom [])}}))

(defn collect-nodes [edges]
  (reduce
    (fn [cur [lname rname]]
      (let [lnode (create-or-get-node cur lname)
            rnode (create-or-get-node cur rname)]
        (swap! ((lnode lname) :Paths) conj rname)
        (swap! ((rnode rname) :Paths) conj lname)
        (merge (merge lnode rnode) cur)))
    {}
    edges))

(def day12-caves
  (-> "day12"
      (inp/get-parsed-input-lines #(strs/split % #"-"))
      (collect-nodes)))

(defn visit? [map name twice]
  (if (or (= ((map name) :Type) :Big) (< ((map name) :Visited) (if (= name twice) 2 1)))
    (list (assoc map name (assoc (map name) :Visited (inc ((map name) :Visited)))) true)
    (list map false)))

(defn all-paths [so-far start-where map twice]
  (if (= start-where "end")
    (list (cons "end" so-far))
    (let [possibilities @((map start-where) :Paths)
          cph           (cons start-where so-far)]
      (reduce
        (fn [paths cnext]
          (let [[m v?] (visit? map cnext twice)]
            (if v?
              (concat paths (map #(concat cph %) (all-paths cph cnext m twice)))
              paths)))
        ()
        (remove (partial = "start") possibilities)))))

(defn part1 []
  (count (all-paths () "start" day12-caves nil)))

(defn part2 []
  (let [names (keys day12-caves)
        flt (remove #(some #{%} '("start" "end")) names)
        no-big (remove #(= (strs/upper-case %) %) flt)]
    (count (apply sets/union (map #(set (all-paths () "start" day12-caves %)) no-big)))))
