(ns aoc2021.days.day25
  (:require [aoc2021.util.input :as inp]
            [clojure.string :as strs])
  (:import (java.util.concurrent Executors)))

(def threads
  (Executors/newFixedThreadPool
    (.availableProcessors (Runtime/getRuntime))))

(defn gen-loc [num width]
  [(mod num width) (quot num width)])

(defn generate-map [lls]
  (let [height (count lls)
        width  (count (first lls))]
    {:Dimensions [width height]
     :Data       (->> (reduce
                        (fn [[m c] s]
                          (case s
                            :> [(assoc m (gen-loc c width) :East) (inc c)]
                            :v [(assoc m (gen-loc c width) :South) (inc c)]
                            :. [m (inc c)]))
                        [{} 0]
                        (apply concat lls))
                      (first))}))

(def day25-cucumbers
  (->> "day25"
       (inp/get-input-lines)
       (map #(strs/split % #""))
       (map (partial map keyword))
       (generate-map)))

(defn next-move [[width height] [[x y] dir]]
  (case dir
    :East  [(mod (inc x) width) y]
    :South [x (mod (inc y) height)]))

(defn check-space [data coords]
  (not (get data coords)))

(defn try-move [new-data-atom dims data [coords dir :as cucumber]]
  (let [next-sp  (next-move dims cucumber)
        can-move (check-space data next-sp)]
    (if can-move
      (swap! new-data-atom assoc next-sp dir)
      (swap! new-data-atom assoc coords dir))))

(defn step [cucumber-map]
  (let [dims                        (get cucumber-map :Dimensions)
        data                        (get cucumber-map :Data)
        {easts :East souths :South} (group-by second data)
        new-data                    (atom {})]
    (let [tasks   (for [east easts] (fn [] (try-move new-data dims data east)))
          futures (.invokeAll threads tasks)]
      (doseq [future futures] (.get future)))
    (let [result    (atom @new-data)
          used-data (merge @new-data (into {} souths))
          tasks     (for [south souths] (fn [] (try-move result dims used-data south)))
          futures   (.invokeAll threads tasks)]
      (doseq [future futures] (.get future))
      {:Dimensions dims :Data @result})))

(defn until-unmoving [cucumber-map]
  (loop [steps 1
         old   cucumber-map
         new   (step cucumber-map)]
    (if (= old new)
      steps
      (recur (inc steps) new (step new)))))

(defn part1 []
  (until-unmoving day25-cucumbers))

