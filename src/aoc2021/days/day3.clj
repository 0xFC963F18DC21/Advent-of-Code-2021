(ns aoc2021.days.day3
  (:require [aoc2021.util.input :as util]
            [clojure.string :as string]))

(defn day3-lists []
  (util/get-parsed-input-lines
    "day3"
    #(map (fn [i] (Long/parseLong i)) (string/split %1 #""))))

(defn columnar-transpose [seq-of-seqs]
  (apply map list seq-of-seqs))

(defn count-n [seq n]
  (count (filter #(= n %) seq)))

(defn digit-minmax
  [seq]
  (let [zeroes (count-n seq 0)
        ones   (count-n seq 1)]
    (cond
      (> zeroes ones) (list 1 0)
      (< zeroes ones) (list 0 1)
      :else (list 0 1))))

(defn delta-epsilon [[acce accd] transposed-diagnostic]
  (let [[e d] (digit-minmax transposed-diagnostic)]
    (list (str acce e) (str accd d))))

(defn singleton? [coll]
  (= 1 (count coll)))

(defn life-solver [init]
  (loop [ol init
         cl init
         bit 0]
    (if (and (singleton? ol) (and singleton? cl))
      (list (first ol) (first cl))
      (let [tol (columnar-transpose ol)
            tcl (columnar-transpose cl)
            [_ obc] (digit-minmax (nth tol bit))
            [cbc _] (digit-minmax (nth tcl bit))]
        (recur
          (if (singleton? ol) ol (filter #(= obc (nth % bit)) ol))
          (if (singleton? cl) cl (filter #(= cbc (nth % bit)) cl))
          (inc bit))))))

(defn part1 []
  (let [transposed  (columnar-transpose (day3-lists))
        [e-str d-str] (reduce delta-epsilon (list "" "") transposed)
        eps         (Long/parseLong e-str 2)
        dlt         (Long/parseLong d-str 2)]
    (* eps dlt)))

(defn part2 []
  (let [p2l (day3-lists)
        [o c] (life-solver p2l)
        o-num (Long/parseLong (apply str o) 2)
        c-num (Long/parseLong (apply str c) 2)]
    (* o-num c-num)))


