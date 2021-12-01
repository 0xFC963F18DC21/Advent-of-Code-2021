(ns aoc2021.util.input
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentVector LazySeq)))

(defn ^String get-input-file [^String name]
  "Gets an input file as a string."
  (slurp (str "inputs/" name ".txt")))

(defn ^PersistentVector get-input-lines [^String name]
  "Gets an input file as a vector of strings."
  (let [s (get-input-file name)]
    (str/split-lines s)))

(defn ^LazySeq get-parsed-input-lines [^String name func]
  "Gets the lines of an input file, and parses them according to a parser function."
  (let [l (get-input-lines name)]
    (map func l)))

(defn print-all-in-lines [sequence]
  "Prints everything inside a sequence."
  (doseq [x sequence] (println x)))
