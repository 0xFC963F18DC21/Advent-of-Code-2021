(ns aoc2021.util.input
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentVector LazySeq)))

(defn ^String get-input-file
  "Gets an input file as a string."
  [^String name] (slurp (str "inputs/" name ".txt")))

(defn ^PersistentVector get-input-lines
  "Gets an input file as a vector of strings."
  [^String name]
  (let [s (get-input-file name)]
    (str/split-lines s)))

(defn ^LazySeq get-parsed-input-lines
  "Gets the lines of an input file, and parses them according to a parser function."
  [^String name func]
  (let [l (get-input-lines name)]
    (map func l)))

(defn print-all-in-lines
  "Prints everything inside a sequence."
  [sequence] (doseq [x sequence] (println x)))
