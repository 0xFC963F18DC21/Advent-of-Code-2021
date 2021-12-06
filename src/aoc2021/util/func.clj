(ns aoc2021.util.func
  (:import (java.lang FunctionalInterface)))

(defn flip
  "Flips the argument order of a two-argument function."
  [f & args] (apply f (reverse args)))

(defmacro ^FunctionalInterface afi1
  "Turns this Clojure function into a 1-arity Java SAM FunctionalInterface object."
  [f intr-name sam-name]
  `(reify
     ~intr-name
     (~sam-name [_# arg#]
       (~f arg#))))

(defmacro ^FunctionalInterface afi2
  "Turns this Clojure function into a 2-arity Java SAM FunctionalInterface object."
  [f intr-name sam-name]
  `(reify
     ~intr-name
     (~sam-name [_# arg1# arg2#]
       (~f arg1# arg2#))))
