(ns aoc2021.util.func)

(defn flip
  "Flips the argument order of a two-argument function."
  [f] #(f %2 %1))

(defn compose
  "Takes two one-argument functions and composes them together, where given
  functions f and g, applies them as if it was done as (f (g arg))."
  [f g] #(f (g %1)))

(defn dbl-cmp
  "Takes a one-argument function and a two-argument function and applies them in
  such a way that the output of the latter is fed as the input to the former.
  I.e. if f is the one-argument function and g is the two-argument function,
  they are applied in the following manner: (f (g arg1 arg2))."
  [f g] #(f (g %1 %2)))
