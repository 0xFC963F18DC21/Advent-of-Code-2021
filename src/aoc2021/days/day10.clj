(ns aoc2021.days.day10
  (:require [aoc2021.util.input :as inp]
            [clojure.string :as strs]))

(def day10-lines
  (-> "day10"
      (inp/get-parsed-input-lines #(map symbol (strs/split % #"")))
      (lazy-seq)))

(def starts
  {(symbol "(") (symbol ")")
   (symbol "[") (symbol "]")
   (symbol "{") (symbol "}")
   (symbol "<") (symbol ">")})

(def scores
  {(symbol ")") '(3 1)
   (symbol "]") '(57 2)
   (symbol "}") '(1197 3)
   (symbol ">") '(25137 4)})

(defn match? [start end]
  (= (starts start) end))

(defn line-reader [no-sym-str-act no-sym-end-act illegal-act]
  (letfn [(lr [[sym & syms] [top & rest :as stk]]
            (if (starts sym)
              (if syms
                (recur syms (cons sym stk))
                (no-sym-str-act sym stk))
              (if (match? top sym)
                (if syms
                  (recur syms rest)
                  (no-sym-end-act sym stk))
                (illegal-act sym))))]
    lr))

(defn syn-err-score [line]
  ((line-reader (constantly 0) (constantly 0) #(first (scores %))) line ()))

(defn autocheck-stk [line]
  ((line-reader #(cons %1 %2) #(next %2) #(throw (RuntimeException. (str "Unexpected: " %)))) line ()))

(defn autocheck-score [line]
  (reduce #(+ (* 5 %1) %2) 0 (map #(-> % (starts) (scores) (second)) (autocheck-stk line))))

(defn part1 []
  (reduce + 0 (map syn-err-score day10-lines)))

(defn part2 []
  (let [incomplete (filter #(= 0 (syn-err-score %)) day10-lines)
        scores     (map autocheck-score incomplete)]
    (nth (sort scores) (/ (count scores) 2))))
