(ns aoc2021.days.day4
  (:require [clojure.string :as str]
            [aoc2021.util.input :as util]
            [aoc2021.util.misc :as misc]))

(defn day4-lines []
  (->> "day4"
       (util/get-input-lines)
       (filter not-empty)))

(defn split-long-parse [st]
  (map #(Long/parseLong %) (filter not-empty (str/split st #" "))))

(defn make-bingo-board [[[a1 a2 a3 a4 a5]
                         [b1 b2 b3 b4 b5]
                         [c1 c2 c3 c4 c5]
                         [d1 d2 d3 d4 d5]
                         [e1 e2 e3 e4 e5]
                         & inp]]
  (list
    {:r1 {a1 false a2 false a3 false a4 false a5 false}
     :r2 {b1 false b2 false b3 false b4 false b5 false}
     :r3 {c1 false c2 false c3 false c4 false c5 false}
     :r4 {d1 false d2 false d3 false d4 false d5 false}
     :r5 {e1 false e2 false e3 false e4 false e5 false}}
    inp))

(defn call-number [{r1 :r1
                    r2 :r2
                    r3 :r3
                    r4 :r4
                    r5 :r5
                    :as board}
                   n]
  (cond
    (misc/in (keys r1) n) (assoc board :r1 (assoc r1 n true))
    (misc/in (keys r2) n) (assoc board :r2 (assoc r2 n true))
    (misc/in (keys r3) n) (assoc board :r3 (assoc r3 n true))
    (misc/in (keys r4) n) (assoc board :r4 (assoc r4 n true))
    (misc/in (keys r5) n) (assoc board :r5 (assoc r5 n true))
    :else board))

(defn extract-called-numbers [[i & is]]
  (vector
    (map #(Long/parseLong %) (str/split i #","))
    (map split-long-parse is)))

(defn create-boards [inp]
  (loop [remain inp
         curr   (list)]
    (if remain
      (let [[b r] (make-bingo-board remain)]
        (recur r (conj curr b)))
      curr)))

(defn fully-parse [lines]
  (let [[calls rest] (extract-called-numbers lines)
        boards       (create-boards rest)]
    (list calls boards)))

(defn board-to-list-of-lists [board]
  (map (fn [[_ r]] (vals r)) board))

(defn check-win [board]
  ; The outer reduction checks rows, we now need to check the columns for a win too. Probably using a helper.
  (reduce
    (fn [acc [_ r]]
      (or acc (reduce #(and %1 %2) (vals r))))
    ; Columnar check for win.
    (reduce
      (fn [acc vs]
        (or acc (reduce #(and %1 %2) vs)))
      false
      (-> board (board-to-list-of-lists) (misc/transpose)))
    board))

(defn find-win [boards]
  (reduce
    #(if (and (not %1) (check-win %2)) %2 %1)
    nil
    boards))

(defn get-score [called board]
  (let [sum (reduce (fn [acc [_ r]] (apply + acc (->> r (filter #(->> % (next) (first) (not))) (map first)))) 0 board)]
    (* called sum)))

(defn part1 []
  (loop [[calls boards] (fully-parse (day4-lines))
         [c & cs]       calls
         bs             boards]
    (if c
      (let [updated (map #(call-number % c) bs)
            win     (find-win updated)]
        (if win
          (get-score c win)
          (recur nil cs updated)))
      (throw (IllegalStateException. "Ran out of calls.")))))

(defn part2 []
  (loop [[calls boards] (fully-parse (day4-lines))
         [c & cs]       calls
         bs             boards
         last-wins      nil
         last-c         nil]
    (if (and c (-> bs (empty?) (not)))
      (let [updated (map #(call-number % c) bs)
            wins    (filter check-win updated)]
        (if (-> wins (empty?) (not))
          (recur nil cs (remove check-win updated) wins c)
          (recur nil cs updated last-wins last-c)))
      (get-score last-c (first last-wins)))))
