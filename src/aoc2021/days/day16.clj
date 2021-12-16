(ns aoc2021.days.day16
  (:require [aoc2021.util.input :as inp]))

(def h2b-table
  {\0 "0000" \1 "0001" \2 "0010" \3 "0011"
   \4 "0100" \5 "0101" \6 "0110" \7 "0111"
   \8 "1000" \9 "1001" \A "1010" \B "1011"
   \C "1100" \D "1101" \E "1110" \F "1111"})

(defn h2b [hex-str]
  (->> hex-str
       (reduce #(.append %1 (h2b-table %2)) (StringBuilder.))
       (.toString)))

(def day16-bin-str
  (->> "day16"
       (inp/get-input-lines)
       (first)
       (h2b)))

(defn b2d [bin-arr]
  (Long/parseLong (apply str bin-arr) 2))

(declare parse-packet)

(defn parse-packets [bits]
  (loop [bs  bits
         acc []]
    (if (< (count bs) 11)
      acc
      (let [[pkt rest] (parse-packet bs)]
        (recur rest (conj acc pkt))))))

(defn parse-packet [bits]
  (let [packet         (atom {})
        [version rest] (split-at 3 bits)
        [type-id rest] (split-at 3 rest)]
    (swap! packet assoc :Version (b2d version))
    (swap! packet assoc :Type (b2d type-id))
    (if (= (@packet :Type) 4)
      (loop [[[c? & four] left] (split-at 5 rest)
             so-far      (StringBuilder.)]
        (.append so-far (apply str four))
        (case c?
          \0 (do
               (swap! packet assoc :Literal (b2d so-far))
               (list @packet left))
          \1 (recur (split-at 5 left) so-far)))
      (let [[[ltid] left] (split-at 1 rest)]
        (swap! packet assoc :Length-Type (b2d [ltid]))
        (case ltid
          \0 (let [[len left]  (split-at 15 left)
                   [bits left] (split-at (b2d len) left)
                   sub-pkts    (parse-packets bits)]
               (swap! packet assoc :Sub-Packets sub-pkts)
               (list @packet left))
          \1 (let [[num left] (split-at 11 left)
                   num-pkts   (b2d num)
                   sub-pkts   (atom [])]
               (loop [i      num-pkts
                      l      left
                      parsed (lazy-seq (parse-packet l))]
                 (if (<= i 0)
                   (do
                     (swap! packet assoc :Sub-Packets @sub-pkts)
                     (list @packet l))
                   (do
                     (swap! sub-pkts conj (first parsed))
                     (recur (dec i) (second parsed) (lazy-seq (parse-packet (second parsed)))))))))))))

(defn version-sum [pkt]
  (let [v (pkt :Version)]
    (if (contains? pkt :Sub-Packets)
      (+ v (reduce #(+ %1 (version-sum %2)) 0 (pkt :Sub-Packets)))
      v)))

(defn calculate [pkt]
  (let [spks (lazy-seq (pkt :Sub-Packets))]
    (case (pkt :Type)
      0 (reduce #(+ %1 (calculate %2)) 0 spks)
      1 (reduce #(* %1 (calculate %2)) 1 spks)
      2 (reduce #(min %1 (calculate %2)) Long/MAX_VALUE spks)
      3 (reduce #(max %1 (calculate %2)) Long/MIN_VALUE spks)
      4 (pkt :Literal)
      5 (if (> (calculate (first spks)) (calculate (second spks))) 1 0)
      6 (if (< (calculate (first spks)) (calculate (second spks))) 1 0)
      7 (if (= (calculate (first spks)) (calculate (second spks))) 1 0))))

(defn part1 []
  (->> day16-bin-str
       (parse-packets)
       (first)
       (version-sum)))

(defn part2 []
  (->> day16-bin-str
       (parse-packets)
       (first)
       (calculate)))

