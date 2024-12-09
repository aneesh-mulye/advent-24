(ns advent-24.day-04
  (:require [clojure.string :as str]))


(defn xmases [l c lines]
  (let [possibs [(for [a (range 4)] (get-in lines [(+ l a) c]))
                 (for [a (range 4)] (get-in lines [(- l a) c]))
                 (for [a (range 4)] (get-in lines [l (+ c a)]))
                 (for [a (range 4)] (get-in lines [l (- c a)]))
                 (for [a (range 4)] (get-in lines [(+ l a) (+ c a)]))
                 (for [a (range 4)] (get-in lines [(+ l a) (- c a)]))
                 (for [a (range 4)] (get-in lines [(- l a) (+ c a)]))
                 (for [a (range 4)] (get-in lines [(- l a) (- c a)]))]]
    (count (filter #(= [\X \M \A \S] %) possibs))))

(defn x-mas?-01 [l c lines]
  (if (#{[\M \S \A \M \S]
         [\S \S \A \M \M]
         [\M \M \A \S \S]
         [\S \M \A \S \M]} [(get-in lines [(dec l) (dec c)])
                            (get-in lines [(dec l) (inc c)])
                            (get-in lines [l c])
                            (get-in lines [(inc l) (dec c)])
                            (get-in lines [(inc l) (inc c)])])
    1
    0))

(x-mas?-01 1 1 ["M.S" ".A." "M.S"])

;; Takes input, generates horizontal, vertical, and the two diagonal
;; orders which can be searched for the word in both forward and reverse
;; order.
(defn day04-01 [fname]
  (let [lines (str/split-lines (slurp fname))]
    (->> (for [l (range (count lines))
               c (range (count (first lines)))]
           (xmases l c lines))
         (reduce +))))

(defn day04-02 [fname]
  (let [lines (str/split-lines (slurp fname))]
    (->> (for [l (range (count lines))
               c (range (count (first lines)))]
           (x-mas?-01 l c lines))
         (reduce +))))
