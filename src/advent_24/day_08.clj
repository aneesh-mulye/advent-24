(ns advent-24.day-08
  (:require [clojure.string :as str]))

(defn parse-d08-input [fname]
  (mapv vec (str/split-lines (slurp fname))))

(defn is-antinode? [antmap linenum colnum]
  (some
    identity
    (for [l (range (count antmap))
          c (range (count (first antmap)))]
      (let [char-at (get-in antmap [l c])
            [ldist cdist] [(- l linenum) (- c colnum)]
            [corres-l corres-c] [(+ linenum (* 2 ldist)) (+ colnum (* 2 cdist))]
            char-at-opp (get-in antmap [corres-l corres-c])]
            ;_ (println l c char-at ldist cdist corres-l corres-c char-at-opp)]
        (cond
          (= char-at \.) false
          (= [l c] [linenum colnum]) false
          :else (= char-at char-at-opp))))))

(defn d08-1 [antmap]
  (->> (for [l (range (count antmap))
             c (range (count (first antmap)))]
         (is-antinode? antmap l c))
       (filter identity)
       (count)))

(defn antenna-positions [antmap]
  (loop [coords (for [l (range (count antmap))
                      c (range (count (first antmap)))]
                  [l c])
         antposes {}]
    (let [[[l c] & r] coords
          char-at (get-in antmap [l c])]
      (cond
        (nil? char-at) antposes
        (= \. char-at) (recur r antposes)
        :else (recur r (update
                         antposes char-at
                         (fnil (fn [s] (conj s [l c])) #{})))))))

(defn antinodes-this-way [[l1 c1] [l2 c2] antmap]
  (let [ldiff (- l2 l1)
        cdiff (- c2 c1)]
    (take-while
      #(not= nil (get-in antmap %))
      (iterate (fn [[l c]] [(+ l ldiff) (+ c cdiff)]) [l2 c2]))))

(defn d08-2 [antmap]
  (let [position-groups (vals (antenna-positions antmap))]
    (->> (for [pgroup position-groups
               p1 pgroup
               p2 pgroup
               :when (not= p1 p2)]
           (antinodes-this-way p1 p2 antmap))
         (reduce into #{})
         (count))))
