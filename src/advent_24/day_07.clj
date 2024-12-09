(ns advent-24.day-07
  (:require [clojure.string :as str]))

(defn parse-d07-input [fname]
  (let [lines (str/split-lines (slurp fname))]
    (mapv (fn [line]
            (let [[target-s nums-s] (str/split line #": ")]
              [(parse-long target-s)
               (mapv #(parse-long %) (str/split nums-s #" "))]))
          lines)))

(defn satisfiable-inner? [target nums sofar]
  (cond
    (and (empty? nums) (= target sofar)) target
    (and (empty? nums) (not= target sofar)) nil
    :else
    (or
      (satisfiable-inner? target (rest nums) (+ sofar (first nums)))
      (satisfiable-inner? target (rest nums) (* sofar (first nums))))))

(defn satisfiable? [target nums]
  (satisfiable-inner? target (rest nums) (first nums)))

(defn d07-1 [prob-input]
  (->> prob-input
       (map #(apply satisfiable? %))
       (filter identity)
       (reduce +)))

(defn satisfiable-inner-2? [target sofar nums]
  (cond
    (and (empty? nums) (= target sofar)) target
    (and (empty? nums) (not= target sofar)) nil
    :else
    (let [[f & r] nums]
      (or
        (satisfiable-inner-2? target (+ sofar f) r)
        (satisfiable-inner-2? target (* sofar f) r)
        (satisfiable-inner-2? target (parse-long (str sofar f)) r)))))

(defn satisfiable-2? [target nums]
  (satisfiable-inner-2? target (first nums) (rest nums)))

(defn d07-2 [prob-input]
  (->> prob-input
       (map #(apply satisfiable-2? %))
       (filter identity)
       (reduce +)))
