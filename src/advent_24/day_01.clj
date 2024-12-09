(ns advent-24.day-01
  (:require [clojure.string :as str]))

(defn read-many-nums-in-str [s]
  (->> (str/split s #"\s+")
       (mapv #(parse-long %))))

(defn parse-prob01-input [fname]
  (let [lines (str/split-lines (slurp fname))
        numpairs (mapv read-many-nums-in-str lines)
        firsts (sort (map first numpairs))
        seconds (sort (map second numpairs))]
    [firsts seconds]))

(defn dist [firsts seconds]
  (->> (map (fn [f s] (abs (- f s))) firsts seconds)
       (reduce +)))

(defn similarity [firsts seconds]
  (let [secfreqs (frequencies seconds)]
    (->> firsts
         (map (fn [n] (* n (or (secfreqs n) 0))))
         (reduce +))))
