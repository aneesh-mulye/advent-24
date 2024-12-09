(ns advent-24.day-02
  (:require [clojure.string :as str]))

(defn read-many-nums-in-str [s]
  (->> (str/split s #"\s+")
       (mapv #(parse-long %))))

(defn parse-p2-input [fname]
  (->> (slurp fname)
       (str/split-lines)
       (mapv read-many-nums-in-str)))

(defn safe-report? [report]
  (let [diffs (map - report (rest report))
        diffmuls (map * diffs (rest diffs))]
    (cond
      (some #(> (abs %) 3) diffs) false
      (some #(<= % 0) diffmuls) false
      :else true)))

;; Let me introduce you to my friend, Brute Force.
(defn safe-report-2? [report]
  (->> (for [exclude (range (count report))]
         (into (subvec report 0 exclude) (subvec report (inc exclude))))
       (map safe-report?)
       (some true?)
       (or (safe-report? report))))

(defn p2-1 [fname]
  (let [input (parse-p2-input fname)]
    (->> input
         (map safe-report-2?)
         (filter true?)
         count)))
