(ns advent-24.day-12
  (:require [clojure.string :as str]
            [clojure.set]))

(defn parse-d12-input [fname]
  (->> fname
       (slurp)
       (str/split-lines)
       (mapv vec)))

(defn adjacents [m [l c]]
  (let [char-at (get-in m [l c])
        left [l (dec c)]
        right [l (inc c)]
        up [(dec l) c]
        down [(inc l) c]]
    (filter #(= char-at (get-in m %)) [left right up down])))

(defn region-points [m start]
  (loop [seen #{}
         q #{start}]
    (if (empty? q) seen
      (let [p (first q)
            new-adjs (filter #(not (seen %)) (adjacents m p))]
        (recur (conj seen p)
               (-> q
                   (disj p)
                   (into new-adjs)))))))

(defn region-fence-cost-1 [m reg-pts]
  (let [reg-area (count reg-pts)
        reg-fences (->> reg-pts
                        (map #(- 4 (count (adjacents m %))))
                        (reduce +))]
    (* reg-area reg-fences)))

(defn horiz-sides-for-line [m reg-pts l]
  (->> (for [c (range (count (first m)))]
         [(boolean (reg-pts [l c])) (boolean (reg-pts [(inc l) c]))])
       (reduce
         (fn [[fences [old-mine old-lower]] [mine lower]]
           (cond
             (or
               (and (= [old-mine old-lower] [false false])
                    (= [mine lower] [false true]))
               (and (= [old-mine old-lower] [false false])
                    (= [mine lower] [true false]))
               (and (= [old-mine old-lower] [true false])
                    (= [mine lower] [false true]))
               (and (= [old-mine old-lower] [false true])
                    (= [mine lower] [true false]))
               (and (= [old-mine old-lower] [true true])
                    (= [mine lower] [true false]))
               (and (= [old-mine old-lower] [true true])
                    (= [mine lower] [false true])))
             [(inc fences) [mine lower]]
             :else [fences [mine lower]]))
         [0 [false false]])
       (first)))


(defn horiz-region-sides-2 [m reg-pts]
  (->> (range -1 (count m))
       (map #(horiz-sides-for-line m reg-pts %))
       (reduce +)))

(defn region-fence-cost-2 [m reg-pts]
  (let [sides 
        (+ (horiz-region-sides-2 m reg-pts)
           (horiz-region-sides-2 (vec (apply map vector m))
                                 (->> reg-pts
                                      (map reverse)
                                      (into #{}))))]
    (* sides (count reg-pts))))

(defn d12 [m fence-cost-fun]
  (let [points (into #{}
                     (for [l (range (count m))
                           c (range (count (first m)))]
                       [l c]))]
    (loop [pts-rem points
           total-fence-cost 0]
      (if (empty? pts-rem) total-fence-cost
        (let [reg-pts (region-points m (first pts-rem))]
          (recur (clojure.set/difference pts-rem reg-pts)
                 (+ total-fence-cost (fence-cost-fun m reg-pts))))))))

(comment
  (def i (advent-24.day-12/parse-d12-input "resources/input_12"))
  (def s00 (parse-d12-input "resources/sample_12_00"))
  (def s01 (parse-d12-input "resources/sample_12_01"))
  (def s02 (parse-d12-input "resources/sample_12_02"))
  (d12 s02 region-fence-cost-1)
  (d12 i region-fence-cost-1)
  (d12 i region-fence-cost-2)
  (d12 s00 region-fence-cost-2)

  (def s00A (region-points s00 [0 0]))
  (def s00B (region-points s00 [1 0]))
  (def s00C (region-points s00 [1 2]))
  (horiz-region-sides-2 s00 s00A)
  (horiz-region-sides-2 s00 s00C)

  ) 
