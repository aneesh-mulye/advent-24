(ns advent-24.day-10
  (:require [clojure.string :as str]))

(defn parse-d10-input [fname]
  (->> (str/split-lines (slurp fname))
       (mapv #(mapv (fn [c] (parse-long (str c))) %))))

(defn adjacent-downs [m [l c]]
  (let [adjs [[(inc l) c] [(dec l) c] [l (inc c)] [l (dec c)]]
        elevation (get-in m [l c])]
    (->> adjs
         (filter #(= (dec elevation) (get-in m %)))
         (set))))

(defn trailheads-to [m [l c]]
  (loop [curr (get-in m [l c])
         froms #{[l c]}]
    (if (= curr 0) froms
      (recur (dec curr)
             (->> froms
                  (map #(adjacent-downs m %))
                  (reduce into #{}))))))

(defn d10-1 [m]
  (->> (for [l (range (count m))
             c (range (count (first m)))]
         (when (= (get-in m [l c]) 9) (trailheads-to m [l c])))
       (reduce into [])
       (count)))

(defn adjacent-downs-2 [m [[l c] arity]]
  (let [pot-adjs [[(inc l) c] [(dec l) c] [l (inc c)] [l (dec c)]]
        elevation (get-in m [l c])]
    (->> pot-adjs
         (filter #(= (dec elevation) (get-in m %)))
         (reduce
           (fn [adjs [l c]]
             (assoc adjs [l c] arity))
           {}))))

(defn trailheads-to-2 [m [l c]]
  (loop [curr (get-in m [l c])
         froms {[l c] 1}]
    (if (= curr 0) froms
      (recur (dec curr)
             (->> froms
                  (map #(adjacent-downs-2 m %))
                  (reduce into [])
                  (reduce
                    (fn [acc [[l c] arity]]
                      (update-in acc [[l c]] (fnil #(+ % arity) 0)))
                    {}))))))

(defn d10-2 [m]
  (->> (for [l (range (count m))
             c (range (count (first m)))]
         (when (= (get-in m [l c]) 9) (trailheads-to-2 m [l c])))
       (apply merge-with +)
       (vals)
       (reduce +)))
