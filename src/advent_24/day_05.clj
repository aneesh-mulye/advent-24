(ns advent-24.day-05
  (:require [clojure.string :as str]))

(defn parse-p5-input [fname]
  (let [[pord page-orders] (str/split (slurp fname) #"\R\R")
        pord (->> (str/split-lines pord)
                  (map #(str/split % #"\|"))
                  (map (fn [[f s]] [(parse-long f) (parse-long s)]))
                  (reduce
                    (fn [pord-sofar [k v]]
                      (update
                        pord-sofar k
                        (fnil (fn [afterset] (conj afterset v)) #{}))) 
                    {}))
        page-orders (->> (str/split-lines page-orders)
                         (map #(str/split % #","))
                         (mapv (fn [sq] (mapv parse-long sq))))]
    [page-orders pord]))

(defn page-in-order? [page afters pord]
  (loop [remaining afters]
    (cond
      (not (seq remaining)) true
      (nil? (pord (first remaining))) (recur (rest remaining))
      ((pord (first remaining)) page) false
      :else (recur (rest remaining)))))

(defn pages-ordered? [pages pord]
  (loop [[f & r] pages]
    (cond
      (not (seq r)) true
      (not (page-in-order? f r pord)) false
      :else (recur r))))

(defn p5-1 [page-orders pord]
  (->> page-orders
       (map (fn [pages]
              (if (pages-ordered? pages pord)
                (get pages (quot (count pages) 2))
                0)))
       (reduce +)))

(defn p5-2 [page-orders pord]
  (let [page-comp (fn [p1 p2] (if (and (pord p1) ((pord p1) p2)) true false))]
    (->> page-orders
         (map (fn pages-to-midpage [pages]
                (if (pages-ordered? pages pord)
                  0
                  (get (vec (sort page-comp pages)) (quot (count pages) 2)))))
         (reduce +))))
