(ns advent-24.day-11
  (:require [clojure.string :as str]))

(defn parse-d11-input [fname]
  (->> (str/split (first (str/split-lines (slurp fname))) #"\s+")
       (mapv #(parse-long %))))

(defn d11-1-blink [stones]
  (reduce
    (fn [v stone]
      (cond
        (= 0 stone) (conj v 1)

        (even? (count (str stone)))
        (let [len (/ (count (str stone)) 2)
              sstone (str stone)
              l (parse-long (subs sstone 0 len))
              r (parse-long (subs sstone len))]
          (conj v l r))

        :else (conj v (* 2024 stone))))
    []
    stones))

(defn d11-2-blink [stone-freqs]
  (reduce
    (fn [m [stone freq]]
      (cond
        (= 0 stone) (update m 1 (fnil #(+ % freq) 0))

        (even? (count (str stone)))
        (let [len (/ (count (str stone)) 2)
              sstone (str stone)
              l (parse-long (subs sstone 0 len))
              r (parse-long (subs sstone len))]
          (-> m
              (update l (fnil #(+ % freq) 0))
              (update r (fnil #(+ % freq) 0))))

        :else (update m (* 2024 stone) (fnil #(+ % freq) 0))))
    {}
    stone-freqs))

(comment
  ;; Parsing input
  (def i (parse-d11-input "resources/input_11"))
  ;; Part 1
  (->> i
       (iterate d11-1-blink)
       (take 26)
       (last)
       (count))
  ;; Part 2
  (->> (frequencies i)
       (iterate d11-2-blink)
       (take 76)
       (last)
       (vals)
       (reduce +))
  )
