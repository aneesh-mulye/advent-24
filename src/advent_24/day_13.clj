(ns advent-24.day-13
  (:require [clojure.string :as str]))

(defn parse-machine [s]
  (->> s
       (str/trim)
       (re-matches #"Button A: X\+(\d+), Y\+(\d+)\RButton B: X\+(\d+), Y\+(\d+)\RPrize: X=(\d+), Y=(\d+)")
       (rest)
       (mapv #(parse-long %))))

(defn parse-d12-input [fname]
  (->> (str/split (slurp fname) #"\R\R")
       (mapv parse-machine)))

(defn determinant [x1 y1 x2 y2]
  (- (* x1 y2) (* x2 y1)))

;; Turns out the input is *way* more constrained than the description implies,
;; otherwise I wouldn't have been able to do this. WTF.
(defn machine-req-spend [machine offset]
  (let [[x1 y1 x2 y2 xX yY] machine
        xX (+ xX offset)
        yY (+ yY offset)
        det (determinant x1 x2 y1 y2)
        a-num (determinant xX yY x2 y2)
        a (/ a-num det)
        b-num (determinant x1 y1 xX yY)
        b (/ b-num det)]
    (if (and (pos-int? a) (pos-int? b))
      (+ a a a b)
      0)))

(defn d13 [machines offset]
  (->> machines
       (map #(machine-req-spend % offset))
       (reduce +)))


(comment
  (def i (parse-d12-input "resources/input_13"))
  (d13 i 0)
  (d13 i 10000000000000)

  )
