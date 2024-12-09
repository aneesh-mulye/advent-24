(ns advent-24.day-03)

(defn p2-1 [fname]
  (let [f (slurp fname)
        muls (re-seq #"mul\((\d+),(\d+)\)" f)]
    (reduce
      (fn [sum [_ s1 s2]]
        (+ sum (* (parse-long s1) (parse-long s2))))
      0
      muls)))

(defn p2-2 [fname]
  (let [f (slurp fname)
        muls (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" f)]
    (reduce
      (fn [[do? sum] [p s1 s2]]
        (case p
          "do()" [true sum]
          "don't()" [false sum]
          [do? (+ sum
                  (if do? (* (parse-long s1) (parse-long s2)) 0))]))
      [true 0]
      muls)))
