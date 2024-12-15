(ns advent-24.day-14
  (:require [clojure.string :as str]))

;; Returns: [x y dx dy]
(defn parse-robot [rs]
  (->> (re-matches #"p=(.*),(.*) v=(.*),(.*)" rs)
       (rest)
       (mapv #(parse-long %))))

(defn empty-grid [gx gy]
  (vec (repeat gy
               (vec (repeat gx \space)))))

(defn put-into [grid robot]
  (let [[x y dx dy] robot]
    (update-in grid [y x] conj [dx dy])))

(defn parse-d14-input [fname]
  (->> fname
       (slurp)
       (str/split-lines)
       (map parse-robot)))

(defn step-n [robots n gx gy]
  (->> robots
       (mapv
         (fn [[x y dx dy]]
           [(mod (+ x (* dx n)) gx) (mod (+ y (* dy n)) gy) dx dy]))))

(defn safety-factor [robots gx gy]
  (let [mx (long (/ gx 2))
        my (long (/ gy 2))]
    (->> robots
         (reduce
           (fn [[tl tr bl br] [x y _ _]]
             (cond
               (and (< x mx) (< y my)) [(inc tl) tr bl br]
               (and (< x mx) (> y my)) [tl tr (inc bl) br]
               (and (> x mx) (< y my)) [tl (inc tr) bl br]
               (and (> x mx) (> y my)) [tl tr bl (inc br)]
               :else [tl tr bl br]))
           [0 0 0 0])
         (reduce *))))

(defn render [robots gx gy]
  (->> robots
       (reduce
         (fn [grid [x y _ _]]
           (assoc-in grid [y x] \*))
         (empty-grid gx gy))))

(defn print-render [r]
  (doseq [l r]
    (println (apply str l))))

(defn has-long-*** [r]
  (->> (for [l r]
                   (re-matches #".*\*{10}.*" (apply str l)))
       (some identity)
       (boolean)))

(defn find-christmas-tree [robots gx gy]
  (->> (for [n (range)]
         [n (render (step-n robots n gx gy) gx gy)])
       (take-while (comp not has-long-*** second))
       (last)
       (first)
       (inc)))


(comment
  (parse-robot "p=0,4 v=3,-3")
  (def s00 (parse-d14-input "resources/sample_14_00"))
  (print-render (render s00 11 7))
  (def i (parse-d14-input "resources/input_14"))
  (safety-factor (step-n s00 100 11 7) 11 7)
  (safety-factor (step-n i 100 101 103) 101 103)
  (find-christmas-tree i 101 103)
  )
