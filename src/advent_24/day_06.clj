(ns advent-24.day-06
  (:require [clojure.string :as str]))

(defn parse-d06-input [fname]
  (mapv vec (str/split-lines (slurp fname))))

(def guard
  {\< :left
   \^ :up
   \> :right
   \v :down})

(def turn-right
  {:up :right
   :right :down
   :down :left
   :left :up})

(defn guard-initial [board]
  (let [[l c]
        (some identity
              (for [l (range (count board))
                    c (range (count (first board)))]
                (if (guard (get-in board [l c])) [l c] nil)))
        orientation (guard (get-in board [l c]))]
    [l c orientation]))

(defn lc-next [l c dir]
  (case dir
    :up [(dec l) c]
    :down [(inc l) c]
    :left [l (dec c)]
    :right [l (inc c)]))

(defn d06-1 [board]
  (let [[l c dir] (guard-initial board)]
    (loop [[l c] [l c]
           dir dir
           board board]
      (let [[ln cn] (lc-next l c dir)
            char-next (get-in board [ln cn])
            board-next (assoc-in board [l c] \X)]
        (case char-next
          nil [board-next ((frequencies (flatten board-next)) \X)]
          \# (recur [l c] (turn-right dir) board-next)
          (recur [ln cn] dir board-next))))))

(defn loops? [board]
  (let [[l c dir] (guard-initial board)]
    (loop [[l c] [l c]
           dir dir
           trace #{}]
      (let [[ln cn] (lc-next l c dir)
            char-next (get-in board [ln cn])]
        (cond
          (trace [l c dir]) true
          :else
          (case char-next
            nil false
            \# (recur [l c] (turn-right dir) (conj trace [l c dir]))
            (recur [ln cn] dir (conj trace [l c dir]))))))))

(defn blockables [board]
  (let [final-board (first (d06-1 board))
        [l-init c-init _] (guard-initial board)
        blockables-prov
        (->> (for [l (range (count board))
                   c (range (count (first board)))]
               (if (= \X (get-in final-board [l c])) [l c] false))
             (into #{}))]
    (disj blockables-prov false [l-init c-init])))

(defn d06-2 [board]
  (->> (blockables board)
       (map #(assoc-in board % \#))
       (map loops?)
       (filter identity)
       (count)))
