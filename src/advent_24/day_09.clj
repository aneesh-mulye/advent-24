(ns advent-24.day-09
  (:require [clojure.string :as str]))

(defn parse-d09-input-1 [fname]
  (let [fs (->> (str/trim (slurp fname))
                (map #(parse-long (str %))))
        files (->> (vec (take-nth 2 fs))
                   (reduce-kv (fn [fv ind size]
                                (conj fv (repeat size ind)))
                              [])
                   (vec))
        files (conj files [])
        freespaces (->> (take-nth 2 (rest fs))
                        (map (fn [size] (repeat size \.)))
                        (vec))
        freespaces (conj freespaces [])]
    (->> (interleave files freespaces)
         (flatten)
         (vec))))

(defn compact-1 [fs]
  (loop [i 0
         j (dec (count fs))
         cfs fs]
    (if (>= i j) cfs
      (let [ci (cfs i)
            cj (cfs j)]
        (cond
          (= cj \.) (recur i (dec j) cfs)
          (not= ci \.) (recur (inc i) j cfs)
          :else (recur (inc i) (dec j)
                       (-> cfs
                           (assoc i cj)
                           (assoc j ci))))))))

(defn checksum-1 [cfs]
  (->> (take-while #(not= \. %) cfs)
       (vec)
       (reduce-kv
         (fn [acc ind fid]
           (+ acc (* ind fid)))
         0)))

(defn d09-1 [fs]
  (->> fs
       (compact-1)
       (checksum-1)))

(defn parse-d09-input-2 [fname]
  (let [fs (->> (str/trim (slurp fname))
                (map #(parse-long (str %))))
        files (->> (vec (take-nth 2 fs))
                   (reduce-kv (fn [fv ind size]
                                (conj fv {:chunk :file
                                          :index ind
                                          :size size}))
                              [])
                   (vec))
        freespaces (->> (take-nth 2 (rest fs))
                        (map (fn [size] {:chunk :freespace
                                         :size size}))
                        (vec))
        fsp (->> (interleave files freespaces)
                 (filter #(not= 0 (:size %)))
                 (vec))]
    (if (odd? (count fs))
      (conj fsp (peek files))
      fsp)))

(defn first-fitting-fspace-index [v size]
  (some
    identity
    (for [i (range (count v))]
      (let [c (v i)]
        (when (and (= :freespace (:chunk c))
                   (<= size (:size c)))
          i)))))

(defn move-into [v fsi c]
  (let [before (take fsi v)
        added (let [csize (:size c)
                    ssize (:size (v fsi))]
                (if (= csize ssize)
                  [c]
                  [c {:chunk :freespace
                      :size (- ssize csize)}]))
        after (drop (inc fsi) v)]
    (vec (concat before added after))))

(defn compact-2 [fs]
  (loop [remaining fs
         after ()]
    (let [l (peek remaining)]
      (cond
        (empty? remaining) after

        (= :freespace (:chunk l))
        (recur (subvec remaining 0 (dec (count remaining)))
               (conj after l))

        :else
        (if-let [fsi (first-fitting-fspace-index remaining (:size l))]
          (recur 
            (move-into (subvec remaining 0 (dec (count remaining))) fsi l)
            (conj after {:chunk :freespace :size (:size l)}))
          (recur (subvec remaining 0 (dec (count remaining)))
                 (conj after l)))))))

(defn explode-2 [fs]
  (->> fs
       (map (fn [c]
                (repeat
                  (:size c)
                  (case (:chunk c)
                    :file (:index c)
                    :freespace \.))))
       (flatten)))

(defn checksum-2 [fs]
  (->> fs
       (vec)
       (reduce-kv
         (fn [acc ind fid]
           (+' acc (if (= fid \.) 0 (* ind fid))))
         0)))

(defn d09-2 [fs]
  (->> fs
       (compact-2)
       (explode-2)
       (checksum-2)))
