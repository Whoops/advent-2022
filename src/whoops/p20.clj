(ns whoops.p20
  (:require
   [whoops.utils :as utils]))

(def day20-data
  (->> "day20.txt"
       utils/file-lines
       (mapv parse-long)))

(defn remap [data idx-map]
  (reduce (fn [v src-idx]
            (let [dest-idx (nth idx-map src-idx)]
              (assoc v dest-idx (nth data src-idx))))
          data
          (range (count data))))

(defn move-item [idx-map idx data]
  (let [current-idx (nth idx-map idx)
        shifted (-> current-idx (+ (nth data idx)) (mod (dec (count data))))
        new-map (->> idx-map
                     (mapv #(if (> % (nth idx-map idx))
                              (dec %)
                              %))
                     (mapv #(if (>= % shifted)
                              (inc %)
                              %)))
        new-map (assoc new-map idx shifted)]
    new-map))

(defn mix [data n]
  (let [idx-map (vec (range (count data)))
        idx-maps (iterate
                  (fn [idx-map]
                    (reduce #(move-item %1 %2 data)
                            idx-map (range (count data))))
                  idx-map)
        idx-map (nth idx-maps n)]
    (remap data idx-map)))

(defn find-nth [data n]
  (let [zero-pos (first (keep-indexed #(when (zero? %2) %1) data))
        idx (mod (+ zero-pos n) (count data))]
    (nth data idx)))

(defn p1 []
  (let [mixed (mix day20-data 1)]
    (+ (find-nth mixed 1000) (find-nth mixed 2000) (find-nth mixed 3000))))

(defn p2 []
  (let [data (mapv #(* 811589153 %) day20-data)
        mixed (mix data 10)]
    (+ (find-nth mixed 1000) (find-nth mixed 2000) (find-nth mixed 3000))))

(utils/register-day 20 [p1 p2])
