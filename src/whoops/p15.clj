(ns whoops.p15
  (:require
   [whoops.utils :as utils]))

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn extract-sensor-data [[[sx sy] [bx by]]]
  (let [radius (manhattan [sx sy] [bx by])]
    {:sensor [sx sy]
     :beacon [bx by]
     :radius radius}))

(def day15-data
  (->> "day15.txt"
       utils/file-lines
       (map #(re-seq #"-?\d+" %))
       (map #(map parse-long %))
       (map #(partition 2 %))
       (mapv extract-sensor-data)))

(defn covers? [sensor row]
  (let [[_ sy] (:sensor sensor)
        dist (abs (- sy row))]
    (>= (:radius sensor) dist)))


(defn row-coverage [sensor row]
  (when (covers? sensor row)
    (let [[sx sy] (:sensor sensor)
          diff (- (:radius sensor) (abs (- row sy)))]
      [(- sx diff) (+ sx diff)])))

(defn adjust-vector [[b1 b2] [n1 n2]]
  (let [contains-bottom? (<= b1 n1 b2)
        contains-top? (>= b2 n2 b1)
        bottom (if contains-bottom? (inc b2) n1)
        top (if contains-top? (dec b1) n2)]
    (when (<= bottom top)
        [bottom top])))

(defn add-vector [vectors new-vec]
  (loop [new-vec new-vec
         process-vecs vectors
         processed-vecs '()]
          (let [old-vec (first process-vecs)]
            (cond
              (not new-vec) (concat processed-vecs process-vecs)
              (not old-vec) (conj processed-vecs new-vec)
              (not (adjust-vector new-vec old-vec)) (recur new-vec (rest process-vecs) processed-vecs)
              :else (recur (adjust-vector old-vec new-vec) (rest process-vecs) (conj processed-vecs old-vec))))))

(defn flatten-vectors [vectors]
  (reduce (fn [vectors [n1 n2]]
            (let [[v1 v2] (first vectors)]
              (cond
                (not v1) (conj vectors [n1 n2])
                (= (inc v2) n1) (conj (rest vectors) [v1 n2])
                :else (conj vectors [n1 n2]))))
          '()
          (sort vectors)))

(defn find-coverage [row sensors]
  (->> sensors
       (map #(row-coverage %1 row))
       (remove nil?)
       (reduce add-vector '())
       flatten-vectors
       sort))

(defn vec-size [[bottom top]]
  (inc (- top bottom)))

(defn beacon-count [row sensors]
  (count
   (into #{}
         (keep (fn [{:keys [beacon]}]
                 (let [[_ y] beacon]
                   (when (= y row)
                     beacon)))
               sensors))))

(defn p1 []
  (let [coverage-size (->> day15-data (find-coverage 2000000) (map vec-size) (reduce +))
        beacons (beacon-count 2000000 day15-data)]
    (- coverage-size beacons)))

(defn to-index [[row-gap]]
  (let [[[_ x] _] (second row-gap)]
    [(inc x) (first row-gap)]))

(defn tuning-freq [[x y]]
  (+ y (* 4000000 x)))

(defn p2 []
  (->> (range 0 (inc 4000000))
       (map-indexed #(vector %1 (find-coverage %2 day15-data)))
       (filter #(< 1 (count (second %))))
       to-index
       tuning-freq))

(utils/register-day 15 [p1 p2])
