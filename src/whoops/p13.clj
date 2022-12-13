(ns whoops.p13
  (:require [whoops.utils :as utils]
            [clojure.edn :as edn]))


(def day13-data
  (->> "day13.txt"
       utils/file-lines
       (partition-by #(= "" %))
       (remove #(= '("") %))
       (map #(map edn/read-string %))))

(declare compare-packet)

(defn compare-seq [left right]
  (loop [left left
         right right]
    (let [l (first left)
          r (first right)
          result (compare-packet l r)
          left (seq (rest left))
          right (seq (rest right))]
      (if (or (not= 0 result) (and (nil? left) (nil? right)))
        result
        (recur left right)))))

(defn compare-packet [left right]
  (cond
    (and (nil? left) (nil? right)) 0
    (nil? left) -1
    (nil? right) 1
    (and (number? left) (number? right)) (compare left right)
    (and (number? left) (sequential? right)) (compare-seq [left] right)
    (and (sequential? left) (number? right)) (compare-seq left [right])
    (and (sequential? left) (sequential? right)) (compare-seq left right)
    :else (throw (IllegalArgumentException. "Unknown types provided"))))

(defn p1 []
  (->> day13-data
       (keep-indexed (fn [idx [l r]]
                       (let [result (compare-packet l r)]
                         (when (or (zero? result) (= -1 result))
                           (inc idx)))))
       (reduce +)))

(defn p2 []
  (->> day13-data
       (concat [[[[2]] [[6]]]])
       (apply concat)
       (sort compare-packet)
       (keep-indexed (fn [idx packet]
                       (when (or (= packet [[2]]) (= packet [[6]]))
                         (inc idx))))
       (apply *)))

(utils/register-day 13 [p1 p2])
