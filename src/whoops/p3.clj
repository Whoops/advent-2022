(ns whoops.p3
  (:require [whoops.utils :as utils]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [per-pocket (/ (count line) 2)
        left (into #{} (subs line 0 per-pocket))
        right (into #{} (subs line per-pocket))]
    [left right]))

(def day3-data
  (->> "day3.txt"
       utils/file-lines
       (map parse-line)))

(defn char->priority [char]
  (if (Character/isLowerCase char)
    (- (Character/digit char 36) 9)
    (+ (Character/digit char 36) 17)))

(defn p3-1 []
  (->> day3-data
       (map #(apply set/intersection %))
       (map first)
       (map char->priority)
       (reduce +)))

(defn p3-2 []
  (->> day3-data
       (map #(apply set/union %))
       (partition 3)
       (map #(apply set/intersection %))
       (map first)
       (map char->priority)
       (reduce +)))


(utils/register-day 3 [p3-1 p3-2])
