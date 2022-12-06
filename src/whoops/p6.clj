(ns whoops.p6
  (:require [whoops.utils :as utils]))

(def day6-data
  (-> "day6.txt"
      (utils/file-lines)
      first))

(defn find-marker [len data]
  (->> data
       (partition len 1)
       (map set)
       (take-while #(not= len (count %)))
       count
       (+ len)))

(defn p1 []
  (find-marker 4 day6-data))

(defn p2 []
  (find-marker 14 day6-data))

(utils/register-day 6 [p1 p2])
