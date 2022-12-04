(ns whoops.p4
  (:require
   [whoops.utils :as utils]
   [clojure.string :as string]))

(defn parse-range [range]
  (let [delims (string/split range #"-")]
    (map parse-long delims)))

(defn parse-line [line]
  (let [pairs (string/split line #",")]
    (map parse-range pairs)))

(def day4-data
  (->> "day4.txt"
       (utils/file-lines)
       (map parse-line)))

(defn eclipses? [[s1 e1] [s2 e2]]
  (and (<= s1 s2) (>= e1 e2)))


(defn contains-eclipse? [range1 range2]
  (or (eclipses? range1 range2) (eclipses? range2 range1)))

(defn overlap? [[s1 e1] [s2 e2]]
  (or
   (<= s2 s1 e2)
   (<= s1 e2 e1)
   (<= s1 e2 e1)
   (<= s2 e1 e2)))


(defn p4-1 []
  (->> day4-data
       (filter #(apply contains-eclipse? %))
       count))

(defn p4-2 []
  (->> day4-data
       (filter #(apply overlap? %))
       count))
