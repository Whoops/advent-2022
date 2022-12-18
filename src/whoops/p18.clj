(ns whoops.p18
  (:require [whoops.utils :as utils]
            [clojure.string :as string]))

(def sample-data
  (->> "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"
       (string/split-lines)
       (map #(string/split % #","))
       (map #(mapv parse-long %))
       (into #{})))

(def day18-data
  (->> "day18.txt"
       utils/file-lines
       (map #(string/split % #","))
       (map #(mapv parse-long %))
       (into #{})))

(defn generate-adjacents [[x y z]]
  [[x y (inc z)]
   [x y (dec z)]
   [x (inc y) z]
   [x (dec y) z]
   [(inc x) y z]
   [(dec x) y z]])

(defn sides-exposed [cubes cube]
  (->> cube
       generate-adjacents
       (filter #(not (cubes %)))
       count))

(defn exterior-sides-exposed [steam cube]
  (->> cube
       generate-adjacents
       (filter #(steam %))
       count))

(defn in-range? [[x y z] x-max y-max z-max]
  (and (<= -1 x (inc x-max))
       (<= -1 y (inc y-max))
       (<= -1 z (inc z-max))))

(defn find-steam [cubes]
  (let [x-max (->> cubes (map first) (apply max))
        y-max (->> cubes (map second) (apply max))
        z-max (->> cubes (map #(nth % 2)) (apply max))]
    (loop [steam #{[0 0 0]}
           to-explore #{[0 0 0]}]
      (if (not (seq to-explore))
        steam
        (let [explore (first to-explore)
              to-explore (disj to-explore explore)
              new-steam (->> explore
                             (generate-adjacents)
                             (filter #(not (steam %)))
                             (filter #(not (cubes %)))
                             (filter #(in-range? % x-max y-max z-max)))]
          (recur (into steam new-steam)
                 (into to-explore new-steam)))))))

(defn sample-p1 []
  (->> sample-data
       (map #(sides-exposed sample-data %))
       (reduce +)))

(defn sample-p2 []
  (let [steam (find-steam sample-data)]
    (->> sample-data
         (map #(exterior-sides-exposed steam %))
         (reduce +))))

(defn p1 []
  (->> day18-data
       (map #(sides-exposed day18-data %))
       (reduce +)))

(defn p2 []
  (let [steam (find-steam day18-data)]
    (->> day18-data
         (map #(exterior-sides-exposed steam %))
         (reduce +))))

(utils/register-day 18 [p1 p2])
