(ns whoops.p8
  (:require [whoops.utils :as utils]))

(def day8-data
  (->> "day8.txt"
       utils/file-lines
       (mapv #(mapv (comp parse-long str) %))))

(defn trees-west [grid [x y]]
  (let [indexes (range (dec x) -1 -1)]
    (mapv #(utils/grid-point grid [% y]) indexes)))

(defn trees-east [grid [x y]]
  (let [[x-dim _] (utils/grid-dims grid)
        indexes (range (inc x) x-dim)]
    (mapv #(utils/grid-point grid [% y]) indexes)))

(defn trees-north [grid [x y]]
  (let [indexes (range (dec y) -1 -1)]
    (mapv #(utils/grid-point grid [x %]) indexes)))

(defn trees-south [grid [x y]]
  (let [[_ y-dim] (utils/grid-dims grid)
        indexes (range (inc y) y-dim)]
    (mapv #(utils/grid-point grid [x %]) indexes)))

(defn larger? [tree other-tress]
  (every? #(< % tree) other-tress))

(defn visible? [grid point]
  (let [tree (utils/grid-point grid point)]
    (or
     (larger? tree (trees-west grid point))
     (larger? tree (trees-east grid point))
     (larger? tree (trees-south grid point))
     (larger? tree (trees-north grid point)))))

(defn view-score-direction [tree other-trees]
  (reduce (fn [score other-tree]
            (if (>= other-tree tree)
              (reduced (inc score))
              (inc score)))
          0
          other-trees))

(defn view-score [grid point]
  (let [tree (utils/grid-point grid point)]
    (*
     (view-score-direction tree (trees-west grid point))
     (view-score-direction tree (trees-east grid point))
     (view-score-direction tree (trees-south grid point))
     (view-score-direction tree (trees-north grid point)))))

(defn p1 []
  (let [[x-dim y-dim] (utils/grid-dims day8-data)
        vis-grid (map
                  (fn [y]
                    (map
                     (fn [x]
                       (visible? day8-data [x y]))
                     (range x-dim)))
                  (range y-dim))]
    (->> vis-grid
         (map #(filter true? %))
         (map count)
         (reduce +))))

(defn p2 []
  (let [[x-dim y-dim] (utils/grid-dims day8-data)
        score-grid (map
                    (fn [y]
                      (map
                       (fn [x]
                         (view-score day8-data [x y]))
                       (range x-dim)))
                    (range y-dim))]
    (->> score-grid
         (map #(apply max %))
         (apply max))))

(utils/register-day 8 [p1 p2])
