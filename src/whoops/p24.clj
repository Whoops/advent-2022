(ns whoops.p24
  (:require
   [whoops.utils :as utils]))

(def arrow->direction
  {">" :east
   "<" :west
   "^" :north
   "v" :south})

(defn parse-world [grid]
  (let [[width height] (utils/grid-dims grid)
        storms (for [x (range width)
                     y (range height)
                     :let [tile (utils/grid-point grid [x y])]
                     :when (and (not= "#" tile) (not= "." tile))]
                 {:pos [x y]
                  :direction (arrow->direction tile)})]
    {:start [1 0]
     :exit [(- width 2) (dec height)]
     :dims [width height]
     :storms storms
     :storm-index (into #{} (map :pos storms))}))

(def day24-data
  (->> "day24.txt"
       utils/file-lines
       (mapv #(mapv str %))
       parse-world))

(defn wall? [{:keys [dims]} [x y]]  
  (let [[width height] dims]
    (or (neg? x)
        (neg? y)
        (zero? x)
        (zero? y)
        (= (dec width) x)
        (= (dec height) y))))

(defn move-storm [{:keys [dims] :as world} {:keys [pos direction] :as storm}]
  (let [[width height] dims
        [x y] pos
        new-pos (condp = direction
                  :north [x (dec y)]
                  :south [x (inc y)]
                  :east [(inc x) y]
                  :west [(dec x) y])]
    (if (not (wall? world new-pos))
      (assoc storm :pos new-pos)
      (assoc storm :pos
             (condp = direction
               :north [x (- height 2)]
               :south [x 1]
               :east [1 y]
               :west [(- width 2) y])))))

(defn move-storms [{:keys [storms] :as world}]
  (let [new-storms (map #(move-storm world %) storms)]
    (assoc world
           :storms new-storms
           :storm-index (into #{} (map :pos new-storms)))))

(defn can-move? [{:keys [storm-index start exit] :as world} point]
  (cond
    (or (= start point) (= exit point)) true
    (storm-index point) false
    (wall? world point) false
    :else true))

(defn generate-options [world [x y]]
  (remove nil?
          [(when (can-move? world [x y])
             [x y])
           (when (can-move? world [x (inc y)])
             [x (inc y)])
           (when (can-move? world [x (dec y)])
             [x (dec y)])
           (when (can-move? world [(inc x) y])
             [(inc x) y])
           (when (can-move? world [(dec x) y])
             [(dec x) y])]))

(defn find-exit [world start exit]
  (loop [world world
         parties #{start}
         turn 0]
    (if (parties exit)
      [world turn]
      (let [world (move-storms world)
            parties (->> parties
                         (mapcat #(generate-options world %))
                         (into #{}))]
        (recur world
               parties
               (inc turn))))))

(defn p1 []
  (let [{:keys [start exit] :as world} day24-data]
    (second (find-exit world start exit))))

(defn p2 []
  (let [{:keys [start exit] :as world} day24-data
        [world to-exit] (find-exit world start exit)
        [world to-entrance] (find-exit world exit start)
        [_ back-to-exit] (find-exit world start exit)]
    (+ to-exit to-entrance back-to-exit)))

(utils/register-day 24 [p1 p2])
