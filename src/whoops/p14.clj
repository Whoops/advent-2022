(ns whoops.p14
  (:require
   [clojure.string :as string]
   [whoops.utils :as utils]))

(defn expand-wall [[[x1 y1] [x2 y2]]]
  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))]
    [x y]))

(def sample-data
  (->> "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"
       string/split-lines
       (map #(string/replace % " -> " ","))
       (map #(string/split % #","))
       (map #(map parse-long %))
       (map #(partition 2 %))
       (map #(partition 2 1 %))
       (apply concat)
       (map expand-wall)
       (apply concat)
       (apply sorted-set)))

(def day14-data
  (->> "day14.txt"
       utils/file-lines
       (map #(string/replace % " -> " ","))
       (map #(string/split % #","))
       (map #(map parse-long %))
       (map #(partition 2 %))
       (map #(partition 2 1 %))
       (apply concat)
       (map expand-wall)
       (apply concat)
       (apply sorted-set)))

(defn find-x-bounds [walls]
  [(->> walls
        (map first)
        (apply min)
        (+ -2))
   (->> walls
        (map first)
        (apply max)
        (+ 3))])

(defn find-floor [walls]
  (->> walls
       (map second)
       (apply max)
       (+ 2)))

(defn initial-state [walls floor?]
  (let [[x-min x-max] (find-x-bounds walls)
        size (- x-max x-min)]
    {:offset x-min
     :grid (mapv (fn [idx]
                   (->> walls
                        (filter #(= (+ idx x-min) (first %)))
                        (map second)
                        (apply sorted-set)))
                 (range size))
     :void false
     :blocked false
     :floor (when floor? (find-floor walls))
     :grains 0}))

(defn occupied? [{:keys [offset grid floor]} [x y]]
  (boolean (or (= y floor) ((nth grid (- x offset)) y))))

(defn settle-point [{:keys [offset grid floor]} [x y]]
  (let [x (- x offset)
        col (nth grid x)
        peak (first col)
        floor (when floor (dec floor))]
    (cond
      (not peak) floor
      (< y peak) (dec peak)
      :else (let [y (->> col
                         (drop-while #(< % y))
                         first)]
              (if y
                (dec y)
                floor)))))

(defn expand-state [{:keys [offset grid] :as state} x]
  (cond
    (= x (inc offset)) (-> state
                           (assoc :grid (vec (concat [(sorted-set)] grid)))
                           (update :offset dec))
    (= x (+ offset (count grid) -2)) (assoc state :grid (vec (concat grid [(sorted-set)])))
    :else state))

(defn set-sand [{:keys [grid offset] :as state} [orig-x y]]
  (let [x (- orig-x offset)
        grid (update grid x conj y)]
    (-> state
        (assoc :grid grid)
        (update :grains inc)
        (expand-state orig-x))))

(defn drop-sand [state]
  (loop [sand [500 0]]
    (let [[x _] sand
          y (settle-point state sand)]
      (cond
        (not y) (assoc state :void true)
        (not (occupied? state [(dec x) (inc y)])) (recur [(dec x) (inc y)])
        (not (occupied? state [(inc x) (inc y)])) (recur [(inc x) (inc y)])
        (= [500 0] [x y]) (-> state
                              (set-sand [x y])
                              (assoc :blocked true))
        :else (set-sand state [x y])))))

(defn flow [state]
  (loop [state state]
    (let [state (drop-sand state)]
      (if (or (:void state) (:blocked state))
        state
        (recur state)))))

(defn p1 []
  (-> day14-data
      (initial-state false)
      flow
      :grains))

(defn p2 []
  (-> day14-data
      (initial-state true)
      flow
      :grains))

(utils/register-day 14 [p1 p2])
