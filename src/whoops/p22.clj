(ns whoops.p22
  (:require
   [whoops.utils :as utils]))

(defn initial-state [grid directions]
  {:pos [(->> grid first (keep-indexed #(when (= %2 ".") %1)) first) 0]
   :facing :east
   :grid grid
   :directions directions})

(def letter->direction
  {"L" :left
   "R" :right})

(defn parse-directions [directions]
  (->> directions
       (re-seq #"(?:\d+|L|R)")
       (map #(if-let [n (parse-long %)]
               n
               (letter->direction %)))))

(defn parse [[grid _ directions]]
  (initial-state
   (mapv #(mapv str %) grid)
   (parse-directions (first directions))))

(def day22-data
  (->> "day22.txt"
       utils/file-lines
       (partition-by #(= "" %))
       parse))

(def right-turn
  {:east :south
   :south :west
   :west :north
   :north :east})

(def left-turn
  {:east :north
   :north :west
   :west :south
   :south :east})

(defn grid-pos [grid [x y]]
  (if (or (neg? y) (> y (dec (count grid))))
    nil
    (let [row (nth grid y)]
      (if (or (neg? x) (> x (dec (count row))))
        nil
        (nth row x)))))

(def invert? {[:north :north] false
              [:north :east] false
              [:north :west] true
              [:north :south] true
              [:east :north] false
              [:east :east] false
              [:east :west] true
              [:east :south] true
              [:south :north] true
              [:south :east] true
              [:south :west] false
              [:south :south] false
              [:west :north] true
              [:west :south] false
              [:west :east] true
              [:west :west] false})

(defn find-face [{:keys [pos size faces]}]
  (first
   (keep-indexed (fn [idx face]
                   (when (and (<= (first face) (first pos) (+ size (dec (first face))))
                              (<= (second face) (second pos) (+ size (dec (second face)))))
                     idx))
                 faces)))

(defn wrap-cube [{:keys [pos facing face-map faces size grid] :as state}]
  (let [[x y] pos
        face (find-face state)
        [current-face-x current-face-y] (nth faces face)
        rel-x (- x current-face-x)
        rel-y (- y current-face-y)
        adj (if (#{:north :south} facing) rel-x rel-y)
        [new-face new-facing] (-> face-map (nth face) facing)
        adj (if (invert? [facing new-facing]) (- (dec size) adj) adj)
        [new-face-x new-face-y] (nth faces new-face)
        new-pos (condp = new-facing
                  :south [(+ new-face-x adj) new-face-y]
                  :north [(+ new-face-x adj) (+ new-face-y (dec size))]
                  :east [new-face-x (+ new-face-y adj)]
                  :west [(+ new-face-x (dec size)) (+ new-face-y adj)])]
    (if (= "#" (grid-pos grid new-pos))
      state
      (assoc state :pos new-pos :facing new-facing))))

(defn shift-direction [[x y] direction]
  (condp = direction
    :east [(inc x) y]
    :west [(dec x) y]
    :south [x (inc y)]
    :north [x (dec y)]))

(defn wrap-pos [grid [x y] direction]
  (let [next-pos (cond
                   (= :east direction) [0 y]
                   (= :west direction) [(dec (count (nth grid y))) y]
                   (= :south direction) [x 0]
                   (= :north direction) [x (dec (count grid))])]
    (loop [next-pos next-pos]
      (let [tile (grid-pos grid next-pos)]
        (cond
          (= tile "#") [x y]
          (= tile ".") next-pos
          (or (nil? tile) (= tile " ")) (recur (shift-direction next-pos direction)))))))

(defn next-pos [{:keys [pos facing grid face-map] :as state}]
  (let [next-pos (shift-direction pos facing)
        tile (grid-pos grid next-pos)]
    (cond
      (= tile ".") (assoc state :pos next-pos)
      (= tile "#") state
      (and face-map (or (nil? tile) (= " " tile))) (wrap-cube state)
      (or (nil? tile) (= " " tile)) (assoc state :pos (wrap-pos grid pos facing)))))

(defn turn [state direction]
  (condp = direction
    :left (update state :facing left-turn)
    :right (update state :facing right-turn)))

(defn follow-direction [{:keys [directions] :as state}]
  (let [instruction (first directions)
        next-state (if (number? instruction)
                     (as-> state $ (iterate next-pos $) (nth $ instruction))
                     (turn state instruction))]
    (update next-state :directions rest)))

(defn follow-directions [state]
  (loop [{:keys [directions] :as state} state]
    (if (seq directions)
      (recur (follow-direction state))
      state)))

(def facing->score
  {:east 0
   :south 1
   :west 2
   :north 3})

(defn score-state [{:keys [pos facing]}]
  (let [[x y] pos]
    (+ (* 1000 (inc y)) (* 4 (inc x)) (facing->score facing))))

(defn cube-size [grid]
  (/
   (max (count grid)
        (apply max (map count grid)))
   4))

(defn find-faces [{:keys [grid] :as state}]
  (let [size (cube-size grid)
        max-x (apply max (map count grid))
        max-y (count grid)
        faces (for [y (range 0 max-y size)
                    x (range 0 max-x size)
                    :let [tile (grid-pos grid [x y])]
                    :when (and tile (not= tile " "))]
                [x y])]
    (assoc state :faces (vec faces) :size size)))

(def p2-map
  [{:north [5 :east]
    :west [3 :east]}
   {:north [5 :north]
    :east [4 :west]
    :south [2 :west]}
   {:east [1 :north]
    :west [3 :south]}
   {:north [2 :east]
    :west [0 :east]}
   {:east [1 :west]
    :south [5 :west]}
   {:east [4 :north]
    :south [1 :south]
    :west [0 :south]}])

(defn add-face-map [face-map state]
  (assoc state :face-map face-map))

(defn p1 []
  (->> day22-data
       follow-directions
       score-state))

(defn p2 []
  (->> day22-data
       find-faces
       (add-face-map p2-map)
       follow-directions
       score-state))

(utils/register-day 22 [p1 p2])
