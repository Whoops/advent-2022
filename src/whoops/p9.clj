(ns whoops.p9
  (:require [whoops.utils :as utils]
            [clojure.string :as string]))

(def dir-lookup {"R" :right
                 "L" :left
                 "U" :up
                 "D" :down})

(defn parse-direction [[dir cnt]]
  (let [steps (parse-long cnt)
        direction (dir-lookup dir)]
    [direction steps]))

(def day9-data
  (->> "day9.txt"
       utils/file-lines
       (map #(string/split % #" "))
       (mapv parse-direction)))

(defn move-head [state direction]
  (condp = direction
    :up (update-in state [:knots 0 1] inc)
    :down (update-in state [:knots 0 1] dec)
    :left (update-in state [:knots 0 0] dec)
    :right (update-in state [:knots 0 0] inc)))

(defn move-knot [state n]
  (let [[kx ky] (get-in state [:knots n])
        [px py] (get-in state [:knots (dec n)])
        dx (- px kx)
        dy (- py ky)
        move-knot? (or (= 2 (abs dx)) (= 2 (abs dy)))
        dx (cond (zero? dx) 0 (neg? dx) -1 (pos? dx) 1)
        dy (cond (zero? dy) 0 (neg? dy) -1 (pos? dy) 1)]
    (if move-knot?
      (assoc-in state [:knots n] [(+ kx dx) (+ ky dy)])
      state)))

(defn update-tail-visits [{:keys [knots] :as state}]
  (update state :tail-visits conj (last knots)))

(defn move-knots [state]
  (let [knots (->> state :knots count (range 1))]
    (reduce move-knot state knots)))

(defn move [state direction]
  (-> state
      (move-head direction)
      move-knots
      update-tail-visits))

(defn move-n [state [direction n]]
  (->> state
       (iterate #(move % direction))
       (take (inc n))
       last))

(defn initial-state [knots]
  {:knots (vec (repeat knots [0 0])) :tail-visits #{[0 0]}})

(defn p1 []
  (->> day9-data
       (reduce move-n (initial-state 2))
       :tail-visits
       count))

(defn p2 []
  (->> day9-data
       (reduce move-n (initial-state 10))
       :tail-visits
       count))

(utils/register-day 9 [p1 p2])
