(ns whoops.p10
  (:require [whoops.utils :as utils]
            [clojure.string :as string]))

(defmulti parse-instruction first)

(defmethod parse-instruction "noop" [_]
  [:noop])

(defmethod parse-instruction "addx" [[_op arg]]
  [:addx (parse-long arg)])

(defn initial-state [instructions]
  {:x 1
   :x-last 1
   :cycle 0
   :cycle-op 0
   :ops instructions
   :display (vec (repeat 6 (vec (repeat 40 "."))))})

(defn sprite-visible? [{:keys [x]} col]
  (<= (abs (- x col)) 1))

(defn set-current-pixel [{:keys [cycle display] :as state}]
  (let [row (unchecked-divide-int cycle 40)
        col (mod cycle 40)]
    (if (sprite-visible? state col)
      (assoc state :display (assoc-in display [row col] "#"))
      state)))

(defn next-op [{:keys [ops] :as state}]
  (assoc state
         :op (first ops)
         :ops (rest ops)
         :cycle-op 0))

(defmulti perform-op
  (fn [{:keys [op]}]
    (first op)))

(defmethod perform-op :noop [state]
  (assoc state :op nil))

(defmethod perform-op :addx [{:keys [cycle-op op] :as state}]
  (let [[_ arg] op]
    (if (= 1 cycle-op)
      (-> state
          (update :x + arg)
          (assoc :op nil))
      state)))

(defn run-cycle [{:keys [op :x] :as state}]
  (let [state (if op state (next-op state))]
    (-> state
        set-current-pixel
        perform-op
        (update :cycle inc)
        (update :cycle-op inc)
        (assoc :x-last x))))

(def day10-data
  (->>
   "day10.txt"
   utils/file-lines
   (map #(string/split % #" "))
   (mapv parse-instruction)))

(defn signal [{:keys [x-last cycle]}]
  (* x-last cycle))

(defn trace [x]
  x)

(defn p1 []
  (let [cycles-of-interest [20 60 100 140 180 220]
        steps (iterate run-cycle (initial-state day10-data))]
    (->> cycles-of-interest
         (map #(signal (nth steps %)))
         trace
         (reduce +))))

(defn p2 []
  (println)
  (->> (initial-state day10-data)
       (iterate run-cycle)
       (take 240)
       last
       :display
       (map string/join)
       (map println)
       (doall)))

(utils/register-day 10 [p1 p2])
