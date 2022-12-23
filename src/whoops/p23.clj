(ns whoops.p23
  (:require [whoops.utils :as utils]
            [clojure.string :as string]))

(defn find-elves [grid]
  (let [[max-x max-y] (utils/grid-dims grid)]
    (into #{}
          (for [x (range max-x)
                y (range max-y)
                :when (= "#" (utils/grid-point grid [x y]))]
            [x y]))))

(def day23-data
  (->> "day23.txt"
       utils/file-lines
       (mapv #(mapv str %))
       find-elves))

(defn initial-state [elves]
  {:elves elves
   :directions '(:north :south :west :east)})

(defn look [elves [x y] direction]
  (condp = direction
    :north (or (elves [(dec x) (dec y)]) (elves [x (dec y)]) (elves [(inc x) (dec y)]))
    :south (or (elves [(dec x) (inc y)]) (elves [x (inc y)]) (elves [(inc x) (inc y)]))
    :east (or (elves [(inc x) (dec y)]) (elves [(inc x) y]) (elves [(inc x) (inc y)]))
    :west (or (elves [(dec x) (dec y)]) (elves [(dec x) y]) (elves [(dec x) (inc y)]))))

(defn shift-pos [[x y] direction]
  (condp = direction
    :north [x (dec y)]
    :south [x (inc y)]
    :east [(inc x) y]
    :west [(dec x) y]))

(defn get-proposals [elves moving-elves directions]
  (reduce (fn [proposals elf]
            (loop [directions directions]
              (let [direction (first directions)]
                (cond
                  (nil? direction) proposals
                  (not (look elves elf direction)) (assoc proposals elf 
(shift-pos elf direction))
                  :else (recur (rest directions))))))
          {}
          moving-elves))

(defn should-move? [elves [e-x e-y]]
  (let [adjacent-elves
        (for [x [(dec e-x) e-x (inc e-x)]
              y [(dec e-y) e-y (inc e-y)]
              :when (not (and (= x e-x) (= y e-y)))]
          (elves [x y]))]
    (not (every? nil? adjacent-elves))))

(defn move-elf [elves elf proposals proposal-counts]
  (let [proposal (proposals elf)]
    (if (= 1 (proposal-counts proposal))
      (-> elves
          (disj elf)
          (conj proposal))
      elves)))

(defn move [{:keys [elves directions] :as state}]
  (let [moving-elves (filter #(should-move? elves %) elves)
        proposals (get-proposals elves moving-elves directions)
        proposal-counts (frequencies (vals proposals))
        new-elves (reduce #(move-elf %1 %2 proposals proposal-counts) elves (keys proposals))]
    (assoc state :elves new-elves
           :directions (concat (rest directions) [(first directions)]))))

(defn state-bounds [{:keys [elves]}]
  [[(apply min (map first elves))
    (apply min (map second elves))]
   [(apply max (map first elves))
    (apply max (map second elves))]])

(defn draw-state [{:keys [elves] :as state}]
  (let [[[min-x min-y] [max-x max-y]] (state-bounds state)]
    (doseq [y (range min-y (inc max-y))]
      (->> (range min-x (inc max-x))
           (map #(if (elves [% y])
                   "#"
                   "."))
           string/join
           println))))


(defn count-empty-tiles [{:keys [elves] :as state}]
  (let [[[min-x min-y] [max-x max-y]] (state-bounds state)]
    (-
     (* (- (inc max-x) min-x)
        (- (inc max-y) min-y))
     (count elves))))

(defn run-simulation [state]
  (loop [{:keys [elves] :as state} state
         turn 0]
    (let [new-state (move state)]
      (if (= elves (:elves new-state))
        (inc turn)
        (recur new-state (inc turn))))))

(defn p1 []
  (->> day23-data
       initial-state
       (iterate move)
       (drop 10)
       first
       count-empty-tiles))

(defn p2 []
  (run-simulation (initial-state day23-data)))

(utils/register-day 23 [p1 p2])
