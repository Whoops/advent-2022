(ns whoops.p12
  (:require
   [clojure.set :as set]
   [whoops.utils :as utils]))

(defn find-letter [grid letter]
  (let [[rows cols] (utils/grid-dims grid)]
    (reduce
     (fn [_ row]
       (let [start (reduce
                    (fn [_ col]
                      (when (= letter (utils/grid-point grid [row col]))
                        (reduced [row col])))
                    nil
                    (range cols))]
         (when start
           (reduced start))))
     nil
     (range rows))))

(defn to-state [grid]
  {:start (find-letter grid \S)
   :end (find-letter grid \E)
   :grid grid
   :cost-grid (utils/grid-of (utils/grid-dims grid) nil)
   :to-visit #{}
   :visited #{}})

(defn letter->elevation [letter]
  (condp = letter
    \S 0
    \E 25
    (- (Character/digit letter 36) 10)))

(defn mark-elevations [state]
  (update state :grid (fn [grid] (mapv #(mapv letter->elevation %) grid))))

(def day12-data
  (->> "day12.txt"
       utils/file-lines
       (mapv vec)
       to-state
       mark-elevations))

(defn neighbors [grid [x y]]
  (filter
   #(utils/in-grid? grid %)
   [[(dec x) y]
    [(inc x) y]
    [x (dec y)]
    [x (inc y)]]))

(defn reachable? [grid point elevation]
  (let [target-elevation (utils/grid-point grid point)]
    (<= target-elevation (inc elevation))))

(defn reachable-neighbors [grid point]
  (let [elevation (utils/grid-point grid point)
        targets (neighbors grid point)]
    (set (filter #(reachable? grid % elevation) targets))))

(defn search-step [{:keys [to-visit grid cost-grid visited] :as state}]
  (let [next-node (->> to-visit
                       (sort-by #(utils/grid-point cost-grid %))
                       first)
        node-cost (utils/grid-point cost-grid next-node)
        next-nodes (set/difference (reachable-neighbors grid next-node) visited)
        new-cost-grid (reduce #(utils/set-point %1 %2 (inc node-cost)) cost-grid next-nodes)]
    (-> state
        (update :to-visit disj next-node)
        (update :visited conj next-node)
        (update :to-visit #(apply conj % next-nodes))
        (assoc :cost-grid new-cost-grid))))

(defn prepare-state [{:keys [start] :as state}]
  (-> state
      (update :cost-grid #(utils/set-point % start 0))
      (update :to-visit conj start)))

(defn perform-search [{:keys [end] :as initial-state}]
  (loop [state (prepare-state initial-state)]
    (let [next-state (search-step state)
          end-cost (utils/grid-point (:cost-grid next-state) end)]
      (if (or end-cost (empty? (:to-visit next-state)))
        (assoc next-state :end-cost end-cost)
        (recur next-state)))))

(defn lowest-points [grid]
  (let [[rows cols] (utils/grid-dims grid)]
    (for [x (range rows)
          y (range cols)
          :when (= 0 (utils/grid-point grid [x y]))]
      [x y])))

(defn p1 []
  (:end-cost (perform-search day12-data)))

(defn p2 []
  (let [starts (lowest-points (:grid day12-data))]
    (->> starts
         (map #(assoc day12-data :start %))
         (map perform-search)
         (map :end-cost)
         (remove nil?)
         (apply min))))

(utils/register-day 12 [p1 p2])
