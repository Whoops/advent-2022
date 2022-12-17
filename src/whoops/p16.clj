(ns whoops.p16
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [whoops.utils :as utils]))

;; OK this works but takes forever
;; A new idea might be to attempt to build
;; A new graph that only contains the valves
;; worth opening. It'd have to be a wieghted
;; graph with each node connected to every
;; other node. This would solve the problem
;; of idling states where we just wander
;; around without opening anything.
(defn parse-line [line]
  (let [[_ valve rate-str tunnels] (first (re-seq #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)" line))]
    {:name valve
     :rate (parse-long rate-str)
     :tunnels (string/split tunnels #", ")}))

(defn initial-state [mapping]
  {:valves mapping
   :flow 0
   :released 0
   :open-valves (into #{} (->> mapping vals (filter #(zero? (:rate %))) (map :name)))
   :current-location "AA"})

(def day16-data
  (->> "day16.txt"
       utils/file-lines
       (map parse-line)
       (map #(vector (:name %) %))
       (into {})))

(defn release-pressure [{:keys [flow] :as state}]
  (update state :released #(+ flow %)))

(defn valves-open? [state]
  (let [useful-valves (->> (:valves state)
                           vals
                           (remove #(zero? (:rate %)))
                           (map :name)
                           (into #{}))]
    (= useful-valves (:open-valves state))))

(defn stay-put [state]
  state)

(defn open-valve [state]
  (let [flow-inc (get-in state [:valves (:current-location state) :rate])]
    (-> state
        (update :open-valves conj (:current-location state))
        (update :flow + flow-inc))))

(defn move [state location]
  (assoc state :current-location location))

(defn open? [state]
  (boolean ((:open-valves state) (:current-location state))))

(defn generate-options [{:keys [current-location valves] :as state}]
  (if (valves-open? state)
    #{(stay-put state)}
    (let [tunnels (get-in valves [current-location :tunnels])]
      (into #{} (concat (when (not (open? state))
                          [(open-valve state)])
                        (map #(move state %) tunnels))))))

(defn project-state [{:keys [flow released]} turns-remaining]
  (+ released (* flow turns-remaining)))

(defn best-case [{:keys [released]} total-flow turns-remaining]
  (+ released (* total-flow turns-remaining)))

(defn remove-underperformers [best-rate turns-remaining total-flow states]
  (let [best-projection (->> states
                             (map #(project-state % turns-remaining))
                             (reduce max))
        best-state (->> states
                        (map :released)
                        (reduce max))
        threshold (- best-state (* turns-remaining best-rate))]
    (->> states
         (filter #(>= (best-case % total-flow turns-remaining) best-projection))
         (filter #(>= (:released %) threshold)))))

(defn make-el-state [state]
  (-> state
      (assoc :flow 0)
      (assoc :current-location "AA")))

(defn best-result [state turns reset-turn]
  (let [best-rate (->> (:valves state)
                       vals
                       (map :rate)
                       (apply max))
        total-flow (->> (:valves state)
                        vals
                        (map :rate)
                        (sort #(* -1 (compare %1 %2)))
                        (take turns)
                        (apply +))]
    (loop [states [state]
           turn 1]
      (cond
        (= turn turns) (->> states
                            (map release-pressure)
                            (sort-by :released)
                            (last))
        ;; This sets things up for the elephant to take it's turn now
        (= turn reset-turn) (recur (->> states
                                        (map release-pressure)
                                        (pmap generate-options)
                                        (apply set/union)
                                        (remove-underperformers best-rate (- turns turn) total-flow)
                                        (map make-el-state))
                                   (inc turn))
        :else (recur (->> states
                    (map release-pressure)
                    (pmap generate-options)
                    (apply set/union)

                    (remove-underperformers best-rate (- turns turn) total-flow))
               (inc turn))))))

(defn p1 []
  (:released (best-result (initial-state day16-data) 30 nil)))

(defn p2 []
  (:released (best-result (initial-state day16-data) 52 26)))

(utils/register-day 16 [p1 p2])
