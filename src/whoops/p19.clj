(ns whoops.p19
  (:require [clojure.string :as string]
            [whoops.utils :as utils]))

(defn parse-ore [ore]
  {:ore (->> ore
             (re-seq #"\d+")
             first
             parse-long)})

(defn parse-clay [clay]
  {:ore (->> clay
             (re-seq #"\d+")
             first
             parse-long)})

(defn parse-obsidian [obsidian]
  (let [[ore clay] (map parse-long (re-seq #"\d+" obsidian))]
    {:ore ore
     :clay clay}))

(defn parse-geode [geode]
  (let [[ore obsidian] (map parse-long (re-seq #"\d+" geode))]
    {:ore ore
     :obsidian obsidian}))

(defn parse-line [line]
  (let [[blueprint ores] (string/split line #":")
        num (->> blueprint (re-seq #"\d+") first parse-long)
        ores (string/split ores #"\.")
        [ore clay obsidian geode] (map #(%1 %2) [parse-ore parse-clay parse-obsidian parse-geode] ores)]
    {:number num
     :robots {:ore ore
              :clay clay
              :obsidian obsidian
              :geode geode}}))

(defn find-max-rates [blueprint]
  (assoc blueprint
         :max-rates {:ore (->> blueprint
                               :robots
                               vals
                               (map #(get % :ore 0))
                               (apply max))
                     :clay (->> blueprint
                                :robots
                                vals
                                (map #(get % :clay 0))
                                (apply max))
                     :obsidian (->> blueprint
                                    :robots
                                    vals
                                    (map #(get % :obsidian 0))
                                    (apply max))}))

(def day19-data
  (->> "day19.txt"
       utils/file-lines
       (map parse-line)
       (map find-max-rates)))

(defn initial-state []
  {:materials {:ore 0
               :clay 0
               :obsidian 0
               :geode 0}
   :rates {:ore 1
           :clay 0
           :obsidian 0
           :geode 0}
   :building nil})

(defn can-build? [state blueprint robot]
  (reduce (fn [result [material amount]]
            (and result
                 (>=
                  (get-in state [:materials material])
                  amount)))
          true
          (get-in blueprint [:robots robot])))

(defn collect-materials [{:keys [materials rates] :as state}]
  (assoc state :materials
         {:ore (+ (:ore materials) (:ore rates))
          :clay (+ (:clay materials) (:clay rates))
          :obsidian (+ (:obsidian materials) (:obsidian rates))
          :geode (+ (:geode materials) (:geode rates))}))

(defn worth-building? [{:keys [rates]} {:keys [max-rates]} robot]
  (or (= robot :geode) (> (max-rates robot) (rates robot))))

(defn generate-options [state blueprint]
  (cond
    (can-build? state blueprint :geode) #{:geode}
    (and (worth-building? state blueprint :obsidian)
         (can-build? state blueprint :obsidian)) #{:obsidian nil}
    :else (->> [:ore :clay]
               (filter #(worth-building? state blueprint %))
               (map #(when (can-build? state blueprint %) %))
               (into #{}))))

(defn start-building [state blueprint robot]
  (if robot
    (let [state (reduce (fn [state [material amount]]
                          (update-in state [:materials material] #(- % amount)))
                        state
                        (get-in blueprint [:robots robot]))]
      (assoc state :building robot))
    state))

(defn finish-building [state]
  (if-let [robot (:building state)]
    (-> state
        (assoc :building nil)
        (update-in [:rates robot] inc))
    state))

(defn perform-action [state blueprint robot]
  (-> state
      (start-building blueprint robot)
      collect-materials
      finish-building))

(defn next-states [state blueprint]
  (let [options (generate-options state blueprint)
        next-states (map #(perform-action state blueprint %) options)]
    next-states))

(defn find-optimal [blueprint max-turns]
  (loop [states #{(initial-state)}
         turn 0]
    (if (= max-turns turn)
      (->> states (map #(get-in % [:materials :geode])) sort last)
      (recur (->> states
                  (mapcat #(next-states % blueprint))
                  (into #{}))
             (inc turn)))))

(defn p1 []
  (->> day19-data
       (map #(* (:number %) (find-optimal % 24)))
       (reduce +)))


(defn p2 []
  (->> day19-data
       (take 3)
       (map #(find-optimal % 32))
       (apply *)))


(whoops.utils/register-day 19 [p1 p2])
