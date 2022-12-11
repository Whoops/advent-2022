(ns whoops.p11
  (:require [whoops.utils :as utils])
  (:import [clojure.lang PersistentQueue]))


(defn find-single-num [num-str]
  (->>  num-str
        (re-seq #"\d+")
        first
        parse-long))

(defn parse-starting-items [starting-items]
  (->>  starting-items
        (re-seq #"\d+")
        (map parse-long)
        (apply conj PersistentQueue/EMPTY)))

(defn parse-operation [operation]
  (let [[t1 op t2] (drop 1 (re-find #"new = (\d+|\w+) (.) (\d+|\w+)" operation))
        op (if (= "+" op) + *)
        t1 (parse-long t1)
        t2 (parse-long t2)]
    (fn [item]
      (op (if t1 t1 item) (if t2 t2 item)))))

(defn parse-monkey [[monkey starting-items operation test t f]]
  {:monkey (find-single-num monkey)
   :items (parse-starting-items starting-items)
   :operation (parse-operation operation)
   :test (find-single-num test)
   :t (find-single-num t)
   :f (find-single-num f)
   :items-inspected 0})

(def day11-data
  (->>
   "day11.txt"
   utils/file-lines
   (partition-by #(= "" %))
   (filter #(not= 1 (count %)))
   (mapv parse-monkey)))

(def ^:dynamic *depreciate?* true)

(defn depreciate-item [item]
  (if *depreciate?*
    (long (/ item 3))
    item))

(def denom (reduce * (map :test day11-data)))

(defn manage-worry [item]
  (mod item denom))

(defn inspect-item [state n]
  (let [{:keys [items operation test t f] :as monkey} (nth state n)
        item (->> items
                  peek
                  operation
                  depreciate-item
                  manage-worry)
        new-monkey (-> monkey
                       (update :items pop)
                       (update :items-inspected inc))
        test-result (= 0 (mod item test))
        target-monkey (if test-result t f)]
    (-> state
        (assoc n new-monkey)
        (update target-monkey #(update % :items conj item)))))

(defn run-monkey [state n]
  (loop [state state]
    (let [monkey (nth state n)]
      (if (peek (:items monkey))
        (recur (inspect-item state n))
        state))))

(defn do-monkey-business [state]
  (reduce run-monkey state (range (count state))))

(defn monkey-business [state]
  (->> state
       (sort-by :items-inspected)
       reverse
       (take 2)
       (map :items-inspected)
       (apply *)))

(defn p1 []
  (let [state (nth (iterate do-monkey-business day11-data) 20)]
    (monkey-business state)))

(defn p2 []
  (binding [*depreciate?* false]
    (let [state (nth (iterate do-monkey-business day11-data) 10000)]
      (monkey-business state))))

(utils/register-day 11 [p1 p2])
