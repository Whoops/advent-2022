(ns whoops.p5
  (:require
   [whoops.utils :as utils]))

(defn stack->col [i]
  (+ 1 (* 4 (dec i))))

(defn read-stack [line i]
  (let [letter (nth line (stack->col i))]
    (if (= letter \space)
      nil
      letter)))

(defn clean-stacks [stacks]
  (reduce (fn [stacks i] (update stacks i #(->> %1 reverse (drop-while nil?)))) stacks (keys stacks)))

(defn parse-stacks [stacks]
  (let [piles (range 1 10)]
    (loop [lines (drop-last stacks)
           stacks (sorted-map)]
      (let [line (first lines)
            new-stacks (reduce #(update %1 %2 conj (read-stack line %2)) stacks piles)
            lines (rest lines)]
        (if (seq lines)
          (recur lines new-stacks)
          (clean-stacks new-stacks))))))

(defn parse-moves [moves]
  (->> moves
       (map #(re-seq #"\d+" %))
       (map #(map parse-long %))))

(defn parse-day5 [[stacks _split moves]]
  (let [stacks (parse-stacks stacks)
        moves (parse-moves moves)]
    [stacks moves]))

(def day5-data
  (->> "day5.txt"
       utils/file-lines
       (partition-by #(= "" %))
       parse-day5))

(defn do-move-p1 [stacks move]
  (let [[num source dest] move
        picked (take num (stacks source))]
    (-> stacks
        (update source #(drop num %))
        (update dest #(apply conj % picked)))))

(defn do-move-p2 [stacks move]
  (let [[num source dest] move
        picked (take num (stacks source))]
    (-> stacks
        (update source #(drop num %))
        (update dest #(concat picked %)))))

(defn p1 []
  (let [[stacks moves] day5-data
        stacks (reduce do-move-p1 stacks moves)]
    (apply str (map #(first (stacks %)) (-> stacks keys sort)))))

(defn p2 []
  (let [[stacks moves] day5-data
        stacks (reduce do-move-p2 stacks moves)]
    (apply str (map #(first (stacks %)) (-> stacks keys sort)))))

(utils/register-day 5 [p1 p2])
