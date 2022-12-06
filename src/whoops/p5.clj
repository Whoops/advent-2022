(ns whoops.p5
  (:require
   [whoops.utils :as utils]))

(defn stack->col [i]
  (inc (* 4 i)))

(defn read-stack [line i]
  (let [letter (nth line (stack->col i))]
    (if (= letter \space)
      nil
      letter)))

(defn clean-stacks [stacks]
  (mapv #(->> % reverse (drop-while nil?)) stacks))

(defn parse-stacks [stacks]
  (let [num-piles (-> stacks first count (- 3) (/ 4) inc)]
    (loop [lines (drop-last stacks)
           stacks (repeat num-piles '())]
      (let [line (first lines)
            new-stacks (map-indexed #(conj %2 (read-stack line %1)) stacks)]
        (if-let [lines (seq (rest lines))]
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
       parse-day5
       vec))

(defn do-move [stacks move reverse?]
  (let [[num source dest] move
        source (dec source)
        dest (dec dest)
        picked (take num (stacks source))
        picked (if reverse? (reverse picked) picked)]
    (-> stacks
        (update source #(drop num %))
        (update dest #(concat picked %)))))

(defn p1 []
  (let [[stacks moves] day5-data
        stacks (reduce #(do-move %1 %2 true) stacks moves)]
    (apply str (map first stacks))))

(defn p2 []
  (let [[stacks moves] day5-data
        stacks (reduce #(do-move %1 %2 false) stacks moves)]
    (apply str (map first stacks))))

(utils/register-day 5 [p1 p2])
