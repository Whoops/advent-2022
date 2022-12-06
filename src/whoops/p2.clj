(ns whoops.p2
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [whoops.utils :as utils]))

(def beats {:paper :rock
            :scissors :paper
            :rock :scissors})

(def loses (set/map-invert beats))

(def opponent->play
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def me->play
  {"X" :rock
   "Y" :paper
   "Z" :scissors})

(def letter->result
  {"X" :lose
   "Y" :tie
   "Z" :win})

(defn parse-line-p1 [line]
  (let [[them me] (string/split line #"\s+")]
    [(opponent->play them) (me->play me)]))

(defn parse-line-p2 [line]
  (let [[them result] (string/split line #"\s+")]
    [(opponent->play them) (letter->result result)]))

(def day2-data
  (->> "day2.txt"
       utils/file-lines
       vec))


(defn round-result [[them me]]
  (cond
    (= them me) :tie
    (= (beats me) them) :win
    :else :lose))

(def result->score
  {:tie 3
   :win 6
   :lose 0})

(def shape->score
  {:rock 1
   :paper 2
   :scissors 3})

(defn score-round [[them me]]
  (+ (-> [them me]
         round-result
         result->score)
     (shape->score me)))

(defn find-play [[them result]]
  (cond
    (= :lose result) [them (beats them)]
    (= :win result) [them (loses them)]
    (= :tie result) [them them]))

(defn p1 []
  (->> day2-data
       (map parse-line-p1)
       (map score-round)
       (reduce +)))

(defn p2 []
  (->> day2-data
       (map parse-line-p2)
       (map find-play)
       (map score-round)
       (reduce +)))

(utils/register-day 2 [p1 p2])
