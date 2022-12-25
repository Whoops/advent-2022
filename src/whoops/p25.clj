(ns whoops.p25
  (:require
   [clojure.set :refer [map-invert]]
   [clojure.string :as string]
   [whoops.utils :as utils]))

(def day25-data
  (->> "day25.txt"
       utils/file-lines
       (mapv #(mapv str %))))

(def snafu-digit->value
  {"2" 2
   "1" 1
   "0" 0
   "-" -1
   "=" -2})

(def value->snafu-digit
  (map-invert snafu-digit->value))

(defn pow [base exp]
  (long (.pow (bigdec base) exp)))

(defn decode-snafu [snafu]
  (->> snafu
       reverse
       (map snafu-digit->value)
       (map-indexed vector)
       (map (fn [[idx value]]
              (* value (pow 5 idx))))
       (reduce +)))

(defn find-snafu-exp [num]
  (loop [exp 0]
    (if (>= (* 2 (pow 5 exp)) num)
      exp
      (recur (inc exp)))))

(defn encode-snafu [num]
  (let [exp (find-snafu-exp num)]
    (loop [exp exp
           num num
           snafu []]
      (if (= -1 exp)
        (->> snafu
             (map value->snafu-digit)
             string/join)
        (let [base (pow 5 exp)
              times (/ (abs num) base)
              times (Math/round (double times))
              times (min 2 times)
              times (if (neg? num) (* -1 times) times)
              rem (- num (* times base))]
          (recur (dec exp)
                 rem
                 (conj snafu times)))))))

(defn p1 []
  (->> day25-data
       (map decode-snafu)
       (reduce +)
       encode-snafu))

(defn p2 []
  "Merry Christmas!")

(utils/register-day 25 [p1 p2])
