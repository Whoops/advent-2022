(ns whoops.utils
  (:require [clojure.java.io :as io]))

(def days (atom {}))

(defn file-lines [filename]
  (let [file-path (str "files/" filename)]
    (with-open [read (io/reader file-path)]
      (doall (line-seq read)))))


(defn register-day [day [p1 p2]]
  (swap! days assoc day [p1 p2]))


(defn grid-dims [grid]
  [(count (first grid)) (count grid)])

(defn grid-point [grid [x y]]
 (-> grid
     (nth y)
     (nth x)))

(defn grid-of [[rows cols] val]
  (->> val
       (repeat rows)
       vec
       (repeat cols)
       vec))

(defn set-point [grid [x y] val]
  (assoc-in grid [y x] val))

(defn in-grid? [grid [x y]]
  (let [[rows cols] (grid-dims grid)]
    (and (>= x 0)
         (< x rows)
         (>= y 0)
         (< y cols))))
