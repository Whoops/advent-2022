(ns whoops.aoc2022
  (:require
   [whoops.p1]
   [whoops.p2]
   [whoops.p3]
   [whoops.p4]
   [whoops.p5]
   [whoops.p6]
   [whoops.p7]
   [whoops.p8]
   [whoops.p9]
   [whoops.p10]
   [whoops.p11]
   [whoops.utils :as utils])
  (:gen-class))

(defn print-day [day]
  (when-let [problems (@utils/days day)]
    (println "----------")
    (println (str "DAY " day))
    (time (println "Problem 1:" ((first problems))))
    (time (println "Problem 2:" ((second problems))))
    (println "----------")))

(defn -main
  "I don't do a whole lot ... yet."
  [& _args]
  (doseq [day (-> @utils/days keys sort)]
    (print-day day)))
