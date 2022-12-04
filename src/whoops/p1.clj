(ns whoops.p1
  (:require
   [whoops.utils :as utils]
   [clojure.string :as string]))

(def day1-data
  (->> "day1.txt"
      utils/file-lines
      (partition-by string/blank?)
      (remove #(= "" (first %)))
      (map #(map parse-long %))))

(defn elf-calories [elf]
  (reduce + elf))

(defn max-calories [elf]
  (->> elf
       (map elf-calories)
       (apply max)))

(defn p1-1 []
  (max-calories day1-data))

(defn p1-2 []
  (->> day1-data
       (map elf-calories)
       sort
       reverse
       (take 3)
       (reduce +)))

(utils/register-day 1 [p1-1 p1-2])
