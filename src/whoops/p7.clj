(ns whoops.p7
  (:require [whoops.utils :as utils]
            [clojure.string :as string]))

(defn get-directory [state dir]
  (get-in state (concat [:filesystem] (:pwd state) [dir])))

(defn create-directory [state dir]
  (if (get-directory state dir)
    state
    (assoc-in state (concat [:filesystem] (:pwd state) [dir]) {})))

(defn add-file [state file size]
  (assoc-in state (concat [:filesystem] (:pwd state) [file]) (parse-long size)))

(defmulti parse-line
  (fn [_state [cmd]]
    cmd))

(defn cd [state dir]
  (cond
    (= "/" dir) (assoc state :pwd [])
    (= ".." dir) (update state :pwd pop)
    :else (update state :pwd conj dir)))

(defmethod parse-line "$"
  [state [_ cmd arg]]
  (cond
    (= cmd "cd") (cd state arg)
    (= cmd "ls") state
    :else (throw (Exception. "Unknown command"))))

(defmethod parse-line "dir"
  [state [_ dir]]
  (create-directory state dir))

(defmethod parse-line :default
  [state [size file]]
  (add-file state file size))

(declare directory-size)

(defn file-size [file]
  (if (map? file)
    (directory-size file)
    file))

(defn directory-size [dir]
  (->> (keys dir)
       (map dir)
       (map file-size)
       (reduce +)))

(def day7-data
  (->> "day7.txt"
       utils/file-lines
       (map #(string/split % #" "))
       (reduce parse-line {:pwd []})))

(defn sum-small-directories [acc dir]
  (let [files (vals dir)]
    (reduce (fn [acc file]
              (if (map? file)
                (let [dir-size (directory-size file)
                      acc (if (<= dir-size 100000) (+ acc dir-size) acc)]
                  (sum-small-directories acc file))
                acc))
            acc files)))

(defn find-smallest-directory [acc dir target]
  (let [files (vals dir)]
    (reduce (fn [acc file]
              (if (map? file)
                (let [dir-size (directory-size file)
                      acc (if (> acc dir-size target) dir-size acc)]
                  (find-smallest-directory acc file target))
                acc))
            acc
            files)))

(defn p1 []
  (sum-small-directories 0 (:filesystem day7-data)))

(defn p2 []
  (let [disk-size 70000000
        space-needed 30000000
        space-used (directory-size (:filesystem day7-data))
        free-space (- disk-size space-used)
        gap (- space-needed free-space)]
    (find-smallest-directory disk-size (:filesystem day7-data) gap)))

(utils/register-day 7 [p1 p2])
