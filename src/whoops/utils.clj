(ns whoops.utils
  (:require [clojure.java.io :as io]))

(defn file-lines [filename]
  (let [file-path (str "files/" filename)]
    (with-open [read (io/reader file-path)]
      (doall (line-seq read)))))
