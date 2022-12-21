(ns whoops.p21
  (:require
   [clojure.string :as string]
   [whoops.utils :as utils]))

(def str->fn
  {"+" +
   "-" -
   "*" *
   "/" /})

(defn parse-fn [fn-str]
  (if-let [result (parse-long fn-str)]
    [result]
    (let [[m1 op m2] (string/split fn-str #" ")]
      [(str->fn op) m1 m2])))

(defn parse-line [line]
  (let [[monkey fn-str] (string/split line #": ")]
    [monkey (parse-fn fn-str)]))

(def day21-data
  (->> "day21.txt"
       utils/file-lines
       (map parse-line)
       (into {})))

(defn solve-monkey [monkeys monkey human?]
  (let [[op m1 m2] (monkeys monkey)]
    (if (number? op)
      op
      (let [m1 (if (and human? (= "humn" m1))
                 :humn
                 (solve-monkey monkeys m1 human?))
            m2 (if (and human? (= "humn" m2))
                 :humn
                 (solve-monkey monkeys m2 human?))]
        [op m1 m2]))))

(defn reduce-expression [expr]
  (if (or (number? expr) (= :humn expr))
    expr
    (let [[op m1 m2] expr
          m1 (reduce-expression m1)
          m2 (reduce-expression m2)]
      (if (or (= :humn m1) (= :humn m2) (vector? m1) (vector? m2))
        [op m1 m2]
        (op m1 m2)))))

(defn resolve-human-target [expr target]
  (if (= expr :humn)
    target
    (let [[op m1 m2] expr]
      (cond
        (and (= op +) (number? m1)) (resolve-human-target m2 (- target m1))
        (and (= op +) (number? m2)) (resolve-human-target m1 (- target m2))
        (and (= op -) (number? m1)) (resolve-human-target m2 (- m1 target))
        (and (= op -) (number? m2)) (resolve-human-target m1 (+ target m2))
        (and (= op *) (number? m1)) (resolve-human-target m2 (/ target m1))
        (and (= op *) (number? m2)) (resolve-human-target m1 (/ target m2))
        (and (= op /) (number? m1)) (resolve-human-target m2 (/ m1 target))
        (and (= op /) (number? m2)) (resolve-human-target m1 (* m2 target))))))

(defn resolve-human [[_op expr1 expr2]]
  (if (number? expr1)
    (resolve-human-target expr2 expr1)
    (resolve-human-target expr1 expr2)))

(defn p1 []
  (-> day21-data
      (solve-monkey "root" false)
      reduce-expression))

(defn p2 []
  (-> day21-data
      (solve-monkey "root" true)
      reduce-expression
      resolve-human))

(utils/register-day 21 [p1 p2])
