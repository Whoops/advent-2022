(ns whoops.p17
  (:require
   [whoops.utils :as utils]))

(def sym->dir {\> :right
               \< :left})

(def day17-data
  (->> "day17.txt"
       utils/file-lines
       first
       (map sym->dir)))

(defn reversed-set [& keys]
  (apply sorted-set-by #(compare %2 %1) keys))

(defn new-chamber []
  (vec (repeat 7 (reversed-set 0))))

(def pieces [;; -
             (vec (repeat 4 (sorted-set 0)))
             ;; +
             [(sorted-set 1)
              (sorted-set 0 1 2)
              (sorted-set 1)]
             ;; J
             [(sorted-set 0)
              (sorted-set 0)
              (sorted-set 0 1 2)]
             ;; I
             [(sorted-set 0 1 2 3)]
             ;; square
             [(sorted-set 0 1)
              (sorted-set 0 1)]])

(defn spawn-shape [chamber piece]
  (let [height (->> chamber
                    (map first)
                    (apply max)
                    (+ 4))]
    {:pos [2 height]
     :piece piece}))

(defn settled? [chamber {:keys [pos piece]}]
  (let [[x y] pos
        chamber-cols (subvec chamber x (+ x (count piece)))]
    (boolean
     (some identity
           (map (fn [chamber-col piece-col]
                  (let [piece-height (+ (first piece-col) y)]
                    (boolean (chamber-col (dec piece-height)))))
                chamber-cols
                piece)))))

(def direction->adjust-fn
  {:left dec
   :right inc})

(defn collision? [chamber {:keys [pos piece]}]
  (let [[x y] pos
        chamber-cols (subvec chamber x (+ x (count piece)))]
    (boolean
     (some identity
           (mapcat (fn [chamber-col piece-col]
                     (for [p piece-col]
                       (boolean (chamber-col (+ y p)))))
                   chamber-cols
                   piece)))))

(defn move-shape [chamber {:keys [pos piece] :as shape} direction]
  (let [adjust-fn (direction->adjust-fn direction)
        [x y] pos
        max-x (- 7 (count piece))
        new-x (-> x
                  adjust-fn
                  (max 0)
                  (min max-x))
        new-shape (assoc shape :pos [new-x y])]
    (if (collision? chamber new-shape)
      shape
      new-shape)))

(defn drop-shape [shape]
  (update-in shape [:pos 1] dec))

(defn add-piece-columns [chamber-col {:keys [pos piece]} n]
  (let [[_x y] pos]
    (apply conj chamber-col (map #(+ y %) (nth piece n)))))

(defn update-chamber-column [chamber n {:keys [pos] :as shape}]
  (let [[x _] pos]
    (update chamber (+ x n) #(add-piece-columns % shape n))))

(defn settle [chamber {:keys [piece] :as shape}]
  (let [cols (range (count piece))]
    (reduce #(update-chamber-column %1 %2 shape) chamber cols)))

(defn spawn-shape-if-needed [chamber pieces shape]
  (if shape
    [pieces shape]
    [(rest pieces) (spawn-shape chamber (first pieces))]))

(defn expand-shape [{:keys [pos piece]}]
  (let [[x y] pos
        expanded (concat (repeat x #{}) piece (repeat (- 7 (count piece) x) #{}))]
    (->> expanded
         (map #(map (partial + y) %))
         (mapv #(into #{} %)))))

(defn initial-state [jets]
  {:jets (cycle jets)
   :jet-count (count jets)
   :pieces (cycle pieces)
   :chamber (new-chamber)
   :shape nil
   :rock 0
   :turn 0})

(defn game-step [{:keys [pieces jets chamber shape rock] :as state}]
  (let [[pieces shape] (spawn-shape-if-needed chamber pieces shape)
        shape (move-shape chamber shape (first jets))
        dropped? (settled? chamber shape)
        chamber (if dropped? (settle chamber shape) chamber)
        shape (if dropped? nil (drop-shape shape))
        rock (if dropped? (inc rock) rock)]
    (-> state
        (assoc :pieces pieces :shape shape :chamber chamber :rock rock)
        (update :turn inc)
        (update :jets rest))))

(defn chop-top [chamber n]
  (let [chamber-height (->> chamber
                            (map first)
                            (apply max))
        base (max 0 (- chamber-height n))]
    (->> chamber
         (map #(filter (partial <= base) %))
         (map #(map (partial + (- base)) %)))))

(defn compare-state [state1 state2]
  (let [turn1 (:turn state1)
        turn2 (:turn state2)
        jet-pos1 (mod turn1 (:jet-count state1))
        jet-pos2 (mod turn2 (:jet-count state2))
        next-shape1 (-> state1 :pieces first)
        next-shape2 (-> state2 :pieces first)
        top-chamber1 (chop-top (:chamber state1) 100)
        top-chamber2 (chop-top (:chamber state2) 100)]
    (and
     (= jet-pos1 jet-pos2)
     (= next-shape1 next-shape2)
     (= top-chamber1 top-chamber2))))

(defn state-height [{:keys [chamber]}]
  (->> chamber
       (map first)
       (apply max)))

(defn find-cycle [states state]
  (when (nil? (:shape state))
    (->> states
         (filter #(compare-state % state))
         first)))

(defn simulate-to-cycle [state]
  (loop [state state
         states '()]
    (let [next-state (game-step state)]
      (if-let [base (find-cycle states next-state)]
        [base next-state]
        (recur next-state
               (if (nil? (:shape next-state))
                 (conj states next-state)
                 states))))))

(defn quick-run [jets target]
  (let [[base rep] (simulate-to-cycle (initial-state jets))
        base-height (state-height base)
        rep-height (state-height rep)
        rep-rocks (:rock rep)
        cycle-height (- rep-height base-height)
        cycle-rocks (- (:rock rep) (:rock base))
        rocks-remaining (- target rep-rocks)
        cycles-remaining (long (/ rocks-remaining cycle-rocks))
        rocks-after (mod rocks-remaining cycle-rocks)
        cycle-height-added (* cycle-height cycles-remaining)
        remainder-height (->> rep
                              (iterate game-step)
                              (drop-while #(> (+ rep-rocks rocks-after) (:rock %)))
                              first
                              state-height
                              (+ (- rep-height)))]
    (+ rep-height cycle-height-added remainder-height)))

(defn p1 []
  (->> day17-data
       initial-state
       (iterate game-step)
       (drop-while #(> 2022 (:rock %)))
       first
       state-height))

(defn p2 []
  (quick-run day17-data 1000000000000))

(utils/register-day 17 [p1 p2])
