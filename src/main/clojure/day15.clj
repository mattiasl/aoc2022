(ns day15
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day15.in") #"\n"))

(defn manhattan [[^long x1 ^long y1] [^long x2 ^long y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn parse-row [row]
  (let [longs (->> (re-seq #"-?\d+" row)
                   (map #(Long/parseLong %1)))
        [sx sy bx by] longs]
    {:s [sx sy] :b [bx by] :d (manhattan [sx sy] [bx by])}))

(defn get-xs-at-line [{:keys [s d]} ^long y-line]
  (let [[^long sx ^long sy] ^long s
        from-sensor-to-line (Math/abs (- sy y-line))
        overflow (Math/abs (- d from-sensor-to-line))]
    (if (<= from-sensor-to-line d)
      (into #{} (for [x' (range (inc (* 2 overflow)))] (+ (- sx overflow) x')))
      #{})))

(defn part-1 [input y]
  (let [state (map parse-row input)
        beacons-to-remove (->> (map #(%1 :b) state)
                               (filter (fn [[_ y']] (= y y')))
                               (map #(first %1))
                               (set))]
    (as-> state $
          (pmap #(get-xs-at-line %1 y) $)
          (reduce (fn [a c] (clojure.set/union a (set c))) #{} $)
          (apply disj $ beacons-to-remove)
          (count $))))

(defn in-space? [[x y] [xm ym]]
  (and (< -1 x xm) (< -1 y ym)))

(defn not-reachable? [point sensor]
  (let [mhd (sensor :d)
        sensor-point (sensor :s)]
    (> (manhattan point sensor-point) mhd)))

(defn valid-point? [sensors point]
  (->> sensors
       (filter (fn [x] (not-reachable? point x)))
       (count)
       (= (count sensors))))

(defn find-distress-beacons [max sensors sensor]
  (let [mhd (inc (sensor :d))
        [x y] (sensor :s)
        [lx rx uy dy] [(- x mhd) (+ x mhd) (- y mhd) (+ y mhd)]
        lu (->> (map vector (range lx (inc x)) (reverse (range uy (inc y))))
                (filter (fn [z] (in-space? z max)))
                (filter (partial valid-point? sensors)))
        ur (->> (map vector (range x (inc rx)) (range uy (inc y)))
                (filter (fn [z] (in-space? z max)))
                (filter (partial valid-point? sensors)))
        rd (->> (map vector (reverse (range x (inc rx))) (range y (inc dy)))
                (filter (fn [z] (in-space? z max)))
                (filter (partial valid-point? sensors)))
        dl (->> (map vector (reverse (range lx (inc x))) (reverse (range y (inc dy))))
                (filter (fn [z] (in-space? z max)))
                (filter (partial valid-point? sensors)))
        ]
    (concat lu ur rd dl)))

(defn tuning-frequency [[x y]]
  (+ (* 4000000 x) y))

(defn part-2 [input max]
  (let [sensors (map parse-row input)]
    (->> sensors
         (pmap (partial find-distress-beacons max sensors))
         (filter (fn [x] (not (empty? x))))
         (first)
         (first)
         (tuning-frequency))))

(comment
  ;4985193
  (time (part-1 input 2000000))
  ;11583882601918
  (time (part-2 input [4000000 4000000]))
  )
