(ns day12
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [union]]
            [clojure.data.priority-map :refer [priority-map]]))

(def input (split (slurp "./src/main/clojure/input/day12.in") #"\n"))

(def elevations
  (-> (into {} (map-indexed (fn [i c] [c i])) "abcdefghijklmnopqrstuvwxyz")
      (assoc \S 0)
      (assoc \E 25)))

(def inf (Integer/MAX_VALUE))

(defn grid-size [input]
  [(count (first input)) (count input)])

(defn get-neighbours [[xm ym] [x y]]
  (let [dirs [[0 -1] [1 0] [0 1] [-1 0]]]
    (filter
      (fn [[x y]] (and (< -1 x xm) (< -1 y ym)))
      (mapv #(mapv + [x y] %) dirs))))

(defn get-valid-neighbours [max source grid]
  (->> (get-neighbours max source)
       (filter (fn [target] (let [[et es] (map #(elevations (grid %1)) [target source])]
                              (<= et (inc es)))))))

(defn input->grid [input]
  (let [[xm ym] (grid-size input)
        elevation (mapv (partial char-array) input)]
    (->> (for [y (range ym) x (range xm)] [x y])
         (reduce (fn [a [x y]] (assoc a [x y] (get (get elevation y) x))) {}))))

(defn pos-by-height [grid heights]
  (let [inverted (clojure.set/map-invert grid)]
    (map inverted heights)))

(defn backtrack [target previous]
  (loop [s '() u target]
    (if (nil? u)
      s
      (recur (conj s u) (get previous u)))))

(defn dijkstra [grid source target size]
  (loop [q (priority-map source 0)
         state {:distance {source 0}
                :previous {}}]
    (if (empty? q)
      false
      (let [[u _] (peek q)
            q' (pop q)]
        (if (= u target)
          (backtrack target (get state :previous))
          (let [result (reduce (fn [a neighbour]
                                 (let [alt (+ (or (get (get state :distance) u) inf) 1)
                                       dist (or (get (get state :distance) neighbour) inf)]
                                   (if (< alt dist)
                                     (merge-with union a {:distance {neighbour alt} :previous {neighbour u}})
                                     a)))
                               {:distance {} :previous {}}
                               (get-valid-neighbours size u grid))]
            (recur (into q' (get result :distance)) (merge-with union state result))))))))

(defn part-1 [input]
  (let [grid (input->grid input)
        [S E] (pos-by-height grid [\S \E])
        size (grid-size input)]
    (->> (dijkstra grid S E size)
         (count)
         (dec))))

; TODO run from E to S and search for first a instead of testing all combinations and selecting the shortest one
(defn part-2 [input]
  (let [grid (input->grid input)
        [S E] (pos-by-height grid [\S \E])
        size (grid-size input)
        grid' (assoc grid S \a)
        starts (->> grid'
                    (filter (fn [[_ v]] (= \a v)))
                    (keys))]
    (->> (reduce (fn [a S]
                   (let [steps (dijkstra grid' S E size)]
                     (if (coll? steps)
                       (conj a (count steps))
                       a)))
                 #{} starts)
         (apply min)
         (dec))))

(comment
  (part-1 input)
  (part-2 input)
  )