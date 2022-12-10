(ns day09
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day09.in") #"\n"))

(def dirs {"U" [0 -1] "D" [0 1] "L" [-1 0] "R" [1 0]})
(def no-move-zone (for [y (range 1 -2 -1) x (range 1 -2 -1)] [x y]))

(defn move? [h t]
  (not (contains? (set (mapv #(mapv + h %) no-move-zone)) t)))

(defn id [^long z]
  (if (zero? z) 0 (/ (Math/abs z) z)))

(defn new-knot-pos [previous current]
  (let [delta (->> (map - previous current)
                   (map id))]
    (if (move? previous current)
      (map + current delta)
      current)))

(defn move [state move]
  (let [parts (split move #" ")
        dir (dirs (first parts))]
    (loop [state state
           n (Integer/parseInt (last parts))]
      (if (= 0 n)
        state
        (let [knots (:knots state)
              head' (map + (first knots) dir)
              knots' (reduce #(conj %1 (new-knot-pos (last %1) %2))
                             [head']
                             (rest knots))
              visited' (conj (:visited state) (last knots'))]
          (recur {:knots knots' :visited visited'} (dec n)))))))

(defn solver [moves knots]
  (let [knot '(0 0)
        state {:knots (repeat knots knot) :visited #{knot}}]
    (->> moves
         (reduce #(move %1 %2) state)
         (:visited)
         (count))))

(solver input 2)
(solver input 10)