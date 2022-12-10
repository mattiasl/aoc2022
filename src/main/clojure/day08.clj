(ns day08
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day08.in") #"\n"))

(defn to-int-vec [row]
  (mapv (fn [x] (Character/digit x 10)) row))

(defn grid-size [input]
  [(count (first input)) (count input)])

(defn input->grid [input]
  (let [[xm ym] (grid-size input)
        height (mapv to-int-vec input)]
    (->> (for [y (range ym) x (range xm)] [x y])
         (reduce (fn [a [x y]] (assoc a [x y] (get (get height y) x))) {}))))

(defn get-directions [[x y] [xm ym]]
  [(for [y (range (dec y) -1 -1)] [x y])
   (for [y (range (inc y) ym)] [x y])
   (for [x (range (dec x) -1 -1)] [x y])
   (for [x (range (inc x) xm)] [x y]) ])

(defn tallest-until-edge? [height grid trees]
  (->> trees
       (map #(grid %))
       (filter #(>= % height))
       (empty?)))

(defn sees-edge? [[[x y] height] [xm ym] grid]
  (let [[up down left right] (get-directions [x y] [xm ym])]
    (cond
      (or (= x 0) (= y 0) (= x xm) (= y ym)) true
      (or (tallest-until-edge? height grid up)
          (tallest-until-edge? height grid down)
          (tallest-until-edge? height grid left)
          (tallest-until-edge? height grid right)) true
      :else
      false)))

(defn count-visible-trees [height trees grid]
  (loop [visible-trees 0
         [tree & remaining-trees] trees]
    (cond
      (nil? tree) visible-trees
      (>= (grid tree) height) (inc visible-trees)
      :else
      (recur (inc visible-trees) remaining-trees))))

(defn scenic-score [[[x y] height] [xm ym] grid]
  (->> (get-directions [x y] [xm ym])
       (map #(count-visible-trees height % grid))
       (apply *)))

(defn part-1 [input]
  (let [grid (input->grid input)]
    (->> grid
         (map #(sees-edge? % (grid-size input) grid))
         (filter true?)
         (count))))

(defn part-2 [input]
  (let [grid (input->grid input)]
    (->> grid
         (map #(scenic-score % (grid-size input) grid))
         (apply max))))

(part-1 input)
(part-2 input)