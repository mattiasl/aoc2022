(ns day14
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day14.in") #"\n"))

(defn parse-point [point]
  (->> (split point #",")
       (map #(Integer/parseInt %1))))

(defn make-rocks [points]
  (->> (partition 2 1 points)
       (map (fn [[[x1 y1] [x2 y2]]]
              (let [[x1 x2] ((juxt min max) x1 x2)
                    [y1 y2] ((juxt min max) y1 y2)]
                (set (for [y (range y1 (inc y2)) x (range x1 (inc x2))] [x y])))))
       (reduce clojure.set/union)))

(defn parse-rock [row]
  (->> (split row #" -> ")
       (map parse-point)
       (make-rocks)))

(defn get-candidates [[x y]]
  (let [dirs [[0 1] [-1 1] [1 1]]]
    (mapv #(mapv + [x y] %) dirs)))

(defn simulate-sand-part1 [rocks start y-max]
  (loop [sand #{}
         pos start]
    (let [next-pos (->> (get-candidates pos)
                        (filter #(not (or (contains? rocks %1)
                                          (contains? sand %1))))
                        (first))]
      (cond
        (nil? next-pos) (recur (conj sand pos) start)
        (> (last next-pos) y-max) sand
        :else
        (recur sand next-pos)))))

(defn simulate-sand-part2 [rocks start y-max]
  (loop [sand #{}
         pos start]
    (let [next-pos (->> (get-candidates pos)
                        (filter #(not (or (contains? rocks %1)
                                          (contains? sand %1))))
                        (filter (fn [[_ y]] (< y (+ y-max 2))))
                        (first))]
      (cond
        (and (= pos start) (nil? next-pos)) (conj sand pos)
        (nil? next-pos) (recur (conj sand pos) start)
        :else
        (recur sand next-pos)))))

(defn solver [input sand-simulator]
  (let [rocks (->> (map parse-rock input)
                   (reduce clojure.set/union))
        y-max (->> rocks
                   (map second)
                   (apply max))]
    (->> (sand-simulator rocks [500 0] y-max)
         (count))))

(comment
  (solver input simulate-sand-part1)
  (solver input simulate-sand-part2)
  )