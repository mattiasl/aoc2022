(ns day13
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day13.in") #"\n\n"))

(defn str->packet [packet]
  (first (mapv clojure.edn/read-string [packet])))

(defn compare-pair [[left right]]
  (loop [left left
         right right]
    (let [l (first left)
          r (first right)]
      (cond
        (and (nil? l) (nil? r)) nil
        (nil? l) true
        (nil? r) false
        :else
        (cond
          (and (number? l) (number? r)) (cond
                                          (< l r) true
                                          (> l r) false
                                          :else
                                          (recur (rest left) (rest right)))
          (and (vector? l) (vector? r)) (let [answer (compare-pair [l r])]
                                          (if (nil? answer)
                                            (recur (rest left) (rest right))
                                            answer))
          (and (vector? l) (number? r)) (recur left (into [] (conj (rest right) [r])))
          (and (number? l) (vector? r)) (recur (into [] (conj (rest left) [l])) right))))))

(defn part-1 [input]
  (->> (reduce (fn [a c] (let [[left right] (->> (split c #"\n")
                                                 (map str->packet))]
                           (conj a [left right])
                           )) [] input)
       (map compare-pair)
       (map-indexed (fn [idx itm] (if itm (inc idx) 0)))
       (reduce +)))

(defn part-2 [input]
  (let [separators ["[[2]]" "[[6]]"]
        packets (->> (reduce (fn [a c] (let [[left right] (->> (split c #"\n")
                                                               (map str->packet))]
                                         (conj a left right)
                                         )) (map str->packet separators) input)
                     (sort #(if (compare-pair [%1 %2]) -1 1))
                     (map-indexed (fn [idx itm] {(str itm) (inc idx)}))
                     (apply merge))]
    (->> (select-keys packets separators)
         (vals)
         (reduce *))))

(part-1 input)
(part-2 input)