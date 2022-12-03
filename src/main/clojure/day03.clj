(ns day03
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [intersection]]))

(def rucksacks (split (slurp "./src/main/clojure/input/day03.in") #"\n"))

(def priority (into {} (map-indexed (fn [i c] [c i])) "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn get-common-chars [groups]
  (->> (map set groups)
       (apply intersection)
       (into [])))

(defn priorities [groups]
  (->> (map get-common-chars groups)
       (flatten)
       (map priority)
       (reduce +)))

(defn part-1 [rucksacks]
  (priorities (map #(partition (/ (count %) 2) %) rucksacks)))

(defn part-2 [rucksacks]
  (priorities (partition 3 rucksacks)))

((juxt part-1 part-2) rucksacks)