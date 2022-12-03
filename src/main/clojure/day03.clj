(ns day03
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [intersection]]))

(def rucksacks (split (slurp "./src/main/clojure/input/day03.in") #"\n"))

(defn score [c]
  (- (int c) (if (#(Character/isUpperCase %) c) 38 96)))

(defn scores [groups]
  (->> (map set groups)
       (apply intersection)
       (first)
       (score)))

(defn split-into-compartments [rucksack]
  (partition (/ (count rucksack) 2) rucksack))

(defn solve [groups]
  (->> (map scores groups)
       (apply +)))

(defn part-1 [rucksacks]
  (solve (map split-into-compartments rucksacks)))

(defn part-2 [rucksacks]
  (solve (partition 3 rucksacks)))

((juxt part-1 part-2) rucksacks)