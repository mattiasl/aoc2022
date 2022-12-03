(ns day03
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [intersection]]))

(def rucksacks (split (slurp "./src/main/clojure/input/day03.in") #"\n"))

(defn priority [item]
  (- (int item) (if (#(Character/isUpperCase %) item) 38 96)))

(defn get-common-char [groups]
  (->> (map set groups)
       (apply intersection)
       (first)))                                            ; assume only one common

(defn split-into-compartments [rucksack]
  (partition (/ (count rucksack) 2) rucksack))

(defn priorities [groups]
  (->> (map get-common-char groups)
       (map priority)
       (apply +)))

(defn part-1 [rucksacks]
  (priorities (map split-into-compartments rucksacks)))

(defn part-2 [rucksacks]
  (priorities (partition 3 rucksacks)))

((juxt part-1 part-2) rucksacks)