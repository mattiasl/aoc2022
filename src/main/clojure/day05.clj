(ns day05
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day05.in") #"\n\n"))

(defn transpose [m]
  (apply mapv vector m))

(defn init-stacks [initial-state]
  (->> (split initial-state #"\n")
       (transpose)
       (map reverse)
       (map clojure.string/join)
       (map clojure.string/trim)
       (filter #(re-matches #"\d+[A-Z]+" %))
       (reduce (fn [a c] (assoc a (Character/digit (first c) 10) (rest c))) (sorted-map))))

(defn parse-procedures [procedures]
  (->> (split procedures #"\n")
       (map (fn [x] (re-matches #"move (\d+) from (\d+) to (\d+)" x)))
       (map rest)
       (map (fn [x] (map #(Integer/parseInt %) x)))))

(defn make-procedure [func]
  (fn [crates [n from to]]
    (let [from' (crates from)
          to' (concat (crates to) (func (take-last n from')))]
      (merge crates {from (drop-last n from') to to'}))))

(defn run-procedure [[stacks procedures] fn]
  (let [procedure (make-procedure fn)]
    (->> (reduce procedure (init-stacks stacks) (parse-procedures procedures))
         (vals)
         (map last)
         (clojure.string/join))))

(= (run-procedure input reverse) "WCZTHTMPS")               ; part-1
(= (run-procedure input identity) "BLSGJSDTS")              ; part-2