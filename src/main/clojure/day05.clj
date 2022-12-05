(ns day05
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day05.in") #"\n\n"))

(defn crates-by-index [row]
  (map-indexed (fn [i x] [(inc i) x]) (take-nth 4 (rest row))))

(defn make-empty-stacks [indexes]
  (->> (re-seq #"[\d]+" indexes)
       (map #(Integer/parseInt %))
       (reduce (fn [a c] (assoc a c [])) (sorted-map))))

(defn add-crates [stacks crates]
  (reduce (fn [acc [i crate]]
            (if (= crate \ )
              acc
              (assoc acc i (conj (acc i) crate))))
          stacks crates))

(defn make-stacks [initial-state]
  (let [instr (split initial-state #"\n")]
    (->> (butlast instr)
         (reverse)
         (map crates-by-index)
         (reduce add-crates (make-empty-stacks (last instr))))))

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
    (->> (reduce procedure (make-stacks stacks) (parse-procedures procedures))
         (vals)
         (map last)
         (clojure.string/join))))

(= (run-procedure input reverse) "WCZTHTMPS")               ; part-1
(= (run-procedure input identity) "BLSGJSDTS")              ; part-2