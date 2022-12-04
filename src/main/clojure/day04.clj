(ns day04
  (:require [clojure.string :refer [split]]))

(def assignments (split (slurp "./src/main/clojure/input/day04.in") #"\n"))

(defn fully-contains? [[a b c d]]
  (or (<= a c d b) (<= c a b d)))

(defn overlaps? [[a b c d]]
  (and (<= a d) (<= c b)))

(defn parse-assignment [assignment]
  (->> (split assignment #"\D")
       (map #(Integer/parseInt %))))

(defn solve [assignments fn?]
  (->> (map parse-assignment assignments)
       (filter fn?)
       (count)))

(solve assignments fully-contains?)
(solve assignments overlaps?)