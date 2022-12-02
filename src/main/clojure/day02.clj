(ns day02
  (:require [clojure.string :refer [split]]))

(def strategy-guide (split (slurp "./src/main/clojure/input/day02.in") #"\n"))
(def strategy-1 {"A X" (+ 1 3) "A Y" (+ 2 6) "A Z" (+ 3 0) "B X" (+ 1 0) "B Y" (+ 2 3) "B Z" (+ 3 6) "C X" (+ 1 6) "C Y" (+ 2 0) "C Z" (+ 3 3)})
(def strategy-2 {"A X" (+ 3 0) "A Y" (+ 1 3) "A Z" (+ 2 6) "B X" (+ 1 0) "B Y" (+ 2 3) "B Z" (+ 3 6) "C X" (+ 2 0) "C Y" (+ 3 3) "C Z" (+ 1 6)})

(defn scores [input score] (reduce (fn [a c] (+ a (score c))) 0 input))

(scores strategy-guide strategy-1)
(scores strategy-guide strategy-2)