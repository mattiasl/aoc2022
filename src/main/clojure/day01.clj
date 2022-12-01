(ns day01
  (:require [clojure.string :refer [split]]))

(def calories (split (slurp "./src/main/clojure/input/day01.in") #"\n"))

(defn calories-by-elf [calories]
  (->> (partition-by #(= % "") calories)
       (filter #(not= % '("")))
       (map (fn [x] (reduce + (map #(Integer/parseInt %) x))))))

;part-1
(->> (calories-by-elf calories)
     (apply max))

;part-2
(->> (calories-by-elf calories)
     (sort >)
     (take 3)
     (apply +))