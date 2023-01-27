(ns day21
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day21.in") #"\n"))

(def char->operation {"+" + "/" / "-" - "*" *})

(defn parse-input [input]
  (reduce (fn [a row]
            (let [[k exp] (split row #"\: ")]
              (if (re-matches #"\d+" exp)
                (assoc a k (Integer/parseInt exp))
                (assoc a k (split exp #" ")))))
          {} input))

(defn monkey-yell [monkeys monkey]
  (let [numOrExpr (monkeys monkey)]
    (if (number? numOrExpr)
      numOrExpr
      (let [[left op right] numOrExpr]
        (->> (map #(monkey-yell monkeys %) [left right])
             (apply (partial (char->operation op))))))))

(defn human-yell [low high target monkey monkeys]
  (loop [low low
         high high]
    (let [mid (quot (+ low high) 2)
          target?? (monkey-yell (assoc monkeys "humn" mid) monkey)]
      (cond (= target target??) mid
            ; depending on the derivative sign of the function of monkeys the < and the > may need swapping
            (> target target??) (recur low (dec mid))
            (< target target??) (recur (inc mid) high)))))

(comment
  ;part 1
  (time (monkey-yell (parse-input input) "root"))

  ;part 2
  (let [monkeys (parse-input input)
        [left _ right] (monkeys "root")
        target (monkey-yell monkeys right)]
    (time (human-yell 0 10000000000000 target left monkeys)))
  )