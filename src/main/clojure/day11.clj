(ns day11
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day11.in") #"\n\n"))

(defn parse-items [row]
  (->> (split row #"\D+")
       (rest)
       (map #(Long/parseLong %1))
       (vec)))

(def operations {"*" * "+" +})

(defn make-operation [row]
  (let [operation (second (re-matches #"  Operation: new = (.*)" row))
        parts (split operation #" ")
        op (operations (second parts))]
    (if (= (last parts) "old")
      (fn [old] (op old old))
      (let [other (Long/parseLong (last parts))]
        (fn [old] (op old other))))))

(defn make-test [divisor]
  (fn [x] (= (mod x divisor) 0)))

(defn get-divisor [row]
  (let [divisible (second (re-matches #"  Test: divisible by (.*)" row))
        parts (split divisible #" ")]
    (Long/parseLong (last parts))))

(defn parse-throw [row]
  (first (parse-items row)))

(defn make-monkeys [monkeys]
  (reduce (fn [a monkey]
            (let [rows (split monkey #"\n")
                  id (Integer/parseInt (second (re-matches #"Monkey (\d+)\:" (first rows))))
                  divisor (get-divisor (nth rows 3))]
              (assoc a id {:dev   divisor
                           :ins   0
                           :items (parse-items (nth rows 1))
                           :op    (make-operation (nth rows 2))
                           :test  (make-test divisor)
                           true   (parse-throw (nth rows 4))
                           false  (parse-throw (nth rows 5))}))
            ) (sorted-map) monkeys))

(defn round [monkey-id monkeys worry-fn]
  (reduce (fn [monkeys' worry]
            (let [monkey (monkeys monkey-id)
                  op-fn (monkey :op)
                  worry' (-> (op-fn worry)
                             (worry-fn))
                  test (monkey :test)
                  destination-monkey (monkey (test worry'))]
              (-> monkeys'
                  (update-in [destination-monkey :items] conj worry')
                  (update-in [monkey-id :ins] inc))))
          (assoc-in monkeys [monkey-id :items] [])
          (get-in monkeys [monkey-id :items])))

(defn make-worry-fn [f arg]
  #(f %1 arg))

(defn get-common-divisor [monkeys]
  (->> monkeys
       (vals)
       (map (fn [x] (vals (select-keys x [:dev]))))
       (flatten)
       (reduce *)))

(defn solver [input rounds part]
  (let [monkeys (make-monkeys input)
        worry-fn (if (= part 1)
                   (make-worry-fn quot 3)
                   (make-worry-fn mod (get-common-divisor monkeys)))]
    (->> (reduce (fn [a _]
                   (reduce (fn [a monkey-id] (round monkey-id a worry-fn)) a (range (count monkeys))))
                 monkeys
                 (range rounds))
         (vals)
         (map (fn [x] (vals (select-keys x [:ins]))))
         (flatten)
         (sort)
         (take-last 2)
         (reduce *))))

(solver input 20 1)
(solver input 10000 2)