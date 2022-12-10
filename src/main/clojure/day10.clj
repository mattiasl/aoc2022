(ns day10
  (:require [clojure.string :refer [split]]))

(def input (split (slurp "./src/main/clojure/input/day10.in") #"\n"))

(defn noop [c x _]
  {:x x :c (inc c) :r {(inc c) x}})

(defn addx [c x arg]
  {:x (+ x arg) :c (+ c 2) :r {(inc c) x (+ c 2) x}})

(defn parse-instruction [line]
  (cond
    (= line "noop") [noop nil]
    :else
    [addx (->> (split line #" ")
               (last)
               (Integer/parseInt))]))

(defn run [program]
  (loop [[line & lines] program
         state {:x 1 :c 0}
         result {}]
    (if (nil? line)
      result
      (let [[fn arg] (parse-instruction line)
            state (fn (:c state) (:x state) arg)]
        (recur lines state (merge result (:r state)))))))

(defn part-1 [input]
  (->> (run input)
       (filter (fn [[k _]] (contains? #{20 60 100 140 180 220} k)))
       (map (fn [[k v]] (* k v)))
       (reduce +)))

(defn transpose [m]
  (apply mapv vector m))

(defn get-CHAR-key [x]
  (reduce (fn [a c] (str a (apply str c))) "" x))

(def CHAR->char {" ######  #  #  #   #####      " "A"
                 "####### #  ## #  ##    #      " "E"
                 " #### #    ##  # # # ###      " "G"
                 "######     #     #     #      " "L"
                 "#######  #  #  #   ##         " "P"
                 "#####      #     ######       " "U"})

(defn get-character [x]
  (if (contains? CHAR->char x)
    (CHAR->char x)
    (->> (partition 6 x)
         (transpose)
         (reduce (fn [a c] (str a (apply str c) "\n")) "")
         (println)
         (println x))))

(defn crt->chars [crt]
  (->> (transpose crt)
       (partition 5)
       (map get-CHAR-key)
       (map get-character)))

(defn part-2 [input]
  (let [sprites (run input)
        crt (->> (for [c (range 0 240)] c)
                 (reduce (fn [a cycle]
                           (let [sprite (sprites (inc cycle))
                                 pixel (mod cycle 40)]
                             (if (<= (dec sprite) pixel (inc sprite))
                               (str a "#")
                               (str a " ")))
                           ) "")
                 (partition 40)
                 (map clojure.string/join))
        _ (println (->> (crt->chars crt)
                        (clojure.string/join)))]
    crt))

(part-1 input)
(part-2 input)