(ns day06)

(defn solver [packet-size]
  (fn loop
    ([buffer] (loop packet-size buffer))
    ([index [packet & buffer]]
     (if (= (count (set packet)) packet-size)
       index
       (recur (inc index) buffer)))))

(map #((solver %) (partition % 1 (slurp "./src/main/clojure/input/day06.in"))) [4 14])