(ns problem2.part1
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.set :as cset]))


(defn evaluator
  [x y]
  (with-open [fd (io/reader (io/resource "progfile"))]
    (let [line (binding [*in* fd] (read-line))
          prog (cs/split line #",")
          prog (map #(Integer/parseInt %) prog)
          prog-map (apply hash-map (interleave (range) prog))
          prog-map (assoc prog-map 1 x 2 y)
          h-result (reduce
                        (fn [acc operator-index]
                          (let [operator (get acc operator-index)
                                op-1 (get acc (get acc (+ operator-index 1)))
                                op-2 (get acc (get acc (+ operator-index 2)))
                                res-pos (get acc (+ operator-index 3))]
                            (if (= 99 operator)
                              (reduced acc)
                              (let [res (condp = operator
                                          1 (+ op-1 op-2)
                                          2 (* op-1 op-2))]
                                (assoc acc res-pos res)))))
                        prog-map
                        (take-nth 4 (range (count prog))))]
      (first (map #(get h-result %) (range (count h-result)))))))
