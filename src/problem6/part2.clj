(ns problem6.part2
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.set :as cset]))


(def reach-path
  (memoize (fn [r-list vertex]
             (if (= vertex :COM)
               (list :COM)
               (conj (reach-path r-list (get r-list vertex)) vertex)))))



(defn find-total-links
  []
  (let [u-edges (slurp (io/resource "orbits.txt"))
        edges (cs/split u-edges #"\n")
        r-list (reduce
                  (fn [res edge]
                    (let [[l r] (cs/split edge (re-pattern "\\)"))]
                      (assoc res (keyword r) (keyword l))))
                  {}
                  edges)
        you-path (reach-path r-list :YOU)
        san-path (reach-path r-list :SAN)
        int-point (first (filter (set san-path) you-path))
        sum (reduce (fn [acc point]
                      (if (= point int-point)
                        (reduced acc)
                        (inc acc)))
                    0
                    you-path)
        n-sum (reduce (fn [acc point]
                        (if (= point int-point)
                          (reduced acc)
                          (inc acc)))
                      sum
                      san-path)]

    (- n-sum 2)))
