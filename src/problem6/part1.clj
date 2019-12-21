(ns problem6.part1
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.set :as cset]))

(def reach-count
  (memoize (fn [r-list vertex]
             (if (= vertex :COM)
               0
               (+ 1 (reach-count r-list (get r-list vertex)))))))


(defn find-total-links
  []
  (let [u-edges (slurp (io/resource "orbits.txt"))
        edges (cs/split u-edges #"\n")
        r-list (reduce
                  (fn [res edge]
                    (let [[l r] (cs/split edge (re-pattern "\\)"))]
                      (assoc res (keyword r) (keyword l))))
                  {}
                  edges)]
    (reduce +
            0
            (map (fn [vertex]
                   (reach-count r-list vertex))
                 (keys r-list)))))
