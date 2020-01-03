(ns problem10.part1
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))


(defn get-coordinate
  [n  cols]
  (let [row-num (quot n cols)]
    [(- n (* cols row-num)) row-num]))


(defn asteroid-coordinates
  []
  (let [lines (cs/split-lines (slurp (io/resource "prob10input")))
        cols (count (first lines))
        space (map-indexed vector (mapcat identity lines))
        asteroids (keep identity (map (fn [v]
                                        (when (= (second v) \#)
                                          (get-coordinate (first v) cols)))
                                      space))]
    asteroids))


(defn angle
  [[x1 y1] [x2 y2]]
  (let [theta (Math/atan2 (- x2 x1)
                          (- y2 y1))
        degrees (Math/toDegrees theta)]
    (if (< degrees 0)
      (+ degrees 360)
      degrees)))

(defn solve
  []
  (let [ac (asteroid-coordinates)
        angles (for [x ac
                     y ac
                     :when (not= x y)]
                 [x (angle x y)])
        angles-map (map (fn [[k v]]
                          {k [v]}) angles)
        p-angle-map (apply merge-with (comp distinct concat) angles-map)
        counts (reduce-kv
                (fn [acc k v]
                  (assoc acc k [(count v) v] )
                  )
                {}
                p-angle-map)]
    (apply max (map (fn [[__ v]] (first v)) counts ))))
