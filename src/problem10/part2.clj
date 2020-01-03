(ns problem10.part2
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
  (let [theta (Math/atan2 (- y2 y1)
                          (- x2 x1))
        degrees (Math/toDegrees theta)
        degrees-shifted (+ degrees 90)]
    (if (< degrees-shifted 0)
      (+ degrees 450)
      degrees-shifted)))


(defn distance
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2))))


(defn solve
  []
  (let [ac (asteroid-coordinates)
        angles (for [x ac
                     y ac
                     :when (not= x y)]
                 [x (angle x y)])
        angles-map (map (fn [[k v]]
                          {k [v]}) angles)
        p-angle-map (apply merge-with (comp distinct
                                            concat) angles-map)
        counts-map (reduce-kv
                    (fn [acc k v]
                      (assoc acc k [(count v) v] )
                      )
                    {}
                    p-angle-map)]
    (apply max-key first (map (fn [[k v]] [(first v) k]) counts-map ))))


(defn rotate-and-get-nth
  [angle-map c]
  (loop [am (vec angle-map)
         n 1]
    (let [[k v] (first am)]
      (if (and (= n c) (seq v))
        (let [[x y] (-> v
                        first
                        second)]
          (+ (* 100 x) y))
        (if (seq v)
          (recur (vec (rest (conj am [k (rest v)]))) (inc n))
          (recur (vec (rest (conj am [k v]))) n))))))


(defn part-2
  []
  (let [best-postion (-> (solve) second)
        ac (asteroid-coordinates)
        angles (for [x [best-postion]
                     y ac
                     :when (not= x y)]
                 {(angle x y) [(distance x y) y]})
        angle-map (apply merge-with concat angles)
        sorted-angle-map (into (sorted-map) angle-map)
        sorted-angle-map-distance (reduce-kv (fn [acc k v]
                                               (assoc acc k (apply sorted-map v)))
                                             (sorted-map)
                                             sorted-angle-map)]
    (rotate-and-get-nth sorted-angle-map-distance 200)))
