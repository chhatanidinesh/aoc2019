(ns problem12.part1
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))


(defn moons-vec
  []
  (let [lines (cs/split-lines (slurp (io/resource "prob12input")))]
    (mapv (fn [line]
           (mapv #(-> (cs/split % #"=" )
                     second
                     (cs/replace #">" "")
                     read-string)
                (cs/split line #",")))
         lines)))


(defn velocity-vec
  [n]
  (mapv (constantly [0 0 0]) (range n)))


(defn get-count
  [index current-moon all]
  (let [gt (filter #(> (get current-moon index) (get % index)) all)
        lt (filter #(< (get current-moon index) (get % index)) all)]
    (- (count lt) (count gt))))


(defn next-velocity
  [moons velocities]
  (let [velocities (mapv
                    (fn [index]
                      (let [current-moon (get moons index)
                            current-velocity (get velocities index)
                            current-change (map #(get-count % current-moon moons) (range 3))
                            res (mapv + current-velocity current-change)]
                        res))
                    (range (count moons)))]
    velocities))


(defn solve
  [steps]
  (loop [step steps
         moons (moons-vec)
         velocities (velocity-vec (count moons))]
    (if (> step 0)
      (let [nvs (next-velocity moons velocities)
            nms (mapv #(mapv + %1 %2) moons nvs)]
        (recur (dec step) nms nvs))
      (reduce +
              (mapv (fn [moon velocity]
                      (let [msum (apply + (map #(Math/abs %) moon))
                            vsum (apply + (map #(Math/abs %) velocity))]
                        (* msum vsum)))
                    moons
                    velocities)))))
