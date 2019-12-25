(ns problem7.part1
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.math.combinatorics :as cmc]
            [clojure.set :as cset]))




(defn get-mode-of-operand
  [n op-num]
  (loop [n n
         op-num op-num
         res 0]
    (if (> op-num 0)
      (let [res (mod n 10)
            n (quot n 10)]
        (recur n (dec op-num) res))
      res)))


(defn get-opcode
  [n]
  (mod n 100))

(defn get-operand-value
  [input mode value]
  (if (= mode 1)
    value
    (get input value)))

(defn get-operands
  [input curr-index]
  (let [curr-value (get input curr-index)
        op1 (get input (+ curr-index 1))
        op2 (get input (+ curr-index 2))
        op1-mode (get-mode-of-operand (quot curr-value 100) 1)
        op2-mode (get-mode-of-operand (quot curr-value 100) 2)
        op1-value (get-operand-value input op1-mode op1)
        op2-value (get-operand-value input op2-mode op2)]
    [op1-value op2-value]))


(defn add-or-mul
  [input curr-index op-code]
  (let [res-add (get input (+ curr-index 3))
        [op1-value op2-value] (get-operands input curr-index)
        res (if (= op-code 2)
              (* op1-value op2-value)
              (+ op1-value op2-value))]
    (assoc input res-add res)))


(defn jump
  [input curr-index op-code]
  (let [[op1-value op2-value] (get-operands input curr-index)]
    (if (= op-code 5)
      (do
        (if (zero? op1-value)
          (+ curr-index 3)
          op2-value))
      (do
        (if (zero? op1-value)
          op2-value
          (+ curr-index 3))))))


(defn comparision
  [input curr-index op-code]
  (let [res-add (get input (+ curr-index 3))
        [op1-value op2-value] (get-operands input curr-index )
        res (if (= op-code 7)
              (if (< op1-value op2-value)
                (assoc input res-add 1)
                (assoc input res-add 0))
              (if (= op1-value op2-value)
                (assoc input res-add 1)
                (assoc input res-add 0)))]
    res))


(defn evaluate
  [input1 input2]
  (let [input "3,8,1001,8,10,8,105,1,0,0,21,46,63,76,97,118,199,280,361,442,99999,3,9,102,4,9,9,101,2,9,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,101,5,9,9,102,3,9,9,101,3,9,9,4,9,99,3,9,1001,9,2,9,102,3,9,9,4,9,99,3,9,1002,9,5,9,101,4,9,9,1002,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,5,9,101,3,9,9,1002,9,5,9,1001,9,5,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99"]
    (loop [input (mapv #(Integer/parseInt %) (cs/split input #","))
           curr-index 0
           p-input input1
           output nil]
      (let [op-code (get-opcode (get input curr-index))]
        (cond
          (= op-code 3)
          (do
            (let [op1 (get input (+ curr-index 1))]
              (recur (assoc input op1 p-input)
                     (+ curr-index 2)
                     input2
                     output)))

          (= op-code 4)
          (do
            (let [op1 (get input (+ curr-index 1))]
              (println "out is" (get input op1))
              (recur input
                     (+ curr-index 2)
                     p-input
                     (get input op1))))

          (= op-code 99)
          output

          (or (= op-code 5)
              (= op-code 6))
          (recur input
                 (jump input curr-index op-code)
                 p-input
                 output)

          (or (= op-code 7)
              (= op-code 8))
          (recur (comparision input curr-index op-code)
                 (+ curr-index 4)
                 p-input
                 output)

          :else
          (recur (add-or-mul input curr-index op-code)
                 (+ curr-index 4)
                 p-input
                 output))))))

(defn solve-for-input
  [input-vals]
  (reduce (fn [acc input]
            (println acc " is acc")
            (if (= acc nil)
              (evaluate input 0)
              (evaluate input acc)))
          nil
          input-vals))


(defn find-max-output
  []
  (apply max (map solve-for-input (cmc/permutations (range 5)))))
