(ns problem5.part2
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
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
  []
  (let [input "3,225,1,225,6,6,1100,1,238,225,104,0,1002,114,46,224,1001,224,-736,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,1,166,195,224,1001,224,-137,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1001,169,83,224,1001,224,-90,224,4,224,102,8,223,223,1001,224,2,224,1,224,223,223,101,44,117,224,101,-131,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,1101,80,17,225,1101,56,51,225,1101,78,89,225,1102,48,16,225,1101,87,78,225,1102,34,33,224,101,-1122,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1101,66,53,224,101,-119,224,224,4,224,102,8,223,223,1001,224,5,224,1,223,224,223,1102,51,49,225,1101,7,15,225,2,110,106,224,1001,224,-4539,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1102,88,78,225,102,78,101,224,101,-6240,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,226,677,224,102,2,223,223,1006,224,329,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,359,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,374,101,1,223,223,1008,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,404,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,419,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,434,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,449,1001,223,1,223,1107,677,677,224,102,2,223,223,1005,224,464,1001,223,1,223,108,226,226,224,1002,223,2,223,1006,224,479,1001,223,1,223,1008,226,226,224,102,2,223,223,1005,224,494,101,1,223,223,108,677,226,224,102,2,223,223,1005,224,509,1001,223,1,223,8,677,226,224,1002,223,2,223,1006,224,524,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,539,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,569,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,584,101,1,223,223,1108,677,677,224,102,2,223,223,1006,224,599,1001,223,1,223,1008,677,226,224,1002,223,2,223,1005,224,614,1001,223,1,223,8,677,677,224,1002,223,2,223,1006,224,629,1001,223,1,223,107,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,659,101,1,223,223,107,226,226,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226"]
    (loop [input (mapv #(Integer/parseInt %) (cs/split input #","))
           curr-index 0]
      (let [op-code (get-opcode (get input curr-index))]
        (cond
          (= op-code 3)
          (do
            (let [op1 (get input (+ curr-index 1))]
              (recur (assoc input op1 (read-string (read-line)))
                     (+ curr-index 2))))

          (= op-code 4)
          (do
            (let [op1 (get input (+ curr-index 1))]
              (println "out is" (get input op1))
              (recur input
                     (+ curr-index 2))))

          (= op-code 99)
          :done

          (or (= op-code 5)
              (= op-code 6))
          (recur input (jump input curr-index op-code))

          (or (= op-code 7)
              (= op-code 8))
          (recur (comparision input curr-index op-code)
                 (+ curr-index 4))

          :else
          (recur (add-or-mul input curr-index op-code)
                 (+ curr-index 4)))))))