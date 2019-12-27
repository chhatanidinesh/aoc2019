(ns problem8.part1_2
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

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

(defn write-res
  [i-program address value]
  (assoc i-program address value))

(defn get-opcode
  [n]
  (mod n 100))


(defn get-operand-value
  [i-program mode value rel-base]
  (cond
    (= mode 1) value
    (= mode 2) (get i-program (+ rel-base value) 0)
    :else  (get i-program value 0)))

(defn get-next-nth-operand
  [i-program curr-index n rel-base]
  (let [curr-value (get i-program curr-index)
        op1 (get i-program (+ curr-index n))
        op1-mode (get-mode-of-operand (quot curr-value 100) n)
        op1-value (get-operand-value i-program op1-mode op1 rel-base)]
    op1-value))

(defn get-res-address
  [i-program curr-index n rel-base]
  (let [curr-value (get i-program curr-index)
        res-value (get i-program (+ curr-index n))
        res-mode (get-mode-of-operand (quot curr-value 100) n)]
    (if (= res-mode 0)
      res-value
      (+ rel-base res-value))))


(defn get-operands
  [i-program curr-index rel-base]
  (let [[op1-value op2-value] [(get-next-nth-operand i-program curr-index 1 rel-base)
                               (get-next-nth-operand i-program curr-index 2 rel-base)]]
       [op1-value op2-value]))


(defn add-or-mul
  [i-program curr-index op-code rel-base]
  (let [res-add (get-res-address i-program curr-index 3 rel-base)
        [op1-value op2-value] (get-operands i-program curr-index rel-base)
        res (if (= op-code 2)
              (* op1-value op2-value)
              (+ op1-value op2-value))]
    (write-res i-program res-add res )))


(defn jump
  [i-program curr-index op-code rel-base]
  (let [[op1-value op2-value] (get-operands i-program curr-index rel-base)]
    (if (= op-code 5)
      (if (zero? op1-value)
        (+ curr-index 3)
        op2-value)
      (if (zero? op1-value)
        op2-value
        (+ curr-index 3)))))


(defn comparison
  [i-program curr-index op-code rel-base]
  (let [res-add (get-res-address i-program curr-index 3 rel-base)
        [op1-value op2-value] (get-operands i-program curr-index rel-base)
        res (if (= op-code 7)
              (if (< op1-value op2-value)
                (write-res i-program res-add 1)
                (write-res i-program res-add 0))
              (if (= op1-value op2-value)
                (write-res i-program res-add 1)
                (write-res i-program res-add 0)))]
    res))


(defn evaluate
  [program inputs curr-index rel-base]
  (loop [i-program program
         curr-index curr-index
         p-input inputs
         output []
         rel-base rel-base]
    (let [op-code (get-opcode (get i-program curr-index))]
      (cond
        (= op-code 3)
        (let [res-add (get-res-address i-program curr-index 1 rel-base)]
          (when (= nil (first p-input))
            (read-line))
          (recur (assoc i-program res-add (first p-input))
                 (+ curr-index 2)
                 (rest p-input)
                 output
                 rel-base))

        (= op-code 4)
        (let [out (get-next-nth-operand i-program curr-index 1 rel-base)]
          (recur i-program
                 (+ curr-index 2)
                 p-input
                 (conj output out)
                 rel-base))


        (= op-code 99)
        [:halted output (+ curr-index 1) i-program (vec p-input)]

        (or (= op-code 5)
            (= op-code 6))
        (recur i-program
               (jump i-program curr-index op-code rel-base)
               p-input
               output
               rel-base)

        (or (= op-code 7)
            (= op-code 8))
        (recur (comparison i-program curr-index op-code rel-base)
               (+ curr-index 4)
               p-input
               output
               rel-base)

        (= op-code 9)
        (let [op1-value (get-next-nth-operand i-program curr-index 1 rel-base)]
          (recur i-program
                 (+ curr-index 2)
                 p-input
                 output
                 (+ rel-base op1-value)))

        (or (= op-code 1)
            (= op-code 2))
        (recur (add-or-mul i-program curr-index op-code rel-base)
               (+ curr-index 4)
               p-input
               output
               rel-base)

        :else (do (println "invalid instruction") (throw (Exception. "bad opcode")))))))


(defn solve-part-1
  []
  (evaluate (zipmap (range) (map #(Integer/parseInt %1) (cs/split (cs/trim (slurp (io/resource "prob-9-input"))) #",")))
            [1]
            0
            0))

(defn solve-part-2
  []
  (evaluate (zipmap (range) (map #(Integer/parseInt %1) (cs/split (cs/trim (slurp (io/resource "prob-9-input"))) #",")))
            [2]
            0
            0))
