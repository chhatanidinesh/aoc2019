(ns problem7.part2
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


(defn comparison
  [input curr-index op-code]
  (let [res-add (get input (+ curr-index 3))
        [op1-value op2-value] (get-operands input curr-index)
        res (if (= op-code 7)
              (if (< op1-value op2-value)
                (assoc input res-add 1)
                (assoc input res-add 0))
              (if (= op1-value op2-value)
                (assoc input res-add 1)
                (assoc input res-add 0)))]
    res))

(defn evaluate
  [program inputs curr-index]
  (let [output (atom nil)]
    (loop [i-program program
           curr-index curr-index
           p-input inputs
           output output]

      (let [op-code (get-opcode (get i-program curr-index))]
        (cond
          (= op-code 3)
          (do
            (let [op1 (get i-program (+ curr-index 1))]
              (recur (assoc i-program op1 (first p-input))
                     (+ curr-index 2)
                     (rest p-input)
                     output)))

          (= op-code 4)
          (do
            (let [op1 (get i-program (+ curr-index 1))]
              ;;(println "out is" (get input op1))
              (reset! output (get i-program op1))
              [@output (+ curr-index 2) i-program (vec p-input)]))


          (= op-code 99)
          [:halted (+ curr-index 1) i-program (vec p-input)]

          (or (= op-code 5)
              (= op-code 6))
          (recur i-program
                 (jump i-program curr-index op-code)
                 p-input
                 output)

          (or (= op-code 7)
              (= op-code 8))
          (recur (comparison i-program curr-index op-code)
                 (+ curr-index 4)
                 p-input
                 output)

          (or (= op-code 1)
              (= op-code 2))
          (recur (add-or-mul i-program curr-index op-code)
                 (+ curr-index 4)
                 p-input
                 output)

          :else (do (println "invalid instruction") (throw (Exception. "bad opcode")))
          )))))


(defn solve-for-input
  [input-vals]
  (let [prog "3,8,1001,8,10,8,105,1,0,0,21,46,63,76,97,118,199,280,361,442,99999,3,9,102,4,9,9,101,2,9,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,101,5,9,9,102,3,9,9,101,3,9,9,4,9,99,3,9,1001,9,2,9,102,3,9,9,4,9,99,3,9,1002,9,5,9,101,4,9,9,1002,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,5,9,101,3,9,9,1002,9,5,9,1001,9,5,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99"
        input-prog (mapv #(Integer/parseInt %) (cs/split prog #","))]
    (loop [input-prog  (vec (repeat 5 input-prog))
           amp-index 1
           indexes (vec (repeat 5 0))
           prev-out 0
           inputs (cycle input-vals)
           p-inputs (vec (repeat 5 []))
           e-output nil
           c 0]
      (let [[o index input-prog-e p-inputs-e] (evaluate
                                               (get input-prog (dec amp-index))
                                               (concat (get p-inputs (dec amp-index))
                                                       (if (< c 5)
                                                         [(first inputs)
                                                          prev-out]
                                                         [prev-out]
                                                         ) )
                                               (get indexes (dec amp-index)))]

        (if (and (= o :halted) (= amp-index 5) )
          e-output
          (if (= (mod amp-index 5) 0)
            (do
              (recur (assoc input-prog (dec amp-index) input-prog-e)
                     1
                     (assoc indexes (dec amp-index) index)
                     o
                     (rest inputs)
                     (assoc p-inputs (dec amp-index) p-inputs-e)
                     o
                     (inc c)))
            (recur (assoc input-prog (dec amp-index) input-prog-e)
                   (inc amp-index)
                   (assoc indexes (dec amp-index) index)
                   o
                   (rest inputs)
                   (assoc p-inputs (dec amp-index) p-inputs-e)
                   e-output
                   (inc c))))))))

(defn find-max-output
  []
  (apply max (map solve-for-input (cmc/permutations (range 5 10)))))
