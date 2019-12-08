(ns problem4.part1
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.set :as cset]))


(defn passphrase?
  [n]
  (loop [n n
         repeated-pair? false
         prev -1
         divider 100000]
    (cond
      (and (= divider 0) repeated-pair?) true
      (= divider 0) false
      :else
      (let [digit (quot n divider)
            n (mod n divider)]
        (if (< digit prev)
          false
          (recur n
                 (if (false? repeated-pair?)
                   (= prev digit)
                   repeated-pair?)
                 digit
                 (quot divider 10)))))))


(defn all-passphrases
  []
  (count (filter passphrase? (range 265275 781584))))
