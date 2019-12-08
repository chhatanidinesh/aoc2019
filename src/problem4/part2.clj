(ns problem4.part2
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.set :as cset]))


(defn passphrase?
  [n]
  (loop [n n
         repeated-info {}
         prev -1
         divider 100000]
    (cond
      (and (= divider 0) ((set (vals repeated-info)) 1) ) true
      (= divider 0) false
      :else
      (let [digit (quot n divider)
            n (mod n divider)]
        (if (< digit prev)
          false
          (recur n
                 (if (= prev digit)
                   (update repeated-info prev (fnil inc 0))
                   repeated-info)
                 digit
                 (quot divider 10)))))))


(defn all-passphrases
  []
  (count (filter passphrase? (range 265275 781584))))
