(ns main
  (:require [clojure.string :as str]
            [clojure.test :refer [#_with-test #_is run-tests]]
            [typed.clojure :as t]))

(t/ann clojure.core/slurp [t/Str :-> t/Str])

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split #"\n")))
(t/ann read-file [t/Str :-> (t/AVec t/Str)])

(defn read-range
  [the-range]
  (let [[low high] (str/split the-range #"\-")]
    [(parse-long low) (parse-long high)]))

(defn maybe-merge
  [r1 r2]
  (if (>= (inc (last r1)) (first r2))
    (if (<= (last r2) (last r1))
      [[(first r1) (last r1)]]
      [[(first r1) (last r2)]])
    [r1 r2]))

(defn merge-ranges
  [ranges]
  (if (= 0 (count ranges))
    []
    (loop [results (list (first ranges))
           ranges (rest ranges)]
      (if (= 0 (count ranges))
        results
        (recur (into (rest results) (maybe-merge (first results) (first ranges)))
               (rest ranges))))))

(defn cardinality
  [[a b]]
  (inc (- b a)))

(->> (read-file "input.txt")
     (partition-by #(= % ""))
     (first)
     (map read-range)
     (sort-by first)
     (merge-ranges)
     (map cardinality)
     (reduce + 0))





(comment

  ;; Test
  (do
    (run-tests)
    (t/cns))

  '())

