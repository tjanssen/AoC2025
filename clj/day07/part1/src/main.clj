(ns main
  (:require [clojure.string :as str]
            [clojure.test :refer [#_with-test #_is run-tests]]
            [typed.clojure :as t]))

(t/ann clojure.core/slurp [t/Str :-> t/Str])

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split-lines)))
(t/ann read-file [t/Str :-> (t/AVec t/Str)])

(defn process-beam
  [row beam]
  (if (= \^ (get row beam))
    [1 [(dec beam) (inc beam)]]
    [0 [beam]]))

(defn process-row
  [row beams]
  (loop [beams beams
         next-beams #{}
         splits 0]
    (if (= 0 (count beams))
      [splits next-beams]
      (let [[split new-beams] (process-beam row (first beams))]
        (recur (rest beams)
               (into next-beams new-beams)
               (+ splits split))))))

(let [input (read-file "input.txt")]
  (loop [input-rows (rest input)
         splits 0
         beams #{(str/index-of (first input) \S)}]
    (if (= 0 (count input-rows))
      splits
      (let [[row-splits next-beams] (process-row (first input-rows) beams)]
        (recur (rest input-rows)
               (+ splits row-splits)
               next-beams)))))





(comment

  (process-row ".......^.^....." #{7 9})

  ;; Test
  (do
    (run-tests)
    (t/cns))

  '())

