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

(let [roll-map     (read-file "input.txt")
      height       (count roll-map)
      width        (count (first roll-map))
      can-go-up    (fn [[row _col]] (> row 0))
      can-go-down  (fn [[row _col]] (< row (dec height)))
      can-go-left  (fn [[_row col]] (> col 0))
      can-go-right (fn [[_row col]] (< col (dec width)))]
  (loop [current-row 0
         total-acc 0]
    (let [row-total
          (loop [current-col 0
                 row-acc 0]
            (let [current-value
                  (if (= \@ (get-in roll-map [current-row current-col]))
                    (if (<
                         (cond-> 0
                           (and (can-go-up [current-row current-col])
                                (can-go-left [current-row current-col])
                                (= \@ (get-in roll-map [(dec current-row) (dec current-col)]))) (inc)
                           (and (can-go-up [current-row current-col])
                                (= \@ (get-in roll-map [(dec current-row) current-col]))) (inc)
                           (and (can-go-up [current-row current-col])
                                (can-go-right [current-row current-col])
                                (= \@ (get-in roll-map [(dec current-row) (inc current-col)]))) (inc)
                           (and (can-go-left [current-row current-col])
                                (= \@ (get-in roll-map [current-row (dec current-col)]))) (inc)
                           (and (can-go-right [current-row current-col])
                                (= \@ (get-in roll-map [current-row (inc current-col)]))) (inc)
                           (and (can-go-down [current-row current-col])
                                (can-go-left [current-row current-col])
                                (= \@ (get-in roll-map [(inc current-row) (dec current-col)]))) (inc)
                           (and (can-go-down [current-row current-col])
                                (= \@ (get-in roll-map [(inc current-row) current-col]))) (inc)
                           (and (can-go-down [current-row current-col])
                                (can-go-right [current-row current-col])
                                (= \@ (get-in roll-map [(inc current-row) (inc current-col)]))) (inc))
                         4)
                      1
                      0)
                    0)]
              (if (< current-col (dec width))
                (recur (inc current-col) (+ row-acc current-value))
                (+ row-acc current-value))))]
      (if (< current-row (dec height))
        (recur (inc current-row)
               (+ row-total total-acc))
        (+ row-total total-acc)))))





(comment

  ;; Test
  (do
    (run-tests)
    (t/cns)) 
  



  '())

