(ns main
  (:require [clojure.string :as str]
            [clojure.test :refer [#_with-test #_is run-tests]]
            [typed.clojure :as t]))

(t/ann clojure.core/slurp [t/Str :-> t/Str])

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split-lines)))
(t/ann read-file [t/Str :-> (t/Vec t/Str)])

(defn compute-row-paths
  [input-row prev-row-paths]
  (loop [current-col   0
         row-paths-acc []] 
    (if (= (count input-row) (count row-paths-acc))
      row-paths-acc
      (recur (inc current-col)
             (conj row-paths-acc (if (= (get input-row current-col) \^)
                                   (+ (get prev-row-paths (inc current-col))
                                      (get prev-row-paths (dec current-col)))
                                   (get prev-row-paths current-col)))))))

(let [input (read-file "input.txt")
      init-row-paths  (repeat (count (first input)) 1)] 
  (get
   (loop [rows (butlast input)
          row-paths init-row-paths] 
     (if (= 0 (count rows))
       row-paths
       (recur (butlast rows)
              (compute-row-paths (last rows) (into [] row-paths)))))
   (str/index-of (first input) \S)))







(comment

  (compute-row-paths "......^.^......" [1 1 1 1 1, 1 1 1 1 1, 1 1 1 1 1])
  (compute-row-paths "......^.^......" '(1 1 1 1 1, 1 1 1 1 1, 1 1 1 1 1))

  ;; Test
  (do
    (run-tests)
    (t/cns))

  '())

