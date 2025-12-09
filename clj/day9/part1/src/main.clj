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

(defn read-coords
  [coord-string]
  (->> (str/split coord-string #",")
       (mapv parse-long)))

(defn rect-area
  [[x1 y1] [x2 y2]]
  (* (inc (abs (- x1 x2)))
     (inc (abs (- y1 y2)))))

(defn build-rect-list
  [coords]
  (let [num-coords (count coords)]
    (loop [i 0
           coords-a coords
           rect-list []]
      (if (= i num-coords)
        rect-list
        (recur (inc i)
               (rest coords-a)
               (into rect-list
                     (loop [j (+ i 1)
                            coords-b (rest coords-a)
                            rect-list []]
                       (if (= j num-coords)
                         rect-list
                         (recur (inc j)
                                (rest coords-b)
                                (conj rect-list
                                      [(rect-area (first coords-a) (first coords-b)) i j]))))))))))


(->> (read-file "input.txt")
     (map read-coords) 
     (build-rect-list)
     (sort-by first)
     (reverse)
     (first)
     (first))






(comment


  ;; Test
  (do
    (run-tests)
    (t/cns))

  '())

