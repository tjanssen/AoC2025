(ns main
  (:require [clojure.string :as str]
            [clojure.test :refer [with-test is run-tests]]
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
(t/ann read-coords ['t/Str -> '[t/Int t/Int]])

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
                                      [(rect-area (first coords-a) (first coords-b)) (nth coords i) (nth coords j)]))))))))))

(with-test
  (defn points-in-line
    [[ax ay] [bx by]]
    (if (= ax bx)
      (let [min-y (min ay by)
            max-y (max ay by)]
        (for [yi (range min-y (inc max-y))]
          [ax yi]))
      (let [min-x (min ax bx)
            max-x (max ax bx)]
        (for [xi (range min-x (inc max-x))]
          [xi ay]))))
  (is (= #{[2 1] [2 2] [2 3] [2 4] [2 5]} (into #{} (points-in-line [2 1] [2 5]))))
  (is (= #{[1 2] [2 2] [3 2] [4 2] [5 2]} (into #{} (points-in-line [1 2] [5 2]))))
  (is (= #{[2 1] [2 2] [2 3] [2 4] [2 5]} (into #{} (points-in-line [2 5] [2 1]))))
  (is (= #{[1 2] [2 2] [3 2] [4 2] [5 2]} (into #{} (points-in-line [5 2] [1 2])))))

(defn points-on-path
  [point-list]
  (loop [point-list point-list
         points #{}]
    (if (< (count point-list) 2)
      points
      (let [point-a (first point-list)
            point-b (second point-list)]
        (recur (rest point-list) (into points (points-in-line point-a point-b)))))))

(with-test
  (defn point-is-in-rect
    [[min-x max-x min-y max-y] [x y]]
    (and (> x min-x)
         (< x max-x)
         (> y min-y)
         (< y max-y)))
  (is (true? (point-is-in-rect [2 8 1 5] [4 2])))
  (is (true? (point-is-in-rect [2 8 1 5] [3 2])))
  (is (false? (point-is-in-rect [2 8 1 5] [2 1])))
  (is (false? (point-is-in-rect [2 8 1 5] [9 4])))
  (is (false? (point-is-in-rect [2 8 1 1] [4 1]))))


(let [coord-list          (->> (read-file "input.txt")
                               (map read-coords))
      coord-loop          (conj coord-list (last coord-list))
      all-points-on-path  (points-on-path coord-loop)
      rect-list-with-area (->> coord-list
                               (build-rect-list)
                               (sort-by first)
                               (reverse))]
  (loop [rect-list rect-list-with-area]
    (if (seq rect-list)
      (let [[area [ax ay] [bx by]] (first rect-list)]
        (if (some #(point-is-in-rect [ (min ax bx) (max ax bx) (min ay by) (max ay by) ]%  ) all-points-on-path) 
          (recur (rest rect-list))
          area))
      false)))


(comment
  
  ;; Test
  (do
    (run-tests)
    (t/cns))

  '()
  )

