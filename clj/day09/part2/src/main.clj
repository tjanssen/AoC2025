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

(defn between
  [a b c]
  (let [[x y] (sort [b c])]
    (and (>= a x) (<= a y))))
(t/ann between [t/Int t/Int t/Int :-> t/Bool])

(with-test
  (defn point-is-on-line
    [[x y] [[ax ay] [bx by]]]
    (or
     (and (= x ax) (between y ay by))
     (and (= y ay) (between x ax bx))))
  (is (true? (point-is-on-line [2 5] [[2 4] [2 9]])))
  (is (false? (point-is-on-line [5 2] [[2 4] [2 9]])))
  (is (true? (point-is-on-line [2 5] [[2 9] [2 4]])))
  (is (true? (point-is-on-line [5 2] [[4 2] [9 2]])))
  (is (false? (point-is-on-line [2 13] [[2 4] [2 9]]))))

(with-test
  (defn point-is-in-path
    [[x y] point-list]
    (odd? (loop [point-list    point-list
                 intersections 0]
            (if (< (count point-list) 2)
              intersections
              (let [[ax ay] (first point-list)
                    [bx by] (second point-list)]
                (if (point-is-on-line [x y] [[ax ay] [bx by]])
                  1
                  (if (= ay by)
                    ; line is horizontal (ignore)
                    (recur (rest point-list) intersections)
                    (let [[ay by] (sort [ay by])]
                      (if (and (< x ax) (between y ay (dec by)))
                        (recur (rest point-list) (inc intersections))
                        (recur (rest point-list) intersections))))))))))
  (is (true? (point-is-in-path [7 1] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3])))) 
  (is (true? (point-is-in-path [3 5] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3]))))
  (is (true? (point-is-in-path [2 5] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3]))))
  (is (true? (point-is-in-path [9 1] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3]))))
  (is (true? (point-is-in-path [11 4] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3]))))
  (is (true? (point-is-in-path [4 3] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3]))))
  (is (false? (point-is-in-path [0 0] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3]))))
  (is (false? (point-is-in-path [2 1] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3]))))
  (is (false? (point-is-in-path [2 7] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3])))))

(defn perimeter-of-rect
  [[ax ay] [bx by]]
  (let [xrange (range (min ax bx) (inc (max ax bx)))
        yrange (range (min ay by) (inc (max ay by)))]
    (->   (into #{} (map (fn [x] [x, ay]) xrange))
          (into (map (fn [x] [x, by]) xrange))
          (into (map (fn [y] [ax, y]) yrange))
          (into (map (fn [y] [bx, y]) yrange)))))

(with-test
  (defn rect-is-in-path
    [a b coord-loop]
    (let [perimeter-coords (sort-by first (perimeter-of-rect a b))] 
      (loop [perimeter-coords perimeter-coords]
        (if (seq perimeter-coords)
          (if (point-is-in-path (first perimeter-coords) coord-loop)
            (recur (rest perimeter-coords))
            false)
          true))))
  (is (true? (rect-is-in-path  [9 5] [2 3] '([7 3] [7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3])))))

(let [coord-list          (->> (read-file "input.txt")
                               (map read-coords))
      coord-loop          (conj coord-list (last coord-list))
      rect-list-with-area (->> coord-list
                               (build-rect-list)
                               (sort-by first)
                               (reverse))]
  (loop [rect-list rect-list-with-area]
    (if (seq rect-list)
      (if (rect-is-in-path  (second (first rect-list)) (nth (first rect-list) 2) coord-loop)
        (first (first rect-list))
        (recur (rest rect-list)))
      false)))






(comment


  (rect-is-in-path  [9 5] [2 3] '([7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3] [7 1]))




  (perimeter-of-rect [2 4] [3 3])
  (map (fn [x] [x, 3]) '(3 4 5))


  (point-is-in-path [2 1] '([7 1] [11 1] [11 7] [9 7] [9 5] [2 5] [2 3] [7 3]))

  ;; Test
  (do
    (run-tests)
    (t/cns))

  '())

