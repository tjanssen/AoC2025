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

(defn can-remove
  [roll-map current-row current-col]
  (let [height       (count roll-map)
        width        (count (first roll-map))
        can-go-up    (> current-row 0)
        can-go-down  (< current-row (dec height))
        can-go-left  (> current-col 0)
        can-go-right (< current-col (dec width))
        current-cell (get-in roll-map [current-row current-col])]
    (if (= \@ current-cell)
      (if (<
           (cond-> 0
             (and can-go-up
                  can-go-left
                  (= \@ (get-in roll-map [(dec current-row) (dec current-col)]))) (inc)
             (and can-go-up
                  (= \@ (get-in roll-map [(dec current-row) current-col]))) (inc)
             (and can-go-up
                  can-go-right
                  (= \@ (get-in roll-map [(dec current-row) (inc current-col)]))) (inc)
             (and can-go-left
                  (= \@ (get-in roll-map [current-row (dec current-col)]))) (inc)
             (and can-go-right
                  (= \@ (get-in roll-map [current-row (inc current-col)]))) (inc)
             (and can-go-down
                  can-go-left
                  (= \@ (get-in roll-map [(inc current-row) (dec current-col)]))) (inc)
             (and can-go-down
                  (= \@ (get-in roll-map [(inc current-row) current-col]))) (inc)
             (and can-go-down
                  can-go-right
                  (= \@ (get-in roll-map [(inc current-row) (inc current-col)]))) (inc))
           4)
        \x
        \@)
      current-cell)))

(defn transform-row
  [roll-map current-row]
  (let [width (count (first roll-map))]
    (loop [current-col 0
           row-acc ""]
      (if (= (count row-acc) width)
        row-acc
        (recur (inc current-col) (str row-acc (can-remove roll-map current-row current-col)))))))

(defn transform-roll-map
  [roll-map]
  (let [height (count roll-map)]
    (loop [current-row 0
           roll-map-acc []]
      (if (= (count roll-map-acc) height)
        roll-map-acc
        (recur (inc current-row)
               (conj roll-map-acc (transform-row roll-map current-row)))))))

(defn fully-transform-roll-map
  [roll-map]
  (loop [initial-roll-map roll-map]
    (let [transformed-roll-map (transform-roll-map initial-roll-map)]
      (if (= initial-roll-map transformed-roll-map)
        transformed-roll-map
        (recur transformed-roll-map)))))

(->> (read-file "input.txt")
     (fully-transform-roll-map)
     (map seq)
     (apply concat)
     (filter #(= \x %))
     (count))


(comment

  ;; Test
  (do
    (run-tests)
    (t/cns))


  '())

