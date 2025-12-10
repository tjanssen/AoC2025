(ns main
  (:require [clojure.string :as str]))

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split #"\n")))

(defn read-number
  [the-number]
  (let [dir (first the-number)
        num (parse-long (subs the-number 1))]
    (if (= \L dir)
      (- num)
      num)))

(defn moves-to-locations
  [move-list]
  (loop [input move-list
         locations '(50)]
    (if (> (count input) 0)
      (recur (rest input)
             (conj locations (mod (+ (first locations) (first input)) 100)))
      locations)))

(defn  count-zeros
  [locations]
  (count (filter #(= 0 %) locations)))


(->> "input.txt"
     (read-file)
     (map read-number)
     (moves-to-locations)
     (count-zeros))

(comment


  (first "L23")
  (read-number   "R23")

  (conj '(1 2 3) 4)
  '())


