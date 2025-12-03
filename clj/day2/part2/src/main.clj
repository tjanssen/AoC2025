(ns main
  (:require [clojure.string :as str]))

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split #"[\,\n]")))

(defn read-range
  [the-range]
  (let [[low high] (str/split the-range #"\-")]
    [(parse-long low) (parse-long high)]))

(defn is-invalid-id
  [id]
  (boolean (re-matches #"^([0-9]+)\1+$" (str id))))

(defn invalid-ids-in-range
  [[low high]]
  (filter is-invalid-id (range low (+ high 1))))

(->> (read-file "input.txt")
     (map read-range)
     (map invalid-ids-in-range)
     (apply concat)
     (reduce + 0))



(comment

  (is-invalid-id "123123")
  (is-invalid-id "123123123")

  (filter is-invalid-id (range 11 (+ 22 1)))

  '())


