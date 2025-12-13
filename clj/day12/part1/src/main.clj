(ns main
  (:require [clojure.string :as str]
            [clojure.test :refer [#_#_with-test is run-tests]]))

(defn read-shape
  [shape]
  (subvec (str/split shape #"\n") 1))

(defn read-area
  [area]
  (let [[_ len wid amounts] (re-matches #"(\d+)x(\d+): (.+)" area)]
    {:length (parse-long len)
     :width (parse-long wid)
     :amounts (mapv parse-long (str/split amounts #" "))}))

(defn read-file
  [filename]
  (let [teh (str/split (slurp filename) #"\n\n")
        shapes (butlast teh)
        areas (last teh)]
    {:shapes (mapv read-shape shapes) :areas  (map read-area (str/split areas #"\n"))}))

(defn not-enough-space
  "true if there is not enough space in the area to store all occupied tiles"
  [shapes area]
  (>
   (->> (map #(count (filter (fn [x] (= x \#)) (apply str %))) shapes)
        (map (fn [a b] (* a b)) (:amounts area))
        (reduce + 0))
   (* (:length area) (:width area))))

(let [{:keys [shapes areas]} (read-file "input.txt")]
  (count (->> areas
              (remove (partial not-enough-space shapes)))))





(comment




  ;; Test
  (run-tests)

  '())

