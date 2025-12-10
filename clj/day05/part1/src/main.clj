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

(defn read-range
  [the-range]
  (let [[low high] (str/split the-range #"\-")]
    [(parse-long low) (parse-long high)]))

(defn in-range
  [range id]
  (and (>= id (first range))
       (<= id (last range))))

(defn in-any-range
  [ranges id]
  (loop [ranges ranges]
    (if (= 0 (count ranges))
      false
      (if (in-range (first ranges) id)
        true
        (recur (rest ranges))))))

(let [[ranges-str _ ids-str]
      (->> (read-file "input.txt")
           (partition-by  #(= % "")))
      valid-ranges (map read-range ranges-str)
      ids (map parse-long ids-str)]
  (loop [ids ids
         valid-ids 0]
    (if (= 0 (count ids))
      valid-ids
      (recur (rest ids) 
             (if (in-any-range valid-ranges (first ids))
               (inc valid-ids)
               valid-ids)))))


(comment

  ;; Test
  (do
    (run-tests)
    (t/cns))


  '())

