(ns main
  (:require [clojure.string :as str]
            [clojure.test :refer [#_with-test #_is run-tests]]
            [typed.clojure :as t]))

(t/ann clojure.core/slurp [t/Str :-> t/Str])

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split-lines)))
(t/ann read-file [t/Str :-> (t/AVec t/Str)])

(defn split-line
  [line]
  (->>
   (str/split line #" ")
   (filter #(not= % ""))))

(defn compute-math
  [vec]
  (apply (if (= "*" (last vec))
           *
           +)
         (map parse-long (butlast vec))))

(->> (read-file "input.txt")
     (map split-line)
     (apply mapv vector)
     (map compute-math)
     (apply +))





(comment

  (compute-math ["2" "4" "+"])


  ;; Test
  (do
    (run-tests)
    (t/cns))

  '())

