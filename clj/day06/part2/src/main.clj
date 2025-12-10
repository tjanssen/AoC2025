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

(defn all-spaces?
  [vec]
  (every? #(= \space %) vec))

(defn build-number
  [num-vec]
  (->> (butlast num-vec)
       (apply str)
       (str/trim)
       parse-long))

(defn compute-math
  [columns]
  (let [op (if
            (= \* (last (first columns)))
             *
             +)]
    (->> (map build-number columns)
         (apply op))))

(->> (read-file "input.txt")
     (apply mapv vector)
     (partition-by all-spaces?)
     (filter #(not (all-spaces? (first %))))
     (map compute-math)
     (reduce + 0))





(comment

  (compute-math '([\1 \space \space \*] [\2 \4 \space \space] [\3 \5 \6 \space]))

  (parse-long (str/trim (apply str (butlast [\3 \5 \6 \space]))))


  ;; Test
  (do
    (run-tests)
    (t/cns))

  '())

