(ns main
  (:require [clojure.string :as str]
            [clojure.test :refer [with-test is run-tests]]))

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split #"\n")))

(defn max-in-char-seq
  [char-seq]
  (apply max (map (comp parse-long str) char-seq)))

(with-test
  (defn most-joltage
    [batteries]
    (let [first-digit (max-in-char-seq (drop-last batteries))
          last-digit
          (apply max
                 (map (comp parse-long str)
                      (rest 
                       (second 
                        (split-at (str/index-of batteries (char (+ first-digit (int \0))))
                                  (seq batteries))))))]
      (+ (* 10 first-digit) last-digit)))
  (is (= 98 (most-joltage "987654321111111")))
  (is (= 89 (most-joltage "811111111111119")))
  (is (= 78 (most-joltage "234234234234278")))
  (is (= 92 (most-joltage "818181911112111"))))


(->> (read-file "input.txt")
     (map most-joltage)
     (reduce + 0))



(comment

  (run-tests 'main)

  (read-file "test_input.txt")

  (apply max (map (comp parse-long str) (drop-last "811111111111119")))

  (split-at 3 (seq "811111111111119"))



  (apply max
         (map (comp parse-long str)
              (second (split-at (str/index-of "811111111111119"  (char (+ 8 (int \0)))) (seq "811111111111119")))))





  '())


