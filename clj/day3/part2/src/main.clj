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

(defn remaining-batteries
  [batteries split-char]
  (apply str (rest
              (second
               (split-at (str/index-of batteries split-char)
                         batteries)))))

(with-test
  (defn most-joltage
    [batteries length]
    (parse-long
     (loop [acc ""
            length length
            batteries batteries]
       (cond
         (= 0 length) acc
         (< (count batteries) length) acc
         (= (count batteries) length) (str acc batteries)
         :else
         (let [current-digit (max-in-char-seq (drop-last (- length 1) batteries))]
           (recur
            (str acc current-digit)
            (- length 1)
            (remaining-batteries batteries (char (+ current-digit (int \0))))))))))
  (is (= 98 (most-joltage "987654321111111" 2)))
  (is (= 89 (most-joltage "811111111111119" 2)))
  (is (= 78 (most-joltage "234234234234278" 2)))
  (is (= 92 (most-joltage "818181911112111" 2)))
  (is (= 987654321111 (most-joltage "987654321111111" 12)))
  (is (= 811111111119 (most-joltage "811111111111119" 12)))
  (is (= 434234234278 (most-joltage "234234234234278" 12)))
  (is (= 888911112111 (most-joltage "818181911112111" 12))))


(->> (read-file "input.txt")
     (map #(most-joltage % 12))
     (reduce + 0))



(comment

  (run-tests 'main)

  (max-in-char-seq (drop-last 12 "234234234234278"))


  (remaining-batteries (seq "801111151111119") \0)

  (split-at 7 "811111151111119")
  (str/index-of "801111151111119" (char (+ 0 (int \0))))

  '())

