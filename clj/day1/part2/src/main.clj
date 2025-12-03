(ns main
  (:require [clojure.string :as str]
            [clojure.test :refer [with-test is run-tests]]))

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

(with-test
  (defn pass-over-or-land-on-zero
    [loc mov]
    (let [rest-mov (if 
                    (< mov 0)
                     (+ mov (* 100 (quot (abs mov) 100)))
                     (- mov (* 100 (quot mov 100))))]
      (+
       (quot (abs mov) 100)
       (if (and
            (not= 0 loc)
            (or
             (> (+ loc rest-mov) 99)
             (< (+ loc rest-mov) 1)))
         1
         0))))
  (is (= 1 (pass-over-or-land-on-zero 23 -43)))
  (is (= 2 (pass-over-or-land-on-zero 23 -143)))
  (is (= 0 (pass-over-or-land-on-zero 23 43)))
  (is (= 1 (pass-over-or-land-on-zero 23 143)))
  (is (= 1 (pass-over-or-land-on-zero 23 -23)))
  (is (= 0 (pass-over-or-land-on-zero 0 1)))
  (is (= 1 (pass-over-or-land-on-zero 0 100)))
  (is (= 0 (pass-over-or-land-on-zero 0 -99)))
  (is (= 1 (pass-over-or-land-on-zero 0 -100)))
  (is (= 1 (pass-over-or-land-on-zero 0 -101))))

(defn moves-to-lands
  [move-list]
  (loop [input move-list
         locations '(50)
         lands '()]
    (if (> (count input) 0)
      (recur (rest input)
             (conj locations (mod (+ (first locations) (first input)) 100))
             (conj lands (pass-over-or-land-on-zero (first locations) (first input))))
      lands)))

(defn count-lands
  [lands]
  (reduce + 0 lands))

(->> "input.txt"
     (read-file)
     (map read-number)
     (moves-to-lands)
     (count-lands))



(comment

  (run-tests 'main)


  (let [mov 0]
    (if (< mov 0)
      (+ mov (* 100 (quot (abs mov) 100)))
      (- mov (* 100 (quot mov 100)))))

  (* 100 (quot (abs -475647) 100))


  (mod -123 100)

  (let [loc 23
        mov -22]
    (or (> (+ loc mov) 99) (< (+ loc mov) 1)))

  '())


