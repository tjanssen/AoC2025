(ns main
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combi]
            [clojure.test :refer [#_#_with-test is run-tests]]
            [typed.clojure :as t]))

(t/ann clojure.core/slurp [t/Str :-> t/Str])

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split-lines)))
(t/ann read-file [t/Str :-> (t/Vec t/Str)])

(def line-regex #"^\[([\.#]+)\] ([\d,\(\) ]+) \{([\d,]+)\}$")

(defn read-lights
  [lights]
  (loop [lights (str/reverse lights)
         acc 0]
    (if (seq lights)
      (if (= (first lights) \#)
        (recur (rest lights) (inc (bit-shift-left acc 1)))
        (recur (rest lights) (bit-shift-left acc 1)))
      acc)))

(defn read-switch-set
  [switch-set]
  (->> (str/split switch-set #",")
       (map parse-long)
       (map #(bit-shift-left 1 %))
       (reduce + 0)))

(defn read-machine
  [machine-string]
  (let [[_ lights switches _joltage] (re-matches line-regex machine-string)]

    {:lights (read-lights lights)
     :switches (->> (str/split switches #"[ \(\)]")
                    (remove #(= "" %))
                    (map read-switch-set))}))
(t/ann read-machine ['t/Str -> '[t/Int t/Int]])

(defn min-num-switches
  [{:keys [lights switches]}]
  (loop [i 1]
    (let [combinations (combi/combinations switches i)]
      (if (loop [combinations combinations]
            (if (seq combinations)
              (if (= lights (reduce bit-xor 0 (first combinations)))
                i
                (recur (rest combinations)))
              nil))
        i
        (recur (inc i))))))


(->> (read-file "input.txt")
     (map read-machine)
     (map min-num-switches)
     (reduce + 0))





(comment





  ;; Test
  (do
    (run-tests)
    (t/cns))

  '())

