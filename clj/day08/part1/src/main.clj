(ns main
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.test :refer [#_with-test #_is run-tests]]
            [typed.clojure :as t]))

(t/ann clojure.core/slurp [t/Str :-> t/Str])

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split-lines)))
(t/ann read-file [t/Str :-> (t/Vec t/Str)])

(defn read-coords
  [coord-string]
  (->> (str/split coord-string #",")
       (mapv parse-double)))

(defn distance
  [[x1 y1 z1] [x2 y2 z2]]
  (math/sqrt (+ (math/pow (- x1 x2) 2)
                (math/pow (- y1 y2) 2)
                (math/pow (- z1 z2) 2))))

(defn build-dist-list
  [coords]
  (let [num-coords (count coords)]
    (loop [i 0
           coords-a coords
           dist-list []]
      (if (= i num-coords)
        dist-list
        (recur (inc i)
               (rest coords-a)
               (into dist-list
                     (loop [j (+ i 1)
                            coords-b (rest coords-a)
                            dist-list []]
                       (if (= j num-coords)
                         dist-list
                         (recur (inc j)
                                (rest coords-b)
                                (conj dist-list
                                      [(distance (first coords-a) (first coords-b)) i j]))))))))))

(defn merge-circuits
  [circuits a b]
  (let [circuits-to-merge (filter (fn [c] (or (some #(= a %) c)
                                              (some #(= b %) c))) circuits)
        other-circuits (remove (fn [c] (or (some #(= a %) c)
                                           (some #(= b %) c))) circuits)]
    (if (= 1 (count circuits-to-merge))
      circuits
      (conj other-circuits (apply set/union circuits-to-merge)))))


(let [num-connections    1000
      coords             (->> (read-file "input.txt")
                              (map read-coords))
      num-coords         (count coords)
      dist-list          (->> coords
                              (build-dist-list)
                              (sort-by first))
      connections        (->> (take num-connections dist-list)
                              (map (fn [x] [(second x) (second (rest x))])))
      circuits           (map #(identity #{%}) (range num-coords))
      merged-circuits    (loop [connections connections
                                circuits    circuits]

                           (if (= 0 (count connections))
                             circuits
                             (recur (rest connections)
                                    (apply merge-circuits circuits (first connections)))))
      the-answer         (->> merged-circuits
                              (map count)
                              (sort)
                              (reverse)
                              (take 3)
                              (reduce * 1))]
  the-answer)









(comment

  (merge-circuits (mapv #(identity #{%}) (range 20)) 6 7)

  (filter (fn [c] (or (some #(= 6 %) c)
                      (some #(= 7 %) c))) (map #(identity #{%}) (range 20)))

  (merge-circuits  (map #(identity #{%}) (range 20)) 3 6)


  ;; Test
  (do
    (run-tests)
    (t/cns))

  '())

