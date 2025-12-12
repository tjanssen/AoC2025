(ns main
  (:require [clojure.string :as str]
            [clojure.test :refer [#_#_with-test is run-tests]]))

(defn read-file
  [filename]
  (-> (slurp filename)
      (str/split-lines)))

(defn read-links
  [link-string]
  (let [[node target-string] (str/split link-string #": ")]
    [(keyword node) (map keyword (str/split target-string #" "))]))

(defn calc-paths
  [link-map node memo]
  (if (= :out node)
    [1 memo]
    (if (contains? memo node)
      [(node memo) memo]
      (loop [num-paths 0
             out-links    (link-map node)
             memo         memo]
        (if (empty? out-links)
          [num-paths (assoc memo node num-paths)]
          (let [[this-num-paths this-memo] (calc-paths link-map (first out-links) memo)]
            (recur (+ num-paths this-num-paths) (rest out-links) (merge memo this-memo))))))))

(let [link-map (->> (read-file "input.txt")
                    (map read-links)
                    (into {}))]
  (first (calc-paths link-map :you {})))





(comment





  ;; Test
  (run-tests)

  '())

