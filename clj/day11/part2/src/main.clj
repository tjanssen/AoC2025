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
  [link-map start-node target-node memo]
  (if (= start-node target-node)
    [1 memo]
    (if (contains? memo start-node)
      [(start-node memo) memo]
      (loop [num-paths 0
             out-links (start-node link-map)
             memo      memo]
        (if (empty? out-links)
          [num-paths (assoc memo start-node num-paths)]
          (let [[this-num-paths this-memo] (calc-paths link-map (first out-links) target-node memo)]
            (recur (+ num-paths this-num-paths) (rest out-links) (merge memo this-memo))))))))

(let [link-map (->> (read-file "input.txt")
                    (map read-links)
                    (into {}))
      fftdac (calc-paths link-map :fft :dac {})]
  (if (> (first fftdac) 0)
    (* (first (calc-paths link-map :svr :fft {}))
       (first fftdac)
       (first (calc-paths link-map :dac :out {})))
    (* (first (calc-paths link-map :svr :dac {}))
       (first (calc-paths link-map :dac :fft {}))
       (first (calc-paths link-map :fft :out {})))))





(comment





  ;; Test
  (run-tests)

  '())

