(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn solve [opts] (
    let [elf-nums (string/split (slurp "input.txt") #"\n\n")
        calorie-counts (map #(reduce + (mapv read-string (string/split % #"\n"))) elf-nums)]
        (println "Most calories:" (apply max calorie-counts))
        (println "Sum of top 3: " (->> (sort calorie-counts)
                                       (reverse)
                                       (take 3)
                                       (reduce +)))
))
