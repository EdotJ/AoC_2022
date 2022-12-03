(ns day3
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn convert-char [char]
  (let [num-val (int char)]
    ; A = 65; Z = 90; a = 97 
    (if (< num-val 91) (+ (- num-val 64) 26) (- (- num-val 70) 26))))

(defn to-set [text]
  (into #{} (string/split text #"")))

(defn find-common [sets]
  (as-> sets v
          (apply set/intersection v)
          (first v)
          (.charAt v 0)
          (convert-char v)))

(defn part-one [compartments]
  (map find-common compartments))

(defn part-two [compartments]
  (map find-common (partition 3 compartments)))

(defn solve [opts]
  (let [rucksack-list (string/split (slurp "input.txt") #"\n")
        compartments (map #(vector (to-set (subs % 0 (/ (count %) 2))) (to-set (subs % (/ (count %) 2)))) rucksack-list)
        rucksacks (map to-set rucksack-list)]
    (println (reduce + (part-one compartments)))
    (println (reduce + (part-two rucksacks)))))
