(ns day4
  (:require [clojure.string :as string]))

(defn get-input-new-line []
  (string/split (slurp "input.txt") #"\n"))

(defn make-pair [text]
  (let [split (string/split text #"-")] {:start (read-string (first split)) :end (read-string (last split))}))

(defn pair-overlaps? [a b]
  (and (>= (:end a) (:start b)) (<= (:end a) (:end b))))

(defn overlaps-partially? [pair]
  (or (apply pair-overlaps? pair) (apply pair-overlaps? (reverse pair))))

(defn pair-overlaps-completely? [a b]
  (and (<= (:start a) (:start b)) (>= (:end a) (:end b))))

(defn overlaps-completely? [pair]
  (or (apply pair-overlaps-completely? pair) (apply pair-overlaps-completely? (reverse pair))))

(defn get-overlaps [pairs filter-fn]
  (->> pairs
       (filter filter-fn)
       count))

(defn solve [opts]
  (let [pairs (map #(let [ranges (string/split % #",")] (map make-pair ranges)) (get-input-new-line))]
    (println (get-overlaps pairs overlaps-completely?))
    (println (get-overlaps pairs overlaps-partially?))
    ))
