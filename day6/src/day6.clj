(ns day6
  (:require [clojure.string :as string]))

(defn get-input []
  (slurp "input.txt"))

(defn get-marker [signal marker-length]
  (let [marker-combos (map-indexed (fn [idx val] (set (take marker-length (drop (- idx 1) signal)))) signal)
        possible-markers (filter #(= (count %) marker-length) marker-combos)]
    (+ (.indexOf marker-combos (first possible-markers)) (- marker-length 1)))) ; addition to compensate for other letters

(defn get-start-marker [signal]
  (get-marker signal 4))

(defn get-message-marker [signal]
  (get-marker signal 14))

(defn solve [opts]
  (let [signal (get-input)]
    (println (get-start-marker signal))
    (println (get-message-marker signal))))
