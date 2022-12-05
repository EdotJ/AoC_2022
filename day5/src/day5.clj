(ns day5
  (:require [clojure.string :as string]
            [clojure.walk :as walk]))

(def COL_LENGTH 4)

(defn get-input []
  (string/split (slurp "input.txt") #"\n\n"))

(defn read-col [stacks ind]
  (reverse (filter #(> (count %) 0) (map #(string/replace (subs % (- ind COL_LENGTH) (- ind 1)) #" " "") (string/split stacks #"\n")))))

(defn get-col-count [stacks]
  (as-> (string/split stacks #"\n") v
    (last v)
    (+ (count v) 1) ; account for the space at the end
    (/ v COL_LENGTH)))

(defn make-move-map [line]
  (into {} (map #(vector (keyword (first %)) (read-string (last %))) (partition 2 (string/split line #" ")))))

(defn move-crates [move stacks is-9001-model]
  (let [from-col (- (:from move) 1)
        to-col (- (:to move) 1)
        removed-col (drop (:move move) (nth stacks from-col))
        added-col (concat (reverse (take (:move move) (nth stacks from-col))) (nth stacks to-col))
        added-col-9001 (concat (take (:move move) (nth stacks from-col)) (nth stacks to-col))]
    (map-indexed (fn [idx col] (if (= idx from-col) removed-col (if (= idx to-col) (if is-9001-model added-col-9001 added-col) col))) stacks)
    ))

(defn rearrange [moves stacks is-9001-model]
  (let [curr-move (first moves)]
    (if (nil? curr-move)
    stacks
    (rearrange (drop 1 moves) (move-crates (first moves) stacks is-9001-model) is-9001-model))))

(defn solve [opts]
  (let [inputs (get-input)
        stacks (map #(reverse (drop 1 (read-col (first inputs) (* % COL_LENGTH)))) (range 1 (+ (get-col-count (first inputs)) 1)))
        moves (map make-move-map (string/split (second inputs) #"\n"))]
    (println "Virgin CrateMover 9000: " (string/replace (apply str (map first (rearrange moves stacks false))) #"\[|\]" ""))
    (println "Chad CrateMover 9001: " (string/replace (apply str (map first (rearrange moves stacks true))) #"\[|\]" ""))))
