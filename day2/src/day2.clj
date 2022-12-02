(ns day2
  (:require [clojure.string :as string]))

(def score-map {"X" 1
                "Y" 2
                "Z" 3})

(def win-map {"A" "Y"
             "B" "Z"
             "C" "X"})

(def draw-map {"A" "X"
              "B" "Y"
              "C" "Z"})

(def loss-map {"A" "Z"
               "B" "X"
               "C" "Y"})

(def func-map {"X" loss-map
               "Y" draw-map
               "Z" win-map})

(def game-score-map {"X" 0
                     "Y" 3
                     "Z" 6})

(defn map-check [map-to-check, game]
  (= (get map-to-check (first game)) (second game)))

(defn did-win [game]
  (map-check win-map game))

(defn did-draw [game]
  (map-check draw-map game))

(defn get-outcome-score-part1 [game]
  (if (did-win game) 6 (if (did-draw game) 3 0)))

(defn get-play-score-part1 [game]
  (get score-map (second game)))

(defn get-score-part1 [play]
  (let [game (string/split play #" ")]
    (+ (get-outcome-score-part1 game) (get-play-score-part1 game))))

(defn get-outcome-score-part2 [game]
  (get game-score-map (second game)))

(defn get-play-score-part2 [game]
  (get score-map (get (get func-map (second game)) (first game))))

(defn get-score-part2 [play]
  (let [game (string/split play #" ")]
    (+ (get-outcome-score-part2 game) (get-play-score-part2 game))))

(defn solve [opts] 
  (let [plays (string/split (slurp "input.txt") #"\n")]
    (println "Part 1 score: " (reduce + (map get-score-part1 plays)))
    (println "Part 2 score: " (reduce + (map get-score-part2 plays))))
)
