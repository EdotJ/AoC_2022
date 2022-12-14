(ns day14
  (:require [clojure.string :as string]))

(defn make-points [line]
  (partition 2 1 (map #(let [[x y] (map read-string (string/split % #","))] {:x x :y y}) (string/split line #" -> "))))

(defn make-line [line]
  (map (fn [pair] (let [[start end] pair]
                    (if (= (start :x) (end :x))
                      (map #(hash-map [(start :x) %] "W") (range (min (start :y) (end :y)) (inc (max (start :y) (end :y)))))
                      (map #(hash-map [% (start :y)] "W") (range (min (start :x) (end :x)) (inc (max (start :x) (end :x)))))))) (make-points line)))

(defn get-input []
  (apply merge (flatten (map make-line (string/split (slurp "input.txt") #"\n")))))

(defn drop-sand [points check-floor? floor end-func]
  (loop [sand-pos [500 0]
         taken points]
    (if (end-func sand-pos)
      nil
      (if (and check-floor? (= (inc (last sand-pos)) floor)) sand-pos
          (if (contains? taken [(first sand-pos) (inc (last sand-pos))])
            (if (contains? taken [(dec (first sand-pos)) (inc (last sand-pos))])
              (if (contains? taken [(inc (first sand-pos)) (inc (last sand-pos))])
                sand-pos
                (recur [(inc (first sand-pos)) (inc (last sand-pos))] taken))
              (recur [(dec (first sand-pos)) (inc (last sand-pos))] taken))
            (recur [(first sand-pos) (inc (last sand-pos))] taken))))))

(defn p1 [initial-points]
  (let [max-y (+ 2 (apply max (map second (keys initial-points))))]
    (loop [taken initial-points]
      (let [sand (drop-sand taken false max-y (fn [sand-pos] (= max-y (second sand-pos))))]
        (if (nil? sand)
          (- (count taken) (count initial-points))
          (recur (merge taken {sand "S"})))))))

(defn p2 [initial-points]
  (let [max-y (+ 2 (apply max (map second (keys initial-points))))]
    (loop [taken initial-points]
      (let [sand (drop-sand taken true max-y (fn [sand-pos] (contains? taken [500 0])))]
        (if (nil? sand)
          (- (count taken) (count initial-points))
          (recur (merge taken {sand "S"})))))))

(defn solve [opts]
  (let [points (get-input)
        min-x (apply min (map first (keys points)))
        max-x (apply max (map first (keys points)))
        floor-y (+ 2 (apply max (map second (keys points))))]
    (println (p1 points))
    (println (p2 points))
    ))
