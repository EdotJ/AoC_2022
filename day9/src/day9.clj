(ns day9
  (:require [clojure.string :as string]))

(defn make-commands [move]
  (repeat (read-string (last move)) (first move)))

(defn get-input []
  (flatten (map make-commands (map #(string/split % #" ") (string/split (slurp "input.txt") #"\n")))))

(defn move-head [head direction]
  (let [new-head (cond
                   (= "R" direction) {:x (inc (get head :x)) :y (get head :y)}
                   (= "L" direction) {:x (dec (get head :x)) :y (get head :y)}
                   (= "U" direction) {:x (get head :x) :y (inc (get head :y))}
                   (= "D" direction) {:x (get head :x) :y (dec (get head :y))})]
    new-head))

(defn pos-diff [head tail key]
  (abs (- (get head key) (get tail key))))

(defn move-tail [head tail]
  (if (and (< (pos-diff head tail :x) 2) (< (pos-diff head tail :y) 2)) tail
      (let [horizontal-indicator (Integer/signum (- (get head :x) (get tail :x)))
            vertical-indicator (Integer/signum (- (get head :y) (get tail :y)))
            new-tail (cond
                       (or (= (pos-diff head tail :x) 2) (= (pos-diff head tail :y) 2)) {:x (+ (get tail :x) horizontal-indicator) :y (+ (get tail :y) vertical-indicator)}
                       (= (pos-diff head tail :x) 2) {:x (+ (get tail :x) horizontal-indicator) :y (get tail :y)}
                       (= (pos-diff head tail :y) 2) {:x (get tail :x) :y (+ (get tail :y) vertical-indicator)})]
        new-tail)))

(defn update-visited [visited tail]
  (conj visited tail))

(defn move-rope [rope direction]
  (loop [r (vec rope)
         dir direction
         idx 0]
    (cond
      (= idx 0) (recur (assoc r 0 (move-head (first r) direction)) dir (inc idx))
      (= idx (count rope)) r
      :else (recur (assoc r idx (move-tail (nth r (dec idx)) (nth r idx))) dir (inc idx)))))

(defn move [directions length]
  (loop [rope (repeat length {:x 0 :y 0})
         dirs directions
         visited #{}]
    (if (empty? dirs)
      [rope visited]
      (let [new-rope (move-rope rope (first dirs))
            new-tail (last new-rope)]
        (recur new-rope (rest dirs) (conj visited new-tail))))))

(defn solve [opts]
  (let [moves (get-input)
        head {:x 0 :y 0}
        tail {:x (get head :x) :y (get head :y)}
        [rope-p1 visited-p1] (move moves 2)
        [rope-p1 visited-p2] (move moves 10)]
    (println (count visited-p1))
    (println (count visited-p2))))
