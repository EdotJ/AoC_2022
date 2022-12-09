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

(defn move [directions]
  (loop [head {:x 0 :y 0}
         tail {:x (get head :x) :y (get head :y)}
         dirs directions
         visited #{}]
    (if (empty? dirs)
      [head tail visited]
      (let [new-head (move-head head (first dirs))
            new-tail (move-tail new-head tail)]
        (recur new-head new-tail (rest dirs) (conj visited new-tail))))))

(defn solve [opts]
  (let [moves (get-input)
        head {:x 0 :y 0}
        tail {:x (get head :x) :y (get head :y)}
        [final-head final-tail visited] (move moves)]
    (println (count visited))))
