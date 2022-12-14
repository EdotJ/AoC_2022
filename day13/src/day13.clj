(ns day13
  (:require [clojure.string :as string]))

(defn parse-packet [packet]
  (eval (read-string packet)))

(defn get-input []
  (->> (string/split (slurp "input.txt") #"\n\n")
       (map #(string/split % #"\n"))
       (map (fn [pair] (map parse-packet pair)))))

(defn get-smaller [left right]
  (if (< (count right) (count left)) right left))

(defn check-order [packets]
  (let [[left right] packets]
    (cond
      (and (number? left) (number? right)) (- right left)
      (and (sequential? left) (sequential? right)) (loop [results
                                                          (map-indexed (fn [idx item] (check-order [(nth left idx) (nth right idx)])) (get-smaller left right))]
                                                     (cond
                                                       (nil? (first results)) (- (count right) (count left))
                                                       (= 0 (first results)) (recur (rest results))
                                                       :else (first results)))
      (and (sequential? left) (number? right)) (check-order [left [right]])
      (and (number? left) (sequential? right)) (check-order [[left] right]))))

(defn solve [opts]
  (let [input (get-input)
        order-results (mapv check-order input)
        sorted-packets (vec (reverse (sort #(check-order [%1 %2]) (concat (apply concat input) [[2]] [[6]]))))]
    (println (reduce-kv (fn [init key val] (+ init (if (neg? val) 0 (+ key 1)))) 0 order-results))
    (println (reduce-kv (fn [init key val] (* init (if (or (= val [2]) (= val [6])) (+ key 1) 1))) 1 sorted-packets))))
