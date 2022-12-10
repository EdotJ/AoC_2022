(ns day10
  (:require [clojure.string :as string]))

(defn read-instruction [line]
  (let [str (string/split line #" ")] (if (> (count str) 1)
                                        {:cmd (first str) :v (read-string (last str))}
                                        {:cmd (first str)})))

(defn get-input []
  (map read-instruction (string/split (slurp "input.txt") #"\n")))

(defn do-instruction [instruction x]
  (case (instruction :cmd)
    "noop" [x]
    "addx" [x (+ x (instruction :v))]))

(defn execute [instructions]
  (loop [i instructions
         register 1
         cycles []]
    (if (empty? i) cycles
        (let [new-cycles (concat cycles (do-instruction (first i) register))]
          (recur (rest i) (last new-cycles) new-cycles)))))

(defn p1 [program-results]
  (reduce + (map #(* % (nth program-results (- % 2))) (range 20 260 40)))) ;; subtract 2 because of 0-based vectors AND we want the val during the op

(defn draw-pixel [value cycle-count]
  (if (<= (dec value) cycle-count (inc value)) "#" "."))

(defn draw-crt [results]
  (map-indexed (fn [idx item] (do (draw-pixel (nth results idx) (mod idx 40)))) results))

(defn solve [opts]
  (let [instructions (get-input)
        results (execute instructions)
        crt (draw-crt (concat (take 1 results) results))]
    (println "Part One" (p1 results))
    (println "-----------------")
    (println (string/join "\n" (map #(string/join %) (partition 40 crt))))))
