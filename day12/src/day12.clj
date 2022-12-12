(ns day12
  (:require [clojure.string :as string]))
;; http://www.loganlinn.com/blog/2013/04/22/dijkstras-algorithm-in-clojure/

(defn get-neighbor [matrix curr-height row col]
  (let [neighbor (nth (nth matrix row nil) col nil)]
    (if (or (nil? neighbor) (> (- (get neighbor :height) curr-height) 1))
      nil
      {[(get neighbor :row) (get neighbor :col)] 1})))

(defn populate-neigbors [square matrix]
  (let [neighbors [(get-neighbor matrix (get square :height) (get square :row) (dec (get square :col)))
                   (get-neighbor matrix (get square :height) (get square :row) (inc (get square :col)))
                   (get-neighbor matrix (get square :height) (dec (get square :row)) (get square :col))
                   (get-neighbor matrix (get square :height) (inc (get square :row)) (get square :col))]]
    (assoc square :neighbors (apply merge (filter #(not (nil? %)) neighbors)))))

(defn get-height [letter]
  (cond
    (= letter "S") 0
    (= letter "E") 27
    :else (- (int (.charAt letter 0)) 96)))

(defn get-input []
  (let [matrix (map-indexed (fn [row-idx row] (map-indexed (fn [col letter] {:row row-idx :col col :height (get-height letter)}) (string/split row #"")))
                            (string/split (slurp "input.txt") #"\n"))]
    (map (fn [row] (map #(populate-neigbors % matrix) row)) matrix)))

(defn get-coords [node]
  [(get node :row) (get node :col)])

(defn update-costs [matrix costs unvisited curr]
  (let [curr-cost (get costs (get-coords curr))]
    (reduce-kv
     (fn [c neighbor neighbor-cost]
       (if (unvisited neighbor)
         (update-in c [neighbor] min (+ curr-cost neighbor-cost))
         c))
     costs
     (get (nth (nth matrix (get curr :row)) (get curr :col)) :neighbors))))

(defn dijkstra [matrix src dest]
  (let [matrix-keys (map #(vector (get % :row) (get % :col)) (flatten matrix))]
    (loop [costs (assoc (zipmap matrix-keys (repeat Double/POSITIVE_INFINITY)) (get-coords src) 0)
           curr src
           unvisited (disj (apply hash-set matrix-keys) (get-coords src))]
      (cond
        (= curr dest) (select-keys costs [(get-coords dest)])
        (or (empty? unvisited) (= Double/POSITIVE_INFINITY (get costs (get-coords curr)))) costs
        :else
        (let [next-costs (update-costs matrix costs unvisited curr)
              next-node-coords (apply min-key next-costs unvisited)
              next-node (nth (nth matrix (first next-node-coords)) (last next-node-coords))]
          (recur next-costs next-node (disj unvisited (get-coords next-node))))))))

(defn get-step-count [result]
  (first (vals result)))

(defn solve [opts]
  (let [matrix (get-input)
        start (first (filter #(= (get % :height) 0) (flatten matrix)))
        end (first (filter #(= (get % :height) 27) (flatten matrix)))]
    (println "Part one" (get-step-count (dijkstra matrix start end)))
    (println "Part two" (apply min (map #(get-step-count (dijkstra matrix % end)) (filter #(= (get % :height) 1) (flatten matrix)))))
    ))
