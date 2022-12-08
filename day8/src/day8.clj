(ns day8
  (:require [clojure.string :as string]))

(defn get-input []
  (map #(string/split % #"") (string/split (slurp "input.txt") #"\n")))

(defn parse-grid [grid]
  (map-indexed (fn [row-idx row] (map-indexed (fn [col-idx height] {:row row-idx :col col-idx :height (read-string height)}) row)) grid))

(defn is-edge? [tree row-count col-count]
  (or (= (get tree :row) 0) (= (get tree :row) row-count) (= (get tree :col) 0) (= (get tree :col) row-count)))

(defn get-inner-grid [grid row-count col-count]
  (->> (flatten grid)
       (remove #(is-edge? % row-count col-count))))

(defn get-edges [grid row-count col-count]
  (->> (flatten grid)
       (remove #(not (is-edge? % row-count col-count)))))

(defn get-top [tree tree-line]
  (filter #(< (get % :row) (get tree :row)) tree-line))

(defn get-bottom [tree tree-line]
  (filter #(> (get % :row) (get tree :row)) tree-line))

(defn get-left [tree tree-line]
  (filter #(< (get % :col) (get tree :col)) tree-line))

(defn get-right [tree tree-line]
  (filter #(> (get % :col) (get tree :col)) tree-line))

(defn bool-nil [ans]
  (if (nil? ans)
    false
    ans))

(defn check-visibility [tree col]
  (bool-nil (some #(>= (get % :height) (get tree :height)) col)))

(defn invisible-vertically? [tree tree-line]
  (and (check-visibility tree (get-top tree tree-line))
       (check-visibility tree (get-bottom tree tree-line))))

(defn invisible-horizontally? [tree tree-line]
  (and (check-visibility tree (get-left tree tree-line))
       (check-visibility tree (get-right tree tree-line))))

(defn get-row [grid row-num]
  (nth grid row-num))

(defn get-col [grid col-num]
  (filter #(= (get % :col) col-num) (flatten grid)))

(defn invisible? [tree grid]
  (and (invisible-horizontally? tree (get-row grid (get tree :row)))
       (invisible-vertically? tree (get-col grid (get tree :col)))))

(defn get-visible [grid inner-grid]
  (remove #(invisible? % grid) inner-grid))

(defn get-blockers [tree tree-line]
  (filter #(>= (get % :height) (get tree :height)) tree-line))

(defn get-visible-tree-count [blockers trees]
  (if (not-empty blockers)
    (+ (.indexOf trees (first blockers)) 1)
    (count trees)))

(defn get-visible-tree-count-rev [blockers trees]
  (if (not-empty blockers)
    (- (count trees) (.indexOf trees (first blockers)))
    (count trees)))

(defn get-scenic-score [tree grid]
  (let [col (get-col grid (get tree :col))
        row (get-row grid (get tree :row))
        bottom-trees (get-bottom tree col)
        top-trees (reverse (get-top tree col))
        bottom-blockers (get-blockers tree bottom-trees)
        top-blockers (get-blockers tree top-trees)
        left-trees (reverse (get-left tree row))
        left-blockers (get-blockers tree left-trees)
        right-trees (get-right tree row)
        right-blockers (get-blockers tree right-trees)]
    (* (get-visible-tree-count top-blockers top-trees)
       (get-visible-tree-count bottom-blockers bottom-trees)
       (get-visible-tree-count right-blockers right-trees)
       (get-visible-tree-count left-blockers left-trees))))

(defn get-scenic-scores [inner-grid grid]
  (map #(get-scenic-score % grid) inner-grid))

(defn solve [opts]
  (let [grid (parse-grid (get-input))
        row-count (- (count grid) 1) ;; want this to be 0-based
        col-count (- (count (first grid)) 1)
        inner-grid (get-inner-grid grid row-count col-count)
        edges (get-edges grid row-count col-count)]
    ;; (println "Visible tree count: " (+ (count edges) (count (get-visible grid inner-grid))))
    ;; (println (get-scenic-score {:row 2, :col 2, :height 3} grid))
    (println (apply max (get-scenic-scores inner-grid grid)))
    ))
