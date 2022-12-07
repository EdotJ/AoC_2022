(ns day7
  (:require [clojure.string :as string]))

(def SPACE_IN_DEVICE 70000000)
(def UPDATE_SPACE_NEEDED 30000000)

(defn get-input []
  (string/split (slurp "input.txt") #"\n"))

(defn traverse [input acc dir-key]
  (if (empty? @input)
    acc
    (let [cmds (string/split (first @input) #" ")]
      (swap! input #(drop 1 %))
      (if (= (first cmds) "$")
        (if (= (second cmds) "cd")
          (if (= (last cmds) "..")
            acc
            (if (nil? dir-key)
              (traverse input {} (last cmds))
              (let [new-acc (assoc acc :children (if  (nil? (get acc :children))
                                                   [(traverse input {:name (last cmds)} (last cmds))]
                                                   (into [] (concat (get acc :children) [(traverse input {:name (last cmds)} (last cmds))]))))]
                (traverse input new-acc dir-key))))
          (traverse input acc dir-key))
        (if (= (first cmds) "dir")
          (traverse input acc dir-key)
          (traverse input (assoc acc :children (into [] (concat (get acc :children) [{:size (read-string (first cmds)) :name (last cmds)}]))) dir-key))))))



(defn part-one [file acc limit]
  (let [children (get file :children)]
    (if (nil? children)
      (get file :size)
      (let [dir-size (reduce + (map #(part-one % acc limit) children))]
        (do
          (swap! acc + (if (<= dir-size limit) dir-size 0))
          dir-size)))))

(defn total-used-space [file]
  (if (nil? (get file :children))
    (get file :size)
    (reduce + (map #(total-used-space %) (get file :children)))))

(defn dir-sizes [file acc]
  (let [children (get file :children)]
    (if (nil? children)
      (get file :size)
      (let [dir-size (reduce + (map #(dir-sizes % acc) children))]
        (do
          (swap! acc concat [dir-size])
          dir-size)))))

(defn solve [opts]
  (let [input (atom (get-input))
        file-system (traverse input {} nil)
        dirs-acc (atom [])
        used-space (total-used-space file-system)
        needed-space (- UPDATE_SPACE_NEEDED (- SPACE_IN_DEVICE used-space))
        directory-sizes (do (dir-sizes file-system dirs-acc)
                            (sort @dirs-acc))]
    (println "Part one" (->> directory-sizes
                             (filter #(< % 100000))
                             (reduce +)))
    (println "Part two" (->> directory-sizes
                             (filter #(> % needed-space))
                             (apply min)))))
