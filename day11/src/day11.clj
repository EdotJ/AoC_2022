(ns day11
  (:require [clojure.string :as string]))

(def digit-pattern (re-pattern #"\d+"))
(def super-worried? (atom false))

(defn parse-items [item-str]
  (->> (string/split item-str #": ")
       last
       (#(string/split % #", "))
       (map read-string)))

(defn parse-op [op-str]
  (->> (last (string/split op-str #" = "))
       (#(string/split % #" "))))

(defn parse-monkey [monkey-string]
  (let [str (string/split monkey-string #"\n")]
    {:items (parse-items (nth str 1))
     :op (parse-op (nth str 2))
     :test (read-string (re-find (re-matcher digit-pattern (nth str 3))))
     :true-monkey (read-string (re-find (re-matcher digit-pattern (nth str 4))))
     :false-monkey (read-string (re-find (re-matcher digit-pattern (nth str 5))))}))

(defn get-input []
  (mapv parse-monkey (string/split (slurp "input.txt") #"\n\n")))

(defn calculate-worry [item op]
  (let [func (resolve (symbol (op 1)))]
    (func (if (= (first op) "old") item (read-string (first op))) (if (= (last op) "old") item (read-string (last op))))))

(defn make-move [item monkey mod-val]
  (let [worry-level (if @super-worried? (calculate-worry item (monkey :op)) (int (/ (calculate-worry item (monkey :op)) 3)))
        divisable? (= (mod worry-level (monkey :test)) 0)
        next-monkey (if divisable? (monkey :true-monkey) (monkey :false-monkey))]
    [(mod worry-level mod-val) next-monkey]))

(defn do-turn [monkey mod-val]
  (map #(make-move % monkey mod-val) (monkey :items)))

(defn update-monkeys [monkeys moves monkey-index]
  (let [new-monkeys (atom monkeys)]
    (swap! new-monkeys assoc monkey-index (assoc (nth @new-monkeys monkey-index) :items '())) ;; remove all items from monkey
    (doseq [move moves]
      (swap! new-monkeys assoc (last move) (update (nth @new-monkeys (last move)) :items concat [(first move)])))
    @new-monkeys))

(defn do-round [initial-monkeys initial-inspections mod-val]
  (loop [monkeys initial-monkeys
         inspections initial-inspections
         monkey 0]
    (if (= monkey (count monkeys)) [monkeys inspections]
        (let [moves (do-turn (nth monkeys monkey) mod-val)]
          (recur (update-monkeys monkeys moves monkey) (update inspections monkey + (count moves)) (inc monkey))))))

(defn play [monkeys round-limit]
  (let [mod-val (apply * (map #(get % :test) monkeys))]
   (loop [m monkeys
         inspections (vec (repeat (count monkeys) 0))
         r 0]
    (if (= r round-limit) [m inspections]
        (let [round-results (do-round m inspections mod-val)]
          (recur (first round-results) (last round-results) (inc r)))))))

(defn solve [opts]
  (let [monkeys (get-input)]
    (println "Monkey business (Part One)" (apply * (take 2 (sort > (last (play monkeys 20))))))
    (swap! super-worried? false?)
    (println "Monkey business (Part Two)" (apply * (take 2(sort > (last (play monkeys 10000))))))
    ))
