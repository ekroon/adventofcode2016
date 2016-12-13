(ns adventofcode.day11
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :refer [difference union]]))

(def input (slurp (clojure.java.io/resource "day11/input")))

(def example
  {:e 0
   :i [[1 0][2 0]]})

(def assignment1
  {:e 0
   :i [[0 1][0 0][0 1][0 0][0 0]]})

(def assignment2
  {:e 0
   :i [[0 0][0 0][0 1][0 0][0 1][0 0][0 0]]})

(defn normalize [state]
  (update state :i sort))

(defn validate-state [state]
  (let [items      (:i state)
        generators (into #{} (map first items))]
    (every? (fn [[g m]]
              (or (= g m)
                  (not (contains? generators m))))
            items)))

(defn- create-combinations [items combi val]
  (let [items (->> (map #(partition 2 (reduce (fn [r v] (assoc r v val)) items %)) combi)
                   (map #(vec (map vec %)))
                   (map #(into [] (sort %))))]
    (map (fn [i] {:e val :i i}) items)))

(defn generate-next-states [state]
  (let [items (-> state :i sort flatten vec)
        idxs1 (map-indexed (fn [i v] (when (= v (:e state)) i )) items)
        idxs2 (remove nil? idxs1)
        combi (concat (combo/combinations idxs2 1)
                      (combo/combinations idxs2 2))]
    (->> (concat (when (< (:e state) 3)
                   (create-combinations items combi (inc (:e state))))
                 (when (> (:e state) 0)
                   (create-combinations items combi (dec (:e state)))))
         (filter validate-state))))

(defn final-state? [state]
  (every? #(= 3 %) (flatten (:i state))))

(defn solver [state]
  (loop [queue [[state 0]]
         seen  {}
         counter 0]
    (let [[state n] (first queue)]
      (when (= 0 (mod counter 10000))
        (println (format "%s rounds" counter)))
      (cond (get seen state)
            (recur (rest queue) seen (inc counter))

            (final-state? state)
            n

            state
            (recur (concat (rest queue) (map (fn [s] [s (inc n)]) (generate-next-states state)))
                   (assoc seen state n)
                   (inc counter))

            (not state)
            (-> (filter final-state? seen)
                vals
                first)

            :else (assert false)))))

(defn solve [state]
  (format "input %s -> %s steps" state (solver (normalize state))))

(defn -main [& args]
  (println (solve example))
  (println (solve assignment1))
  (println (solve assignment2)))
