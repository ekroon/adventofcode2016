(ns adventofcode.day11
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :refer [difference union]]))

(def input (slurp (clojure.java.io/resource "day11/input")))
(def example
  {1 {:current true
      :items #{{:type :microchip
                :element :hydrogen}
               {:type :microchip
                :element :lithium}}
      :floor 1}
   2 {:items #{{:type :generator
               :element :hydrogen}}
      :floor 2}
   3 {:items #{{:type :generator
               :element :lithium}}
      :floor 3}
   4 {:items #{}
      :floor 4}})

(defn valid-floor? [floor]
  (let [generator? (not (every? #(= :microchip (:type %)) (:items floor)))]
    (if generator?
      (every? #(if (= :generator (:type %))
                 true
                 (do
                   (contains? (:items floor) {:type :generator
                                              :element (:element %)})))
              (:items floor))
      true)))

(defn valid-state? [state]
  (every? valid-floor? (vals state)))

(defn combinations [items n]
  (map set (combo/combinations (vec items) n)))

(defn generate-steps [state]
  (let [current    (->> state vals (filter :current) first)
        floors     (->> state vals (remove #(or (= current %)
                                                (< 1 (Math/abs
                                                      (- (:floor current)
                                                         (:floor %)))))))
        items      (concat (combinations (:items current) 1)
                        (combinations (:items current) 2))
        new-floors (mapcat
                    (fn [i]
                      (let [n (-> current
                                  (update :items #(difference % i))
                                  (dissoc :current))]
                        (map
                         (fn [f] [n
                                  (-> f
                                      (update-in [:items] #(union % i))
                                      (assoc :current true))])
                         floors)))
                    items)
        new-states (map (fn [f] (reduce #(assoc %1 (:floor %2) %2) state f))
                        new-floors)
        only-valid (filter valid-state? new-states)]
    only-valid))

(defn solve [input]
  (generate-steps example))

(defn -main [& args]
  (println (solve input)))
