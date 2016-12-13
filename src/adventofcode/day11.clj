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

(def assignment
  {1 {:current true
      :floor 1
      :items #{{:type :generator
                :element :polonium}
               {:type :generator
                :element :thulium}
               {:type :microchip
                :element :thulium}
               {:type :generator
                :element :promethium}
               {:type :generator
                :element :ruthenium}
               {:type :microchip
                :element :ruthenium}
               {:type :generator
                :element :cobalt}
               {:type :microchip
                :element :cobalt}}}
   2 {:floor 2
      :items #{{:type :microchip
                :element :polonium}
               {:type :microchip
                :element :promethium}}}
   3 {:floor 3
      :items #{}}
   4 {:floor 4
      :items #{}}})

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

(defn is-final? [state]
  (let [result
        (every? (comp empty? :items (partial get state)) [1 2 3])]
    ;;(println result)
    result))

(defn solver [state]
  (loop [queue [{:state state :steps []}]
         seen  {}]
    (let [state (-> queue first :state)
          steps (-> queue first :steps)]
      (when (= 0 (mod (count seen) 100))
        (println (count seen) " " (count queue)))
      (cond (not state)
            (do
              #_(dorun (map (fn [[k v]] (println k)) seen))
              (let [result (into {} (filter (fn [[k v]] (is-final? k)) seen))]
                #_(println (first (first result)))
                result))

            (get seen state)
            (do
              (recur (rest queue) seen))

            (is-final? state)
            (do
              (recur (rest queue) (assoc seen state (first queue))))

            state
            (do
              (let [add (map (fn [s] {:state s :steps (concat steps [state])})
                             (remove #(get seen %) (generate-steps state)))]
                (println "add to queue: " (count add))
                (println "added: " add)
                (recur (concat (rest queue) add)
                       (assoc seen state (first queue)))))

            :else
            (assert false)))))

(defn solve [input]
  (-> (solver example)
      vals first :steps count))

(defn -main [& args]
  (println (solve input)))
