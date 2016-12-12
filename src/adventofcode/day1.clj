(ns adventofcode.day1)

(def input
  (slurp (clojure.java.io/resource "day1/input")))

(def directions
  {:r {[0 1]  [1 0]
       [1 0]  [0 -1]
       [0 -1] [-1 0]
       [-1 0] [0 1]}})
(def directions (assoc directions :l (clojure.set/map-invert (:r directions))))

(defn parse [s]
  (re-seq #"([RL]{1})(\d+)" s))

(defn str->keyword [s]
  (-> s clojure.string/lower-case keyword))

(defn str->int [s]
  (Integer/parseInt s))

(defn next-direction [direction current]
  (get-in directions [direction current]))

(defn first-duplicate [r v]
  (if (contains? r v)
    (reduced v)
    (conj r v)))

(defn solve [input]
  (let [parsed
        (map (fn [p] [(str->keyword (nth p 1))
                      (str->int (nth p 2))])
             (parse input))

        steps
        (map second parsed)

        directions
        (rest (reductions
               (fn [r v] (next-direction v r))
               [0 1] (map first parsed)))

        mapped
        (mapcat (fn [[x y] s] (repeat s [x y]))
             directions steps)

        reduced
        (reductions (fn [[a b] [x y]] [(+ a x) (+ b y)])
                    mapped)]
    [(apply + (map #(Math/abs %) (last reduced)))
     (apply + (map #(Math/abs %) (reduce first-duplicate #{} reduced)))]))

(defn -main [& args]
  (println (solve input)))
