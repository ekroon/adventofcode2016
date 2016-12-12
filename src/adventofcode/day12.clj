(ns adventofcode.day12)

(def input (slurp (clojure.java.io/resource "day12/input")))

(defmulti opcode (fn [context [opcode x y]] opcode))

(defn context []
  {"a" 0
   "b" 0
   "c" 0
   "d" 0
   :counter 0})

(defmethod opcode "cpy"
  [context [_ x y]]
  (cond
    (get context x)
    (-> context
        (assoc-in [y] (get context x))
        (update-in [:counter] inc))
    :else
    (-> context
        (assoc-in [y] (Integer/parseInt x))
        (update-in [:counter] inc))))

(defmethod opcode "inc"
  [context [_ x _]]
  (-> context
      (update-in [x] inc)
      (update-in [:counter] inc)))

(defmethod opcode "dec"
  [context [_ x _]]
  (-> context
      (update-in [x] dec)
      (update-in [:counter] inc)))

(defmethod opcode "jnz"
  [context [_ x y]]
  (if (= 0 (get context x))
    (update-in context [:counter] inc)
    (update-in context [:counter] #(+ % (Integer/parseInt y)))))

(defn parse [input]
  (map #(into [] (rest %)) (re-seq #"(\w{3}) (\S+)[ ]*(\S+)*\n*" input)))

(defn evaluate [program context]
  (loop [current (nth program (:counter context) nil)
         context context
         loops 0]
    (if current
      (let [new-context (opcode context current)]
        (recur (nth program (:counter new-context) nil)
               new-context
               (inc loops)))
      context)))

(defn solve [input context]
  (let [parsed (parse input)]
    (evaluate parsed context)))

(defn -main [& args]
  (println (solve input (context)))
  (println (solve input (assoc (context) "c" 1))))
