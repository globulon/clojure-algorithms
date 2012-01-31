(ns algorithms.core)

(defn decompose [n]
  (letfn
    [(decompose-iter [todo stacked]
       (if (< todo 10)
         (conj stacked todo)
         (let [r (rem todo 10) q (quot todo 10)]
           (recur q (conj stacked r)))))]
    (decompose-iter n [])))

(defn recompose [tenth]
  (reduce +
    (map
      first
      [tenth
       (reduce
         (fn [[sum power] current] [(+ sum (* power current)) (* 10N power)])
         [0N 10N]
         (drop 1 tenth))])))

