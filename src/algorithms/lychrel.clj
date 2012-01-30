(ns algorithms.lychrel)

;; Basically looks up for Lychrel numbers
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
         (fn [[sum power] current] [(+ sum (* power current)) (* 10 N power)])
         [0 N 10 N]
         (drop 1 tenth))])))

(defn lychrel? [num]
  (letfn
    [(lychrel-iter [n iter]
       (if (= 50 iter)
         false
         (let [r (vec (reverse (decompose n)))
               sum (+ n (recompose r))
               dsum (decompose sum)]
           (if (= dsum (reverse dsum))
             true
             (recur sum (inc iter))))))]
    (lychrel-iter num 0)))

(defn count-lychrel-numbers []
  (time
    (count
      (filter
        (fn [item] (not (lychrel? item)))
        (range 1 N 10001 N)))))



