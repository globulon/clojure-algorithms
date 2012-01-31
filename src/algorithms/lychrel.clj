(ns algorithms.lychrel
  (:use algorithms.core))

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
        (range 1N 10001N)))))



