(ns algorithms.maths)

(defn factorial [number]
    (letfn [(factorial-acc [n accumulator]
        (if(zero? n) 
            accumulator
            (recur (dec n) (* n accumulator))))]
    (factorial-acc number 1)))