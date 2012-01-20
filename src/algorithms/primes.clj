(ns algorithms.primes
  (:import [java.lang Math]))

(defn prime-form? [number]
  "All primes greater than 3 can be written in the form 6k+/-1."
  (let [sqrt (Math/floor (Math/sqrt number))]
    (loop [f 5 dividable false]
      (if (or (> f sqrt) dividable)
        (not dividable)
        (recur
          (+ f 6)
          (or
            (= 0 (mod number f))
            (= 0 (mod number (+ 2 f)))))))))

(defn prime? [number]
  "1 is not a prime.
  All primes except 2 are odd."
  (cond
    (even? number) false
    (= 0 (mod number 3)) false
    (prime-form? number) true
    :else false
    ))

(defn next-prime [from-prime]
  (loop [current (+ 2 from-prime)]
    (if (prime? current)
      current
      (recur (+ 2 current)))))

(defn prime-stream
  ([] (lazy-seq (cons 2 (lazy-seq (cons 3 (prime-stream 5))))))
  ([from-prime]
    (lazy-seq
      (cons
        from-prime
        (prime-stream (next-prime from-prime))))))

(defn prime-factors [from-number]
  (letfn [
    (extract-factors [number limit primes factors] 
      (let [current-prime (first (take 1 primes))]
        (cond
          (> current-prime limit) factors
          (= 0 (rem number current-prime)) (recur (/ number current-prime) (/ limit current-prime) primes (cons current-prime factors))
          :else (recur number limit (drop 1 primes) factors))))
    ]
    (extract-factors from-number from-number (prime-stream) [])))


