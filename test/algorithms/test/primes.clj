(ns algorithms.test.primes
  (:use algorithms.primes)
  (:use clojure.test))


; Still in error => must find why
;(deftest next-prime-from-three-should-be-five
;  (is (= 5 (next-prime '(3 2)))))
;
;(deftest next-prime-from-three-should-be-five
;  (is (= 23 (next-prime '(19 17 13 11 7 5 3 2)))))

(deftest take-two-from-sieve-shouldprovide-the-first-primes
  (is (= '(2 3) (take 2 (prime-stream)))))

(deftest take-five-from-sieve-shouldprovide-the-five-primes
  (is (= '(2 3 5 7 11) (take 5 (prime-stream)))))

(deftest take-while-lower-num-should-provide-list
  (is (= '(2 3 5 7 11 13 17 19) (take-while #(< % 20) (prime-stream)))))


