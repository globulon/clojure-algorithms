(ns algorithms.euler
	(:use algorithms.tools)
	(:use algorithms.coins)
  	(:use algorithms.primes)
  	(:use algorithms.matrix)
  (:use algorithms.lychrel)
  (:use algorithms.core)
	(:use [clojure.contrib.io :only [read-lines]]))

(defn div-by [n]
	(fn [input] (= 0 (rem input n))))

(defn problem-one []
	(let [match? #(or ((div-by 3) %) ((div-by 5) %))]
		(reduce + 0 (filter  match? (range 1 1000)))))

(defn lower-than? [limit]
	(fn [number]
		(< number limit)))

(defn problem-two []
	(reduce + 
		(filter #(even? %)
			(take-while  (lower-than? 4000000) (fibo-seq)))))

(defn problem-three[]
	(time
		(first (prime-factors 600851475143))))

(defn problem-four []
	(let [limit (* 999 999)]
			(map (fn [n] [n (prime-factors n)])
				(filter #(< % limit)
					(for [x (range 1 10) y (range 0 10) z (range 0 8)]
						(+ (* 100000 x) (* 10000 y) (* 1000 z) (* 100 z) (* 10 y) x))))))

(defn problem-six [] 
	(let [squares (square-sum-to 100) sum (sum-to 100)]
		(-  (*  sum sum) squares)))

(defn problem-seven [at-position]
  (time
      (nth (prime-stream) at-position)))


(def from-bignum "/dev/projects/clojure/leinengen/algorithms/bignum")

(defn char-seq [from-defintion]
	(lazy-seq (apply concat (map #(lazy-seq %) (read-lines from-defintion)))))

(defn to-number [character]
	(Integer/parseInt (str character)))

(defn extract-product [numbers found]
	(if (< (count numbers) 5)
		found 
		(let [product (reduce * 1 (take 5 numbers))]
			(recur (drop 1 numbers) (max product found)))))

(defn problem-eight []
	(time
		(let [numbers (map to-number (char-seq from-bignum))]
			(extract-product numbers 1))))

(defn problem-nine[] 
	(map (fn [[a b]] [a b (- 1000 a b) (* a b (- 1000 a b))]) 
		(filter #(and (not (ratio? (first %))) (not (ratio? (second %))))
			(for [b (range 1 501) ]
				(let [a (/ (- 1000000 (* 2000 b)) (* 2 (- 1000 b)))]
					[a b])))))


(defn problem-ten [limit]
  (reduce + (take-while #(< % limit) (prime-stream))))

(def from-matrix-file "/dev/projects/clojure/leinengen/algorithms/matrix")
(def from-big-matrix-file "/dev/projects/clojure/leinengen/algorithms/matrix-big")

(defn solve-problem-eleven [in-file for-ramge]
	(let [matrix (matrix-definition in-file)]
		(dorun
			(for [x (range 0 25)]
				(do
					(time
						(println "parallel" (par-find-maximum matrix for-ramge)))
					(time	
						(println "other parallel" (other-par-find-maximum matrix for-ramge)))					
					(time
						(println "not parallel" (find-maximum matrix for-ramge))))))))

(defn problem-eleven []
	(solve-problem-eleven from-matrix-file 20))

(defn reduced-factors [number]
	(map (fn [entry] [(first entry) (count (second entry))])
		(group-by first (map (fn[n] [n 1])(prime-factors number)))))

(defn number-of-dividors [number]
	(let [powers (map #(second %) (reduced-factors number))]
		(reduce * (map #(inc %) powers))))		

(defn rec-finder [number limit]
		(let [
				total (/ (* number (inc number)) 2)
				nb (number-of-dividors total)
			]
			(if (> nb limit)
				total
				(recur (inc number) limit))))


(defn problem-twelve [limit]
	(time (rec-finder 1 limit)))


(defn problem-sixteen [] 
	(let [result (exponent 2 1000)]
		(println result)
		(sum-digits result)))

(defn problem-twenty []
	(sum-digits (factorial 100)))

(defn problem-thirty-one[]
	(combinations-for 200))

(defn problem-fifty-two[]
  (time (take 1 (drop-while #(not (= 1 (count (set (map (comp sort str (partial * %)) [2 3 4 5 6]))))) (iterate inc 1)))))



(defn problem-fifty-five[]
  (count-lychrel-numbers))
