(ns algorithms.fibonacci)

(defn fibo-seq
	([]  (fibo-seq 0 1))
	([a b]
		(lazy-seq
			(cons a (fibo-seq b (+ a b))))))

