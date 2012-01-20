(ns algorithms.sorting)

(defn q-sort [datas]
	(if-let [[head & tail] datas]
		(if (empty? tail)
			(filter #(not (nil? %)) (list head))
			(concat (q-sort (filter #(< % head) tail))
					(filter #(not (nil? %)) (list head))
					(q-sort (remove #(< % head) tail))))))

(defn rq-sort [datas]
	(time
		(letfn [(inner-sort [todo done]
			(if (empty? todo)
				done
				(let [top (peek todo)]
						(cond 
							(= 0 (count top)) (recur (pop todo) done)
							(= 1 (count top)) (recur (pop todo) (cons top done))
							:else (let [[head & tail] top]
									(recur (conj (pop todo)
										(vec (filter #(< % head) tail)) 
										[head] 
										(vec (remove #(< % head) tail))) done))))))]
			(apply concat (inner-sort [datas] [])))))



(defn work-on [files]
	(lazy-seq (loop [[part & parts] files]
		(if-let [[pivot & xs] (seq part)]
			(let [smaller? #(< % pivot)]
				(recur (list* (filter smaller? xs)
						pivot
						(remove smaller? xs)
						parts)))
			(when-let [[x & parts] parts]
				(cons x (work-on parts)))))))

	