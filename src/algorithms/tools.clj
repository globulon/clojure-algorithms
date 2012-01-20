(ns algorithms.tools)

(defn fibo-seq
  ([] (fibo-seq 0 1))
  ([a b]
    (lazy-seq
      (cons a (fibo-seq b (+ a b))))))

(defn numbers-from [n]
  (lazy-seq (cons n (numbers-from (+ 1 n)))))

(defmacro promised [expression]
   `(fn [] ~expression))

(defn fulfilled [promise]
    (promise))

(defn integers-from [n]
  (cons n [(promised (integers-from (+ n 1)))]))

(defn sum-to[n]
  (/ (* n (+ n 1)) 2))

(defn square-sum-to [n]
  (* (/ n 6) (+ 1 (* 2 n)) (inc n)))

(defn first-elements [k stream]
  (if (= 0 k)
    []
    (cons (first stream) (first-elements (dec k) (fulfilled (second stream))))))

(defn- square[x] (* x x))

(defn exponent [number at]
  (letfn [
    (exponent-rec [accumulated at]
      (if (= 0 at)
        accumulated
        (recur (* number accumulated) (dec at))))]
    (exponent-rec 1N at)))

(defn sum-digits [number]
  (letfn [
    (sum-rec [accumulated current]
      (if (= current 0)
        accumulated
        (let [remainder (rem current 10)]
          (recur 
            (+ accumulated remainder) 
            (/ (- current remainder) 10)))))]
    (sum-rec 0N number)))

(defn factorial [n]
  (letfn [
    (fact-rec [accumulated current]
      (if (= current 1)
        accumulated
        (recur (* accumulated current) (dec current))))]
    (fact-rec 1N n)))    