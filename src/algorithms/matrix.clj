(ns algorithms.matrix
	(:use [clojure.contrib.io :only [read-lines]])
	(:use [clojure.string :only [split trim-newline trim]]))

(defn string-matrix [lines]
	(map 
		(fn[line] (split (trim (trim-newline line)) #" ")) 
		lines))

(defn to-int [line]
	(vec (map #(Integer/parseInt %) line)))

(defn converted [s-matrix]
	(vec (map #(to-int %) s-matrix)))

(defn matrix-definition [in-file]
  (converted (string-matrix (read-lines in-file))))

(defn square-at [x y size matrix]
    (vec (for [posy (range y (+ y size))]
      (vec (for [posx (range x (+ x size))]
        (get-in matrix [posy posx]))))))

(defn line-products-in [matrix]
  (vec (map #(reduce * %) matrix)))

(defn col-products-in [matrix]
    (vec (for [x (range 0 (count matrix))]
      (reduce * (map #(get % x) matrix)))))

(defn diag-products-in [matrix]
  (let [size (count matrix)]
    [(reduce * (for [x (range 0 size)]
      (get-in matrix  [x x])))
     (reduce * (for [x (range 0 size)]
      (get-in matrix  [ x (- (dec size) x)])))]))

(defn max-product-in [matrix]
  (apply max 
      (concat 
        (line-products-in matrix)
          (col-products-in matrix)
            (diag-products-in matrix))))

;(defn find-maximum [matrix in-range]
 ; (let [size (- in-range 3 )]
  ;  (apply max 
   ;   (for [x (range 0 size) y (range 0 size)]
    ;    (let [found-product (max-product-in (square-at x y 4 matrix))]
     ;     found-product)))))

(defn find-maximum [matrix in-range]
  (let [
        size (- in-range 3 ) 
        coordinates (for [x (range 0 size) y (range 0 size)] [x y])]
    (apply max 
      (map 
        #(max-product-in (square-at (first %) (second %) 4 matrix)) coordinates))))


(defn par-find-maximum [matrix in-range]
  (let [
        size (- in-range 3 ) 
        sorter (agent 1 :validator number?)]
    (dorun
      (for [x (range 0 size) y (range 0 size)]
        (send sorter max (max-product-in (square-at x y 4 matrix)))))
    (await sorter)
    @sorter))


(defn other-par-find-maximum [matrix in-range]
  (let [
        size (- in-range 3 ) 
        coordinates (for [x (range 0 size) y (range 0 size)] [x y])]
    (apply max 
      (pmap 
        #(max-product-in (square-at (first %) (second %) 4 matrix)) coordinates))))

