(ns algorithms.test.matrix-spec
  (:use algorithms.matrix)
  (:use clojure.test))

(def from-file "/dev/projects/clojure/leinengen/algorithms/matrix")

(def matrix-origin [  [ 8  2 22 97]
                      [49 49 99 40]
                      [81 49 31 73]
                      [52 70 95 23] ])

(def matrix-bottom-right [  [40 62 76 36]
                            [74 04 36 16]
                            [23 57 05 54]
                            [89 19 67 48]])

(deftest loaded-matrix-from-file-should-have-right-size
  (is (= 20 (count  (matrix-definition from-file))))
  (is (= 20 (count  (first (matrix-definition from-file))))))

(deftest four-matrix-at-origin-should-match-file-content
  (is (= matrix-origin (square-at 0 0 4 (matrix-definition from-file )))))

(deftest four-matrix-at-bottom-left-should-match-file-content
  (is (= matrix-bottom-right (square-at 16 16 4 (matrix-definition from-file )))))

(deftest line-products-in-origin-matrix-should-match
  (is (= [34144 9507960 8981847 7953400] (line-products-in (square-at 0 0 4 (matrix-definition from-file ))))))

(deftest col-products-in-origin-matrix-should-match
  (is (= [1651104 336140 6414210 6514520] (col-products-in (square-at 0 0 4 (matrix-definition from-file ))))))

(deftest diag-products-in-origin-matrix-should-match
  (is (= [279496 24468444] (diag-products-in (square-at 0 0 4 (matrix-definition from-file ))))))

(deftest max-product-in-matrix-with-origin-should-match
  (is (= 24468444 (max-product-in (square-at 0 0 4 (matrix-definition from-file ))))))

(deftest find-maximum-from-file
  (is (= 70600674 (find-maximum (matrix-definition from-file) 20))))

;(deftest products-in-square-matrix-at-origin-should-match[]
;  (is (= ([] (products-in (square-at 0 0 4 (matrix-definition from-file)))))))




