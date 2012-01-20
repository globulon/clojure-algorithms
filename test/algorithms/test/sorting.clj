(ns algorithms.test.sorting
  (:use [algorithms.sorting])
  (:use [clojure.test]))


(deftest sorting-should-reverse-list
	(is (= '(1 2 3) (q-sort (list 3 2 1)))))
