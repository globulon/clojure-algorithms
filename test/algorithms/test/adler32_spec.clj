(ns algorithms.test.adler32-spec
  (:use algorithms.adler32)
  (:use clojure.test))

(deftest checksum-with-favourit-sentence-should-produce-result
  (is (= 1839204552 (checksum "Thumper is a cute rabbit"))))