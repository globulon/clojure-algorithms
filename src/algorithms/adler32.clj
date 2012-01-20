(ns algorithms.adler32)

(def base 65521)

(defn cumulate [[a b] x]
  (let [a-prim (rem (+ a (bit-and x 255)) base)]
    [a-prim (+ b a-prim)]))

(derive clojure.lang.LazySeq ::collection)

(defmulti checksum class)
(defmethod checksum String [data]
  (checksum  (lazy-seq (.getBytes data))))
(defmethod checksum ::collection [data]
  (let [[a b] (reduce cumulate [1 0] data)]
    (bit-or (bit-shift-left b 16) a)))
