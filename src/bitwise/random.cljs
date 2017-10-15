(ns bitwise.random
  (:refer-clojure :exclude [rand-int]))

(defn rand-int
  ([end] (clojure.core/rand-int end))
  ([start end] (+ start (clojure.core/rand-int (- end start)))))
