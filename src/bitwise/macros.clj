(ns bitwise.macros)

(defmacro while-let
  [[form test] & body]
  `(loop [~form ~test]
     (when ~form
       ~@body
       (recur ~test))))
