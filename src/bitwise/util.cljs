(ns bitwise.util
  (:require [clojure.pprint :refer (cl-format)]
            [cljs.core.match :refer-macros (match)]
            [goog.string :as gstring]
            [goog.string.format]))

(defn _ [s & args]
  (match [s args]
         [_ nil] s
         :else (apply gstring/format s args)))

(defn display-as-binary [n]
  (cl-format nil "~,'0',B" n))
