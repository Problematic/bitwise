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

(defn timestamp []
  (+ (aget js/performance "timing" "navigationStart") (.now js/performance)))

(defn ms->sec [ms]
  (/ ms 1000))

(defn sec->ms [sec]
  (* sec 1000))

(letfn [(merge-deep* [a b]
          (if (map? a)
            (merge-with merge-deep* a b)
            b))]
  (defn merge-deep
    "Merge multiple nested maps."
    [& args]
    (reduce merge-deep* nil args)))
