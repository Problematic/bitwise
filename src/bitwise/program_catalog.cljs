(ns bitwise.program-catalog
  (:require [bitwise.random :refer [rand-char]]))
  

(defn get-output
  ([input]
    (let [output (char input) ] output)))

(def num (get-random-char))

(def work {:name "work"
           :complexity 1.5
           :memory 128
           :on-fork (fn [state update!]
                      (update! assoc :input (rand-char) ))
           :output (.toString (int num) 2)
           :doWork #()
           :on-complete {:type :increase-resource
                         :resource :data
                         :amount 1}})

(def program-catalog {:work work})
