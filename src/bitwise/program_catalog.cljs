(ns bitwise.program-catalog)

(def work {:name "work"
           :complexity 1.5
           :memory 128
           :on-complete {:type :increase-resource
                         :resource :data
                         :amount 1}})

(def program-catalog {:work work})
