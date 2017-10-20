(ns bitwise.process
  (:require [bitwise.util :as util]
            [bitwise.program-catalog :refer [program-catalog]]))

(defprotocol Executable
  (start [this])
  (stop [this])
  (running? [this])
  (complete? [this])
  (progress [this]))

(defrecord Process [pid program started-at]
  Executable
  (start [this]
    (assoc this :started-at (util/timestamp)))
  (stop [this]
    (assoc this :started-at nil))
  (running? [this]
    (not (nil? (:started-at this))))
  (progress [this]
    (if (running? this)
      (let [elapsed (- (util/timestamp) (:started-at this))
            duration (get-in program-catalog [(:program this) :complexity])]
        (max (min (/ (util/ms->sec elapsed) duration) 1.0) 0.0))
      0.0))
  (complete? [this]
    (and (running? this)
         (>= (util/timestamp)
             (+ (:started-at this) (util/sec->ms
                                    (get-in program-catalog [(:program this) :complexity])))))))

(deftype ProcessHandler []
  Object
  (tag [this v] "process")
  (rep [this v] (into {} v)))

(def write-handler
  {Process (ProcessHandler.)})

(def read-handler
  {"process" (fn [{:keys [pid program started-at]}]
               (Process. pid program started-at))})