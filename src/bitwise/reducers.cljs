(ns bitwise.reducers
  (:require [cljs.core.async :as async :refer [chan]]))

(defmulti reduce-game-state (fn [state action] (:type action)))

(defmethod reduce-game-state :fork-process [state {:keys [program]}]
  (-> state
      (update-in [:processes] conj {:pid (:nextpid state)
                                    :program program
                                    :progress (chan (async/sliding-buffer 1))})
      (update-in [:nextpid] inc)))

(defmethod reduce-game-state :kill-process [state {:keys [pid]}]
  (update-in state [:processes] #(filterv (comp not (partial = pid) :pid) %)))

(defmethod reduce-game-state :complete-process [state action]
  (let [on-complete (get-in action [:program :on-complete])]
    (on-complete state)))

(defn reduce-state [state action]
  (->> [state action]
       (apply reduce-game-state)))
