(ns bitwise.reducers
  (:require [cljs.core.async :as async :refer [chan]]
            [bitwise.util :as util]
            [bitwise.process :as process]))

(defmulti reduce-game-state (fn [state action] (:type action)))

(defmethod reduce-game-state :fork-process [state {:keys [program]}]
  (-> state
      (update-in [:processes] conj [(:nextpid state) (process/Process. (:nextpid state) program nil)])
      (update-in [:nextpid] inc)))

(defmethod reduce-game-state :kill-process [state {:keys [pid]}]
  (update-in state [:processes] dissoc pid))

(defmethod reduce-game-state :execute-process [state {:keys [pid]}]
  (let [process (get-in state [:processes pid])]
    (if (process/running? process)
      state
      (let [architecture (get-in state [:architecture])
            duration (/ (process/complexity process) (get-in architecture [:cpu :speed]))]
        (assoc-in state [:processes pid] (-> process
                                             (process/start)
                                             (assoc-in [:duration] duration)))))))

(defmethod reduce-game-state :complete-process [state {:keys [pid]}]
  (let [process (get-in state [:processes pid])]
    (assoc-in state [:processes pid] (-> process
                                         (process/stop)
                                         (dissoc :duration)))))

(defmethod reduce-game-state :increase-resource [state {:keys [resource amount]}]
  (update-in state [:resources resource] (comp
                                          (partial into {})
                                          (partial map (fn [[key val]]
                                                         [key (+ val amount)])))))

(defmethod reduce-game-state :default [state action]
  (.warn js/console (str "couldn't find method for " (:type action) ", dispatching to :default") action)
  state)

(defn reduce-state [state action]
  (->> [state action]
       (apply reduce-game-state)))
