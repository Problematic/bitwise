(ns bitwise.dispatcher
  (:require [cljs.core.async :as async :refer [chan]]
            [com.stuartsierra.component :as component]))

(defn dispatch [event-bus event]
  (cond
    (sequential? event) (doseq [e event]
                          (dispatch event-bus e))
    :else (async/put! (:event-chan event-bus) event)))

(defn tap
  ([event-bus]
   (tap event-bus (chan)))
  ([event-bus ch]
   (tap event-bus ch true))
  ([event-bus ch close?]
   (async/tap (:event-mult event-bus) ch close?)))

(defn sub
  ([event-bus topic]
   (sub event-bus topic (chan)))
  ([event-bus topic ch]
   (sub event-bus topic (chan) true))
  ([event-bus topic ch close?]
   (async/sub (:event-pub event-bus) topic ch close?)))

(defrecord EventBus [event-chan event-mult event-pub]
  component/Lifecycle
  (start [this]
    (let [event-chan (chan)
          event-mult (async/mult event-chan)
          event-pub (async/pub (async/tap event-mult (chan)) :type)]
      (-> this
          (assoc :event-chan event-chan)
          (assoc :event-mult event-mult)
          (assoc :event-pub event-pub))))
  (stop [this]
    (async/close! event-chan)
    (-> this
        (assoc :event-chan nil)
        (assoc :event-mult nil)
        (assoc :event-pub nil))))

(defn new-event-bus []
  (map->EventBus {}))
