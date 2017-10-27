(ns bitwise.engine
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [com.stuartsierra.component :as component]
            [cljs.core.async :as async :refer [chan alts!]]
            [bitwise.util :as util :refer [ms->sec]]
            [bitwise.dispatcher :as dispatcher]))

(defn ^:private handle-animation-frame [stop-chan timestamp-chan time]
  (go (when (alt! stop-chan false :default true)
       (js/requestAnimationFrame (partial handle-animation-frame stop-chan timestamp-chan))
       (async/put! timestamp-chan time))))

(defn register!
  ([engine handler]
   (swap! (:handlers engine) conj handler)
   engine)
  ([engine handler key]
   (register! engine key)
   (swap! (:lookup engine) assoc key handler)
   engine))

(defn unregister! [engine handler]
  (swap! (:handlers engine) disj handler)
  (when (keyword? handler)
    (swap! (:lookup engine) dissoc handler))
  engine)

(defrecord Engine [handlers lookup event-bus]
  component/Lifecycle
  (start [this]
    (dispatcher/dispatch event-bus {:type :engine-start})
    (let [stop-chan (chan)
          timestamp-chan (chan (async/sliding-buffer 1))]
      (js/requestAnimationFrame (partial handle-animation-frame stop-chan timestamp-chan))
      (go-loop [prev-time (<! timestamp-chan)
                curr-time (<! timestamp-chan)]
        (let [dt (ms->sec (- curr-time prev-time))]
          (doseq [f @handlers]
            (if (keyword? f)
              ((f @lookup) dt)
              (f dt)))
          (recur curr-time (<! timestamp-chan))))
      (assoc this :stop-chan stop-chan)))
  (stop [this]
    (dispatcher/dispatch event-bus {:type :engine-stop})
    (async/close! (:stop-chan this))
    (dissoc this :stop-chan)))

(defn new-engine []
  (map->Engine {:handlers (atom #{})
                :lookup (atom {})}))
