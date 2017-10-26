(ns bitwise.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [reagent.core :as r]
            [cljs.core.async :as async :refer [<! >! chan]]
            [bitwise.util :as util :refer [ms->sec sec->ms timestamp]]
            [clojure.pprint :as pp]
            [bitwise.reducers :as reducers]
            [alandipert.storage-atom :refer [local-storage]]
            [bitwise.program-catalog :refer [program-catalog]]
            [bitwise.process :as process]))

(enable-console-print!)

(reset! alandipert.storage-atom/storage-delay 100)
(swap! alandipert.storage-atom/transit-read-handlers into process/read-handler)
(swap! alandipert.storage-atom/transit-write-handlers into process/write-handler)

(defn default-state [] {:nextpid 1000
                        :programs #{:work}
                        :architecture {:cpu {:cores 1
                                             :speed 0.5}
                                       :memory 512}
                        :resources {:data {:total 0
                                           :current 0}}
                        :processes (sorted-map)})

(defonce game-state (local-storage
                     (r/atom (default-state))
                     :game-state))
(defonce dt-mult nil)
(defonce event-chan (chan))
(defonce event-mult (async/mult event-chan))
(defonce event-pub (async/pub (async/tap event-mult (chan)) :type))

(defn process->program [process]
  ((:program process) program-catalog))

(defn dispatch! [action]
  (cond
    (fn? action) (dispatch! (action))
    (sequential? action) (doall (map dispatch! action))
    :else (let [a (assoc action :timestamp (util/timestamp))]
            (async/put! event-chan a))))

(def process-grid-styles {:display "grid"
                          :grid-template-columns "75px 1fr 1fr"
                          :grid-template-rows "30px"
                          :align-items "center"})

(def action-button-styles {:min-width 100
                           :height 25
                           :border "1px solid gray"
                           :display "flex"
                           :padding "3px 20px"
                           :align-items "center"
                           :justify-content "center"
                           :user-select "none"
                           :text-decoration "none"
                           :margin-right "10px"
                           :color "black"})

(defn action-button [props label]
  [:a (util/merge-deep {:role "button"
                        :href "#"
                        :style action-button-styles}
                       props
                       {:on-click #(do (.preventDefault %) ((:on-click props)))})
   label])

(defn progress-button [props progress label]
  (let [pct (* progress 100)]
    [action-button
     (util/merge-deep {:style {:background (str "linear-gradient(to right, lightgray " pct "%, white " pct "%)")}}
                      props)
     label]))

(defn process-progress-button [props process label]
  (let [progress (r/atom 0.0)]
    (fn [props process label]
      (reset! progress (process/progress process))
      [progress-button props @progress label])))

(defn process-info [process]
  (fn [process]
    (let [program (process->program process)
          pct (* (process/progress process) 100)]
      [:div {:style (merge
                     process-grid-styles
                     {})}
       [:div (:pid process)]
       [:div (:name program)]
       [:div {:style {:display "flex"}}
        [process-progress-button {:on-click #(dispatch! {:type :execute-process
                                                         :pid (:pid process)})}
         process
         "Execute"]
        [action-button {:style {:margin-left 10}
                        :on-click #(dispatch! {:type :kill-process
                                               :pid (:pid process)})}
         "Kill"]]])))

(defn process-slot []
  [:div {:style (merge
                 process-grid-styles
                 {})}
   [:div "-"]
   [:div "idle"]
   [:div]])

(defn process-list [processes]
  [:div
   [:div {:style (merge
                  process-grid-styles
                  {})}
    [:div>b "PID"]
    [:div>b "PROGRAM"]
    [:div]]
   (for [[pid process] @processes]
     ^{:key pid} [process-info process])])

(defn program-info [program]
  [:div {}
   [:div>b (:name program)]
   [:div {} (str "Complexity: " (:complexity program))]
   [:div {} (str "Size: " (:memory program) "mb")]])

(defn app []
  (let [processes (r/cursor game-state [:processes])
        architecture (r/cursor game-state [:architecture])
        process-slots-available (- (get-in @architecture [:cpu :cores]) (count @processes))]
    [:div
     [:div {:style {:display "flex"
                    :font-family "monospace"}}
      [:div {:style {:flex-grow "2"}}
       [:div
        [:div
         (str "data: " (util/display-as-binary (get-in @game-state [:resources :data :current])))]
        [process-list processes]
        (for [idx (range process-slots-available)]
          ^{:key idx} [process-slot])]]
      [:div {:style {:flex-grow "1"}}
       [:h3 "programs"]
       [:div
        (for [[key program] (map #(vector % (% program-catalog)) (:programs @game-state))]
          ^{:key key} [:div {:style {:display "flex"}}
                       [:button {:style {:margin-right 5}
                                 :on-click #(dispatch! {:type :fork-process
                                                        :program key})
                                 :disabled (= process-slots-available 0)} "<"]
                       [program-info program]])]]]
     [:div {:style {:font-family "monospace"
                    :border "2px darkgray dashed"
                    :padding "10px"
                    :margin-top "20px"}}
      [:h3 {:style {:margin-bottom 8}} "Developer Tools"]
      [:div {:style {:display "flex"}}
        [action-button {:on-click #(swap! game-state assoc-in [:programs] (into #{} (keys program-catalog)))} "Grant All Programs"]
        [action-button {:on-click #(when (js/confirm "Are you sure you want to reset your game-state to default-state? This cannot be undone.") (reset! game-state (default-state)))} "Reset Default State"]]
      [:h4 {:style {:margin-bottom 4}} "Game State:"]
      [:pre {:style {:background-color "lightgray"
                     :margin-top 0
                     :padding 10}}
       (with-out-str (pp/pprint @game-state))]]]))

(defn tick! [state dt]
  (dispatch!
   (map
    (fn [[pid process]]
      [(:on-complete (process->program process))
       {:type :complete-process
        :pid pid}])
    (filter
     (comp process/complete? second)
     (get-in state [:processes])))))

(defn handle-animation-frame [timestamp-chan time]
  (.requestAnimationFrame js/window (partial handle-animation-frame timestamp-chan))
  (async/offer! timestamp-chan time))

(defn init [time]
  (let [dt-chan (chan (async/sliding-buffer 1))
        timestamp-chan (chan (async/sliding-buffer 1))]
    (set! dt-mult (async/mult dt-chan))
    (let [event-tap (async/tap event-mult (chan 10))]
      (go-loop [event (<! event-tap)]
        (swap! game-state reducers/reduce-state event)
        (recur (<! event-tap))))
    (go-loop [prev-time (<! timestamp-chan)
              curr-time (<! timestamp-chan)]
      (let [dt (ms->sec (- curr-time prev-time))]
        (>! dt-chan dt)
        (tick! @game-state dt))
      (recur curr-time (<! timestamp-chan)))
    (.requestAnimationFrame js/window (partial handle-animation-frame timestamp-chan))
    (r/render [app] (.getElementById js/document "app"))))

(defonce start (.requestAnimationFrame js/window init))

(defn on-js-reload []
  (swap! game-state update-in [:__figwheel_counter] inc)
)
