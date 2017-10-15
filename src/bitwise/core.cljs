(ns bitwise.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as r]
            [cljs.core.async :as async :refer [<! >! chan]]
            [bitwise.util :as util]
            [clojure.pprint :as pp]))

(enable-console-print!)

(defn default-state [] {:nextpid 1000
                        :programs #{:work}
                        :resources {:data 0}
                        :processes [{:pid 22381
                                     :program :work
                                     :progress (chan 10)}]})

(defonce game-state (r/atom (default-state)))
(defonce last-update (r/atom nil))
(defonce dt-mult nil)

(def program-catalog {:work {:name "work"
                             :duration 1.5
                             :on-complete (fn [state] (update-in state [:resources :data] inc 1))}})

(defn process->program [process]
  ((:program process) program-catalog))

(defn process-runner [process]
  (let [program (process->program process)
        duration (:duration program)
        process-chan (:progress process)
        start-chan (chan)]
    (go
      (while (<! start-chan)
        (let [t (async/timeout (* duration 1000))
              dt-chan (async/tap dt-mult (chan))]
          (loop [elapsed 0
                 dt 0]
            (if (>= elapsed duration)
              (do
                (>! process-chan 1.0)
                (swap! game-state (:on-complete program) process)
                (async/close! dt-chan))
              (do
                (>! process-chan (/ elapsed duration))
                (let [[next-dt c] (async/alts! [dt-chan t] {:priority true})]
                  (if (= t c)
                    (recur duration next-dt)
                    (recur (+ elapsed dt) next-dt)))))))))
    start-chan))

(defn process-info [process]
  (let [program (process->program process)
        progress (r/atom 0)
        click-chan (process-runner process)]
    (go
      (while true (let [prog (<! (:progress process))]
                    (reset! progress prog))))
    (fn []
      (let [p @progress
            pct (* p 100)]
        [:tr
         [:td (:pid process)]
         [:td (:name program)]
         [:td
          [:a {:on-click #(do (.preventDefault %) (async/offer! click-chan :click))
               :role "button"
               :href "#"
               :style {:width 100
                       :height 25
                       :border "1px solid gray"
                       :display "flex"
                       :align-items "center"
                       :justify-content "center"
                       :background (str "linear-gradient(to right, lightgray " pct "%, white " pct "%)")
                       :user-select "none"
                       :text-decoration "none"
                       :color "black"}} "Execute"]]
         [:td
          [:a {}]]]))))

(defn app []
  [:div
   [:div {:style {:display "flex"
                  :font-family "monospace"}}
    [:div {:style {:flex-grow "2"}}
     [:div
      (str "data: " (util/display-as-binary (get-in @game-state [:resources :data])))]
     [:table
      [:tbody
       [:tr
        [:td>b "PID"]
        [:td {:style {:width 200}}
         [:b "PROGRAM"]]
        [:td]
        [:td]]
       (for [process (:processes @game-state)]
         ^{:key (:pid process)} [process-info process])]]]
    [:div {:style {:flex-grow "1"}}
     [:h3 "programs"]
     [:ul
      (for [program (:programs @game-state)]
        ^{:key program} [:li program])]]]
   [:div
    [:h3 {:style {:margin-bottom 8}} "debug"]
    [:pre {:style {:background-color "lightgray"
                   :margin-top 0
                   :padding 10}}
     (with-out-str (pp/pprint @game-state))]]])

(defn tick [state dt]
  (when (> dt 0.0175) (.log js/console "high dt" dt))
  state)

(defn handle-animation-frame [time]
  (.requestAnimationFrame js/window handle-animation-frame)
  (reset! last-update time))

(defn init []
  (let [dt-chan (chan (async/sliding-buffer 1))]
    (set! dt-mult (async/mult dt-chan))
    (add-watch last-update :dt #(when (not (nil? %3))
                                  (let [dt (/ (- %4 %3) 1000)]
                                    (swap! game-state #'tick (/ (- %4 %3) 1000))
                                    (go (>! dt-chan dt))))))
  (.requestAnimationFrame js/window handle-animation-frame)
  (r/render [app] (. js/document (getElementById "app"))))

(defonce start (init))

(defn on-js-reload []
  (swap! game-state update-in [:__figwheel_counter] inc)
)
