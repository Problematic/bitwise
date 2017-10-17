(ns bitwise.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as r]
            [cljs.core.async :as async :refer [<! >! chan]]
            [bitwise.util :as util]
            [clojure.pprint :as pp]))

(enable-console-print!)

(defn ms->sec [ms]
  (/ ms 1000))

(defn sec->ms [sec]
  (* sec 1000))

(defn default-state [] {:nextpid 1000
                        :programs #{:work}
                        :cpu {:cores 1
                              :speed 1.0}
                        :memory 512
                        :resources {:data 0}
                        :processes []})

(defonce game-state (r/atom (default-state)))
(defonce dt-mult nil)

(def program-catalog {:work {:name "work"
                             :complexity 1.5
                             :memory 128
                             :on-complete (fn [state] (update-in state [:resources :data] inc))}})

(defn process->program [process]
  ((:program process) program-catalog))

(defn fork-process [state program]
  (-> state
      (update-in [:processes] conj {:pid (:nextpid state)
                                    :program program
                                    :progress (chan (async/sliding-buffer 1))})
      (update-in [:nextpid] inc)))

(defn kill-process [state pid]
  (update-in state [:processes] #(filterv (comp not (partial = pid) :pid) %)))

(defn process-runner [process]
  (let [program (process->program process)
        duration (:complexity program)
        process-chan (:progress process)
        runner-chan (chan)]
    (go-loop [cmd (<! runner-chan)]
      (when (= :execute cmd)
        (let [t (async/timeout (sec->ms duration))
              dt-chan (async/tap dt-mult (chan))]
          (loop [elapsed 0
                 dt 0]
            (cond
              (= :kill (async/poll! runner-chan)) (do
                                                    (>! process-chan 0.0)
                                                    (async/close! dt-chan))
              (>= elapsed duration) (do
                                      (>! process-chan 1.0)
                                      (swap! game-state (:on-complete program) process)
                                      (async/close! dt-chan))
              :else (do
                      (>! process-chan (/ elapsed duration))
                      (let [[next-dt c] (async/alts! [dt-chan t])]
                        (if (= t c)
                          (recur duration next-dt)
                          (recur (+ elapsed dt) next-dt))))))))
      (recur (<! runner-chan)))
    runner-chan))

(def process-grid-styles {:display "grid"
                          :grid-template-columns "75px 1fr 1fr"
                          :grid-template-rows "30px"
                          :align-items "center"})

(def action-button-styles {:width 100
                           :height 25
                           :border "1px solid gray"
                           :display "flex"
                           :align-items "center"
                           :justify-content "center"
                           :user-select "none"
                           :text-decoration "none"
                           :color "black"})

(defn process-info [process]
  (let [program (process->program process)
        progress (r/atom 0)
        runner-chan (process-runner process)]
    (go-loop [prog 0]
      (reset! progress prog)
      (recur (<! (:progress process))))
    (fn []
      (let [p @progress
            pct (* p 100)]
        [:div {:style (merge
                       process-grid-styles
                       {})}
         [:div (:pid process)]
         [:div (:name program)]
         [:div {:style {:display "flex"}}
          [:a {:on-click #(do (.preventDefault %) (async/offer! runner-chan :execute))
               :role "button"
               :href "#"
               :style (merge
                       action-button-styles
                       {:background (str "linear-gradient(to right, lightgray " pct "%, white " pct "%)")})} "Execute"]
          [:a {:on-click #(do (.preventDefault %) (swap! game-state kill-process (:pid process)) (go (>! runner-chan :kill)))
               :role "button"
               :href "#"
               :style (merge
                       action-button-styles
                       {:margin-left 10})} "Kill"]]]))))

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
   (for [process @processes]
     ^{:key (:pid process)} [process-info process])])

(defn app []
  (let [processes (r/cursor game-state [:processes])
        process-slots-available (- (get-in @game-state [:cpu :cores]) (count @processes))]
    [:div
     [:div {:style {:display "flex"
                    :font-family "monospace"}}
      [:div {:style {:flex-grow "2"}}
       [:div
        [:div
         (str "data: " (util/display-as-binary (get-in @game-state [:resources :data])))]
        [process-list processes]
        (for [idx (range process-slots-available)]
          ^{:key idx} [process-slot])]]
      [:div {:style {:flex-grow "1"}}
       [:h3 "programs"]
       [:div
        (for [[key program] (map #(vector % (% program-catalog)) (:programs @game-state))]
          ^{:key key} [:div
                       (when (> process-slots-available 0) [:button {:on-click #(swap! game-state fork-process key)} "<"])
                       (:name program)])]]]
     [:div
      [:h3 {:style {:margin-bottom 8}} "debug"]
      [:pre {:style {:background-color "lightgray"
                     :margin-top 0
                     :padding 10}}
       (with-out-str (pp/pprint @game-state))]]]))

(defn tick [state dt]
  state)

(defn handle-animation-frame [timestamp-chan time]
  (.requestAnimationFrame js/window (partial handle-animation-frame timestamp-chan))
  (async/offer! timestamp-chan time))

(defn init [time]
  (let [dt-chan (chan (async/sliding-buffer 1))
        timestamp-chan (chan (async/sliding-buffer 1))]
    (set! dt-mult (async/mult dt-chan))
    (go-loop [prev-time (<! timestamp-chan)
              curr-time (<! timestamp-chan)]
      (let [dt (ms->sec (- curr-time prev-time))]
        (>! dt-chan dt)
        (swap! game-state tick dt))
      (recur curr-time (<! timestamp-chan)))
    (.requestAnimationFrame js/window (partial handle-animation-frame timestamp-chan))))

(r/render [app] (. js/document (getElementById "app")))

(defonce start (.requestAnimationFrame js/window init))

(defn on-js-reload []
  (swap! game-state update-in [:__figwheel_counter] inc)
)
