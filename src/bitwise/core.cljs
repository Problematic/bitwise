(ns bitwise.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as r]
            [cljs.core.async :as async :refer [<! >! chan]]
            [bitwise.util :as util]
            [clojure.pprint :as pp]
            [bitwise.reducers :as reducers]))

(enable-console-print!)

(defn ms->sec [ms]
  (/ ms 1000))

(defn sec->ms [sec]
  (* sec 1000))

(defn default-state [] {:nextpid 1000
                        :programs #{:work}
                        :architecture {:cpu {:cores 1
                                             :speed 0.5}
                                       :memory 512}
                        :resources {:data {:total 0
                                           :current 0}}
                        :processes []})

(defonce game-state (r/atom (default-state)))
(defonce dt-mult nil)
(defonce event-chan (chan))
(defonce event-pub (async/pub event-chan :type))

(def program-catalog {:work {:name "work"
                             :complexity 1.5
                             :memory 128
                             :on-complete (fn [state] (update-in state [:resources :data] (comp
                                                                                           (partial into {})
                                                                                           (partial map (fn [[key val]]
                                                                                                          [key (inc val)])))))}})

(defn process->program [process]
  ((:program process) program-catalog))

(defn dispatch! [action]
  (cond
    (fn? action) (dispatch! (action))
    (sequential? action) (doall (map dispatch! action))
    :else (do
            (swap! game-state reducers/reduce-state action)
            (async/offer! event-chan action))))

(defn process-runner [architecture process]
  (let [program (process->program process)
        duration (/ (:complexity program) (get-in architecture [:cpu :speed]))
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
                                                    (async/close! dt-chan)
                                                    (async/close! runner-chan))
              (>= elapsed duration) (do
                                      (>! process-chan 1.0)
                                      (dispatch! {:type :complete-process
                                                  :program program})
                                      (async/close! dt-chan))
              :else (do
                      (>! process-chan (/ elapsed duration))
                      (let [[next-dt c] (async/alts! [dt-chan t])]
                        (if (= t c)
                          (recur duration next-dt)
                          (recur (+ elapsed dt) next-dt)))))))
        (recur (<! runner-chan))))
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

(defn process-info [architecture process]
  (let [program (process->program process)
        progress (r/atom 0)
        runner-chan (process-runner architecture process)]
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
          [:a {:on-click #(do (.preventDefault %)
                              (dispatch! {:type :kill-process
                                          :pid (:pid process)})
                              (go (>! runner-chan :kill)))
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

(defn process-list [architecture processes]
  (let [arch @architecture]
    [:div
     [:div {:style (merge
                    process-grid-styles
                    {})}
      [:div>b "PID"]
      [:div>b "PROGRAM"]
      [:div]]
     (for [process @processes]
       ^{:key (:pid process)} [process-info arch process])]))

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
        [process-list architecture processes]
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
