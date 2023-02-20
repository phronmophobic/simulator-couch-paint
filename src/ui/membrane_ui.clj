(ns ui.membrane-ui
  (:require
    [genek.entities :as e]
    [genek.sim2 :as sim]
    [membrane.basic-components :as basic]
    [membrane.component :refer [defui defeffect make-app]]
    [membrane.ui :as ui]
    [membrane.skia :as skia]
    [membrane.skia.paragraph :as para]
    [taoensso.timbre :as log]
    [ui.gk-membrane-helpers :as mh]))


; someday
; - learn about focus

; next
; X  I’ll have ability to nav through all the frames
;    - prev/next
;    - last frame
;    - handle inc/dec :last-frame
; X and a way to animate all the frames.
; X show all stages [:start :waiting-for-movers1 ...]
;   X highlight which stage we're in (RED)
; - create pane on the left, with buttons: "start FIFO" "start LIFO"
; - slider
; someday
; - left pane
; X take side effects out of event handler
; - 3:20pm: OMG, got it working!
; - 3:55pm: got into trouble
; - 4:05pm: restarted repl, now working again!

; Adrian, to run:
; search for "Adrian" for the 3 forms to run

(def *sim-state genek.sim2/*state)

; :frame: either frame number or :last-frame
(defonce *app-state (atom nil))

(defn init-state!
  [{:keys [sim] :as opts}]
  ;(reset! *app-state {:frame :last-frame})
  (log/warn :init-state! :running-simulator)

  (sim/init-state!)
  (log/warn :init-state! :sim sim :opts opts)
  (sim/simulate-until-done (-> @sim/*state last)
    (merge {:maxturns 500} sim))

  (log/warn :init-state! :updating-atom)
  (swap! *app-state
     assoc
     :frame 0
     :sim-state @*sim-state
     :*sim-state *sim-state)

  nil)


(comment
  (init-state! {})
  0)

(defn next-frame!
  [curr-page total-pages]
  ; cases:
  ;  handle overflow:  more than total-pages
  ;  handle :last-frame: leave unchanged
  (cond
    (= :last-frame curr-page) nil
    (= (dec total-pages) curr-page) nil
    :else (swap! *app-state update-in [:frame] inc)))

(defn prev-frame!
  [curr-page total-pages]
  ; cases:
  ;  handle underflow: if 0, leave at zero
  ;  handle :last-frame: (dec total-pages)
  (log/warn :prev-frame! :curr-page curr-page :total-pages total-pages)
  (case curr-page
    0 nil
    :last-frame (swap! *app-state assoc-in [:frame] (dec total-pages))
    (swap! *app-state update-in [:frame] dec)))

(defn last-frame!
  []
  (swap! *app-state assoc-in [:frame] :last-frame))

(defn first-frame!
  []
  (swap! *app-state assoc-in [:frame] 0))

(defn animate-all-frames!
  " start from zero, and then on a timer, advance frames to the end
    run in a future; pass first frame, recurse to end (total-pages)

    input: framenum"
  [framenum total-pages]
  (first-frame!)
  (loop [framenum framenum
         total-page total-pages]
    ; termination case
    (log/debug :animate-all-frames!/entering :framenum framenum :total-pages total-pages)
    (if (< framenum total-pages)
      (do
        (Thread/sleep 40)
        (next-frame! framenum total-pages)
        ; see if we can force repaint
        (if-let [w (resolve `w2)]
          ((:membrane.skia/repaint @w)))
        (recur (inc framenum) total-pages)))))

(defeffect ::prev-frame
  [curr-page total-pages]
  (prev-frame! curr-page total-pages))

(defn selector
  [curr-page total-pages]
  (ui/on
    ::next-frame (fn []
                   (log/warn ::next-frame)
                   (next-frame! curr-page total-pages)
                   nil)
    ::prev-frame (fn []
                   (log/warn ::prev-frame)
                   [[::prev-frame curr-page total-pages]])
    ::last-frame (fn []
                   (log/warn ::last-frame)
                   (last-frame!)
                   nil)
    ::first-frame (fn []
                    (log/warn ::first-frame)
                    (first-frame!)
                    nil)
    ::animate-all-frames (fn []
                           (log/warn ::animate-all-frames)
                           ; run in future
                           (future
                             (animate-all-frames! 0 total-pages))
                           nil)
    :key-press (fn [k]
                 (log/warn :selector :key-press k :type (type k))
                 (case k
                   "j" [[::next-frame]]
                   "k" [[::prev-frame]]
                   "$" [[::last-frame]]
                   ;["^" "0"] [[::first-frame]]
                   "^" [[::first-frame]]
                   "0" [[::first-frame]]
                   "A" [[::animate-all-frames]]
                   "a" [[::animate-all-frames]]
                   nil))

    (ui/horizontal-layout
      (ui/button "<<"
        (fn []
          [[::prev-frame]]))
      (ui/label (format "curr-page: %s, total-pages %s"
                  (str curr-page) (str total-pages)))
      (ui/button ">>"
        (fn []
          [[::next-frame]]))
      (ui/button "Animate!"
        (fn []
          [[::animate-all-frames]])))))


(defn turn
  [state]
  (ui/vertical-layout
    (ui/horizontal-layout
      (ui/label (format "Turn: %d"
                  (-> state :turn))))
    (ui/spacer 25)))

(def states-text {:initial              ""
                  :waiting-for-movers1  "wait"
                  :removing-furniture   "moving"
                  ;:removing-furniture   [{:text  "🛋 "
                  ;                        :style #:text-style {:font-size 9}
                  ;                       "moving"
                  :waiting-for-painters "wait"
                  :painting             "painting"
                  :waiting-for-movers2  "wait"
                  :restoring-furniture  "moving"
                  :finished             "done!"})

(comment
  (get states-text :initial)
  0)

(defn make-red [text]
  (cond
    (string? text)
    {:text text
     :style #:text-style {:color [1 0 0]}}

    (map? text)
    (assoc-in text [:style :text-style/color] [1 0 0])

    :else
    (map make-red text)))

(defn room-state
  " show all room states, and highlight the current one
    [:initial   :waiting-for-movers1   :removing-furniture   :waiting-for-painters   :painting   :waiting-for-movers2   :restoring-furniture   :finished])"
  [room]
  (ui/translate
   0 3
   (para/paragraph
    (interpose
     " "
      ; skip "start"
     (for [st (rest e/room-states)]
       (if (not= st (:state room))
         (get states-text st)
         (make-red (get states-text st))))))))

(defn time-remaining-bar
  " just time remaining as string of chars "
  [n]
  (str "" (apply str (repeat n "*"))))

(defn worker-str
  [w]
  (-> w :role str (subs 1)))

(defn workers-present
  " show any movers/painters in room"
  [r movers painters]
  (let [rid (:id r)
        workers (->> (concat movers painters)
                  (filter #(= rid (:at-room %)))
                  vec)]
    (apply
      ui/horizontal-layout
      (ui/spacer 20 0)
      (for [w workers]
        (para/paragraph
          [{:text  (case (:role w)
                     :painter "🖌"
                     :mover "🛋")
            :style #:text-style {:font-size 11}}
           {:text  (format "%s %d   " (worker-str w) (:id r))
            :style #:text-style {:font-size 14
                                 :color     (case (:role w)
                                              :painter mh/accent-blue
                                              :mover mh/accent-green)}}])))))


(def work-text-style
  #:text-style{:height 1
               :height-override true})

(defn room
  [r movers painters]
  (ui/vertical-layout
    ; keep all boxes the same width
    (ui/horizontal-layout
      (ui/spacer 405 0))
    (ui/horizontal-layout
      ; https://phronmophobic.github.io/membrane/styled-text/index.html
      (para/paragraph
        {:text (format "Room %d" (-> r :id))
         :style #:text-style {:font-size 14
                              :font-style #:font-style{:weight :bold}}})
      (ui/spacer 50 0)
      (workers-present r movers painters))

    (ui/spacer 5)
    (para/paragraph
     {:text (str "🛋 work remaining: "
                 (time-remaining-bar (-> r :moving1-time-remaining)))
      :style work-text-style})

    ; adrian, is there an easy way to tighen up the line spacing between these?
    (para/paragraph
     {:text (str "🖌 work remaining: "
                 (time-remaining-bar (-> r :painting-time-remaining)))
      :style work-text-style})

    ; adrian, is there an easy way to tighen up the line spacing between these?
    (para/paragraph
     {:text (str "🛋 work remaining: "
                 (time-remaining-bar (-> r :moving2-time-remaining)))
      :style work-text-style})

    (ui/spacer 10)
    (room-state r)
    (ui/spacer 10)))

(defn room-row
  [rooms movers painters]
  (apply
    ui/horizontal-layout
    (for [r rooms]
      (ui/bordered [0 0]
        (ui/padding
          4
          (room r movers painters))))))

(defn rooms
  " main view: will show all details of room, as well as any movers/painters present
    NOTE: 405 pixels is good "
  [state]
  (let [{:keys [rooms movers painters]} state
        roomrows (partition-all 2 rooms)]
    (apply
      ui/vertical-layout
      (for [row roomrows]
        (room-row row movers painters)))))

(defn rooms-OLD
  " main view: will show all details of room, as well as any movers/painters present
    NOTE: 405 pixels is good "
  [state]
  (let [{:keys [rooms movers painters]} state]
    (apply
      ui/vertical-layout
      (for [r rooms]
        (ui/bordered [0 0]
          (ui/padding
            4
            (room r movers painters)))))))

(comment
  (def ww (range 20))
  (partition-all 2 (range 21))
  0)

(defn movers
  [state]
  (apply
    ui/vertical-layout
    (interpose (ui/spacer 10)
      (for [r (-> state :movers)]
        (let [roomnum (-> r :at-room)]
          (ui/label (format "Mover %d -- In Room: %s"
                      (-> r :id)
                      (if roomnum
                        (str (-> r :at-room))
                        "---"))))))))

(defn painters
  [state]
  (apply
    ui/vertical-layout
    (interpose (ui/spacer 10)
      (for [r (-> state :painters)]
        (let [roomnum (-> r :at-room)]
          (ui/label (format "Painter %d -- In Room: %s"
                      (-> r :id)
                      (if roomnum
                        (str (-> r :at-room))
                        "---"))))))))

(defn get-frame
  " get frame, and prevent overflow "
  [n frames]
  ;(log/warn :get-frame :n n :maxn maxn)
  ;(log/warn :get-frame :n n :type (type frames))
  (let [maxn (count frames)]
    (if (>= n maxn)
      (last frames)
      (nth frames n))))

(comment
  (get-frame 100 @*sim-state)
  0)

(defn parse-framenum
  " handle :last-frame: return an int "
  [framenum sim-state]
  (case framenum
    :last-frame (count sim-state)
    framenum))


(defui slider
  [{:keys [frame sim-state]}]
  (ui/vertical-layout
    ;(ui/label "hello!")
    ;(ui/label (str frame))

    ; handle :last-frame
    (let [fnum (parse-framenum frame sim-state)]
      (basic/number-slider {:num fnum
                            :$num $frame
                            :min 0
                            :max (dec (count sim-state))
                            :integer? true}))))


(defui outer-pane
  [{:keys [view]}]
  (ui/vertical-layout
    (ui/spacer 20 20)
    (ui/horizontal-layout
      (ui/horizontal-layout
        (ui/spacer 20 20)
        (basic/button {:text     "Initialize (Painters FIFO)"
                       :on-click #(do
                                    (log/warn :outer-pane :click)
                                    (init-state! {:sim {:painter-fifo true}}))})
        (basic/button {:text     "Initialize (Painters LIFO)"
                       :on-click #(do
                                    (log/warn :outer-pane :click)
                                    (init-state! {:sim {:painter-fifo false}}))})))
    (ui/spacer 20 20)

    (ui/horizontal-layout
      ;[(ui/spacer 100)
      ; (ui/label "hello2")
      (ui/spacer 20)
      view)))

(comment
  (def w3 (skia/run (make-app #'outer-pane {})))
  0)


(defui render-view
  [{:keys [frame sim-state *sim-state]
    :as   m}]
  (let [
        state    (case frame
                   :last-frame (last sim-state)
                   (get-frame frame sim-state))]
    (outer-pane {:view
                 (ui/vertical-layout
                   ; curr-page total-pages
                   (ui/horizontal-layout
                     (slider {:frame        frame
                              :sim-state sim-state})
                     (ui/spacer 20)
                     (selector frame (count sim-state)))
                   ;(turn state)
                   (ui/spacer 20 20)
                   (rooms state)
                   (movers state)
                   (painters state))})))

(comment
  ; this allows getting away from global state, which we used for dev-view
  ;(def dev-app2 (make-app #'my-slider *app-state))
  ; Adrian: run these forms
  (init-state! {})
  (def dev-app2 (make-app #'render-view *app-state))
  (def w2 (skia/run dev-app2))
  0)

(comment
  (swap! *app-state
    assoc
    :frame 0
    :sim-state @*sim-state)

  (swap! *app-state
    update
    :sim-state
    #(subvec % 0 50))

  (first
    (for [x (range 1000)
          y (range 1000)
          :let [intents (seq (ui/mouse-down (dev-view)
                                [x y]))]
          :when intents]
      intents))

  (ui/mouse-down (basic/number-slider {:num 0
                                       :min 0
                                       :max 100
                                       :integer? true})
    [10 10])



  0)

(defn dev-view
  " helper: put anything you're working in here in dev
    (for prod app, it'll just be another view, composing all your components "
  []
  (let [states @*sim-state]
    ;(selector (-> @*app-state :frame) (count states))
    (render-view @*sim-state *app-state)))



(comment
  (skia/run #'dev-view)

  (init-state!)
  (def w (skia/run #'dev-view))
  (def w1 (skia/run #'dev-view))
  ((:membrane.skia/repaint w))
  @*app-state

  (dev-view)
  (skia/save-image "/tmp/devvew.png" (dev-view))

  0)


(comment
  genek.sim2/*state
  (skia/run (fn [] (ui/label "hi"))))