(ns ui.gk-membrane-ui
  (:require
    [genek.entities :as e]
    [genek.sim2 :as sim]
    [ui.menu :as menu]
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
; - new schedules: painter-random, mover-random
; - furniture inventory

; Adrian, to run:
; search for "Adrian" for the 3 forms to run
;   ^^^ this has been moved to ui.main (where I'm putting in REPL friendly convenience forms to run)

(def *sim-state genek.sim2/*state)

; :frame: either frame number or :last-frame
(defonce *app-state (atom nil))

(defn init-state!
  [{:keys [sim load-sim-state!] :as opts}]
  ;(reset! *app-state {:frame :last-frame})
  (log/warn :init-state! :running-simulator :opts opts)

  (if-not load-sim-state!
    ; run simulation
    (do
      (sim/init-state!)
      (log/warn :init-state! :sim sim :opts opts)
      (sim/simulate-until-done (-> @sim/*state last)
        (merge {:maxturns 500} sim))
      #_(sim/simulate-find-min (-> @sim/*state last)
          (merge {:maxturns 500} sim))

      (swap! *app-state
        assoc
        :frame 0
        :sim-state @*sim-state
        :*sim-state *sim-state))
    ; load sim
    (swap! *app-state
      assoc
      :frame 1
      :sim-state @*sim-state
      :*sim-state *sim-state))

  (log/warn :init-state! :updating-atom)

  nil)


(comment
  (init-state! {})
  (init-state! {:load-sim-state! true})
  (tap> @*app-state)
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
        (if-let [w (resolve `ui.main/w2)]
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
                   "l" [[::last-frame]]
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
      (ui/label (format "Current turn: (%s / %s)"
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
     :style #:text-style {:color mh/set1-red
                          :font-size 12
                          :height-override true
                          :height 0.90}}

    (map? text)
    (-> text
      (assoc-in [:style :text-style/color] mh/set1-red))

    :else
    (map make-red text)))

(defn make-small-text
  [t]
  (log/debug :make-small-text t)
  {:text            (str t)
   :style           #:text-style {;:color mh/accent-green
                                  :font-size 13
                                  :height-override true
                                  :height 0.90}})
   ;:height-override true
   ;:height          0.90})

(defn room-state-bar
  " show all room states, and highlight the current one
    [:initial   :waiting-for-movers1   :removing-furniture   :waiting-for-painters   :painting   :waiting-for-movers2   :restoring-furniture   :finished])"
  [room]
  (ui/translate
   0 0
   (para/paragraph
    (interpose
     " "
      ; skip "start"
     (for [st (rest e/room-states)]
       (if (not= st (:state room))
         (make-small-text (get states-text st))
         (make-red (get states-text st))))))))

(defn time-remaining-bar
  " just time remaining as string of chars "
  [n]
  (let [n (or n 0)]
    (str "" (apply str (repeat (/ n 2) "*")))))


(def ROOMWIDTH 260)
(def ROOMHEIGHT 7)
(def SPACERHEIGHT 10)

(defn furniture-bar
  [n]
  (let [n (or n 0)
        msg (str "" (apply str (repeat (/ n 4) "🛋")))]
    (para/paragraph
     {:text  msg
      :style #:text-style {:font-size 8}}
     (- (* ROOMWIDTH 2) 100))))

(defn worker-str
  [w]
  ; to "Movers" and "Painters"
  (str
    (-> w :role str (subs 1) clojure.string/capitalize)
    ; pluralize
    "s"))

(defn worker
  [w movers painters]
  (para/paragraph
    [{:text  (case (:role w)
               :painter "🖌"
               :mover   "🛋")
      :style #:text-style {:font-size 11
                           :color     (case (:role w)
                                        :painter mh/set1-purple
                                        :mover mh/set1-green)
                           :height-override true
                           :height 0.80}}
     {:text  (format "%s %d   " (worker-str w) (inc (:id w)))
      :style #:text-style {:font-size 13
                           :color     (case (:role w)
                                        :painter mh/set1-purple
                                        :mover mh/set1-green)
                           :height-override true
                           :height 0.90}}]))

(defn room-workers-status-bar
  " show any movers/painters in room"
  [r movers painters]
  (let [rid (:id r)
        workers (->> (concat movers painters)
                  (filter #(= rid (:at-room %)))
                  vec)]
    (apply
      ui/horizontal-layout
      (for [w workers]
        (worker w movers painters)))))


; convenient emojis "                     :painter "👯🖌""



(defn room
  [r movers painters]
  (log/warn :room :state (-> r :state))
  (let [color (case (-> r :state)
                ;:removing-furniture mh/pastel2-cyan
                ;:restoring-furniture mh/pastel2-cyan
                ;:painting mh/pastel2-lavender
                [255 255 255])]
    (ui/fill-bordered color [0 0]
      (ui/padding 2
        (ui/vertical-layout
          ; keep all boxes the same width
          (ui/horizontal-layout
            (ui/spacer ROOMWIDTH ROOMHEIGHT))
          (ui/horizontal-layout
            ; https://phronmophobic.github.io/membrane/styled-text/index.html
            (para/paragraph
              {:text  (format "Room %d" (inc (-> r :id)))
               :style #:text-style {:font-size  13
                                    :height-override true
                                    :height 0.90
                                    :font-style #:font-style{:weight :bold}}})
            (ui/spacer 30 0)
            (room-workers-status-bar r movers painters))

          ;(ui/spacer 5)

          (ui/spacer 5)
          (room-state-bar r)


          (ui/horizontal-layout
            (para/paragraph
              {:text  (str "🛋 work remaining: "
                        (-> r :moving1-time-remaining)
                        (time-remaining-bar (-> r :moving1-time-remaining)))
               :style #:text-style {:font-size 13}}))

          (ui/horizontal-layout
            (para/paragraph
              {:text  (str "🖌 work remaining: "
                        (-> r :painting-time-remaining)
                        (time-remaining-bar (-> r :painting-time-remaining)))
               :style #:text-style {:font-size       13
                                    :height-override true
                                    :height          0.90}}))

          (ui/horizontal-layout
            (para/paragraph
              {:text (str "🛋 work remaining: "
                       (-> r :moving2-time-remaining)
                       (time-remaining-bar (-> r :moving2-time-remaining)))
               :style #:text-style {:font-size 13
                                    :height-override true
                                    :height 0.90}}))


          (ui/spacer 5))))))

(defn room-row
  [rooms movers painters]
  (apply
    ui/horizontal-layout
    (for [r rooms]
      (ui/bordered [0 0]
        (room r movers painters)))))

(defn rooms
  " main view: will show all details of room, as well as any movers/painters present
    NOTE: 405 pixels is good "
  [state]
  (let [{:keys [rooms movers painters]} state
        roomrows (partition-all 3 rooms)]
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

(defn worker-in-room
  [w movers painters]
  (let [inroom (:at-room w)
        inroomstr (if inroom
                    ;(format "⌂ room %d" inroom)
                    (format "(Room %d)" inroom)
                    ;(format "- room %d" inroom)
                    (str ""))]
    (log/debug :worker-in-room :roomstr inroomstr :w w)
    (ui/bordered [2 2]
      (ui/vertical-layout
        (ui/spacer 150 0)
        (para/paragraph
          [{:text  (case (:role w)
                     :painter "🖌"
                     :mover   "🛋")
            :style #:text-style {:font-size 11}}
           {:text  (format "%s %d %s" (worker-str w) (:id w) inroomstr)
            :style #:text-style {:font-size 13
                                 :color     (case (:role w)
                                              :painter mh/set1-purple
                                              :mover mh/set1-green)}}])))))




(defn get-nth-state
  " get frame, and prevent overflow "
  [n frames]
  ;(log/warn :get-frame :n n :type (type frames))
  (let [maxn (count frames)
        n    (if (= n :last-frame)
               (dec (count frames))
               n)]
    (log/warn :get-frame :n n :maxn maxn)
    (if (> n maxn)
      (last frames)
      (nth frames n))))

(defui workers-status-row
  [{:keys [frame sim-state]}]
  (let [state (get-nth-state frame sim-state)
        {:keys [movers painters]} state
        workers (concat movers painters)]
    (log/debug :workers-status-row :workers (vec workers))
    (log/debug :workers-status-row :state state)
    (ui/vertical-layout
      (ui/spacer 0 10)
      (ui/bordered [0 0]
        (apply
          ui/horizontal-layout
          (for [w workers]
            (let [wpara (worker-in-room w movers painters)
                  ; to figure out how wide the box should be
                  _     (log/debug (ui/bounds wpara))]
              wpara)))))))

(comment
  (get-nth-state 100 @*sim-state)
  0)

(defn parse-framenum
  " handle :last-frame: return an int "
  [framenum sim-state]
  (case framenum
    :last-frame (count sim-state)
    framenum))


(defui slider
  [{:keys [frame sim-state]}]
  (ui/horizontal-layout
    (ui/horizontal-layout
      (ui/label "Turn #: ")
      (ui/spacer 5 0))
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
   [
    (ui/translate
     0 (+ menu/menu-height 20)
     (ui/horizontal-layout
      ;; [(ui/spacer 100)
      ;; (ui/label "hello2")
      (ui/spacer 20)
      view))

    ;; put menus last so they are drawn
    ;; on top of everything else.
    (ui/translate
     20 0
     (menu/menus
      {:$menus [::top-menu]
       :menus
       [{:text "Init"
         :items [{:text     "Load sim state"
                  :on-click #(do
                               (log/warn :outer-pane :click)
                               (init-state! {:load-sim-state! true}))}
                 {:text     "Initialize (Painters FIFO)"
                  :on-click #(do
                               (log/warn :outer-pane :click)
                               (init-state! {:sim {:painter-schedule :fifo}}))}
                 {:text     "Initialize (Painters LIFO)"
                  :on-click #(do
                               (log/warn :outer-pane :click)
                               (init-state! {:sim {:painter-schedule :lifo}}))}
                 {:text     "Initialize (Painters random)"
                  :on-click #(do
                               (log/warn :outer-pane :click)
                               (init-state! {:sim {:painter-schedule :random}}))}]}
        {:text "Second menu"
         :items [{:text "View All"
                  :on-click
                  (fn []
                    (prn "viewall")
                    nil)}
                 {:text "new"
                  :on-click
                  (fn []
                    (prn "new")
                    nil)}]}]}))]))

(comment
  (def w3 (skia/run (make-app #'outer-pane {})))
  0)

(defn furniture-text
  [nf maxnf]
  (ui/vertical-layout
    (ui/spacer 95 0)
    (para/paragraph
      {:text  (format "Furniture in storage:\n%-11s %-3d\n%-11s %-3d"
                "Current: "
                (or nf 0)
                "Peak: "
                (or maxnf 0))
       :style #:text-style {:font-size 13}})))

(defui furniture-stats
  " show furniture inventory"
  [{:keys [frame sim-state]}]
  (let [state (get-nth-state frame sim-state)
        nf (get-in state [:furniture :in-storage])
        maxnf (get-in state [:furniture :max-in-storage])]
    ;(ui/label (format "Furniture: %d" (-> state :furniture-stored)))
    (ui/vertical-layout
      (ui/spacer SPACERHEIGHT)
      (ui/bordered [2 2]
        (ui/vertical-layout
          (ui/spacer (* ROOMWIDTH 3) 0)
          (ui/horizontal-layout
            (furniture-text nf maxnf)
            ;(ui/label (format "Furniture in storage (%3d, max %3d): " nf maxnf))
            (furniture-bar nf)))))))



(defui render-view
  [{:keys [frame sim-state *sim-state]
    :as   m}]
  (let [
        state    (case frame
                   :last-frame (last sim-state)
                   (get-nth-state frame sim-state))]
    (outer-pane {:view
                 (ui/vertical-layout
                   (ui/horizontal-layout
                     (slider {:frame        frame
                              :sim-state sim-state})
                     (ui/spacer 20)
                     (selector frame (count sim-state)))
                   ;(turn state)
                   (ui/spacer 20 20)
                   (workers-status-row {:frame        frame
                                        :sim-state sim-state})
                   (furniture-stats {:frame frame
                                     :sim-state   sim-state})
                   (ui/spacer 20 20)
                   (rooms state))})))

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

(defn show-leaf-counter
  []
  (let [counter (-> @sim/*leaf-counter)]
    (ui/label (str counter))))

(defn dev-view
  " helper: put anything you're working in here in dev
    (for prod app, it'll just be another view, composing all your components "
  []
  (let [states @*sim-state]
    ;(selector (-> @*app-state :frame) (count states))
    ;(render-view @*sim-state *app-state)
    (show-leaf-counter)))

(comment
  (skia/run #'dev-view)
  ; adrian, wanna leave meeting and come back in?
  0)



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