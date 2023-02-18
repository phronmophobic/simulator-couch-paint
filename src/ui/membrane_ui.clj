(ns ui.membrane-ui
  (:require
    [membrane.ui :as ui]
    [membrane.skia :as skia]
    [membrane.basic-components :as basic]
    [membrane.component :refer
         [defui defeffect make-app]]))

; someday
; - learn about focus

; next
; X  I’ll have ability to nav through all the frames
;    - prev/next
;    - last frame
;    - handle inc/dec :last-frame
; - and a way to animate all the frames.

(def *sim-state genek.sim2/*state)

; :frame: either frame number or :last-frame
(defonce *app-state (atom nil))

(defn init-state!
  []
  (reset! *app-state {:frame :last-frame}))

(comment
  (init-state!)
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
  (println :prev-frame! :curr-page curr-page :total-pages total-pages)
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
    (println :animate-all-frames!/entering :framenum framenum :total-pages total-pages)
    (if (< framenum total-pages)
      (do
        (Thread/sleep 50)
        (next-frame! framenum total-pages)
        ; see if we can force repaint
        (if-let [w (resolve 'w)]
          ((:membrane.skia/repaint w)))
        (recur (inc framenum) total-pages)))))





(defn selector
  [curr-page total-pages]
  (ui/on
    ::next-frame (fn []
                   (println ::next-frame)
                   (next-frame! curr-page total-pages)
                   nil)
    ::prev-frame (fn []
                   (println ::prev-frame)
                   (prev-frame! curr-page total-pages)
                   nil)
    ::last-frame (fn []
                   (println ::last-frame)
                   (last-frame!)
                   nil)
    ::first-frame (fn []
                    (println ::first-frame)
                    (first-frame!)
                    nil)
    ::animate-all-frames (fn []
                           (println ::animate-all-frames)
                           ; run in future
                           (future
                             (animate-all-frames! 0 total-pages))
                           nil)
    :key-press (fn [k]
                 (println :selector :key-press k :type (type k))
                 (case k
                   "j" [[::next-frame]]
                   "k" [[::prev-frame]]
                   "$" [[::last-frame]]
                   ;["^" "0"] [[::first-frame]]
                   "^" [[::first-frame]]
                   "0" [[::first-frame]]
                   "A" [[::animate-all-frames]]
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


(defn rooms
  [states]
  (apply
    ui/vertical-layout
    (for [r (-> states last :rooms)]
      (ui/vertical-layout
        (ui/label (format "Room %d: %s"
                    (-> r :id)
                    (-> r :state)))
        (ui/label (format "    :moving1-time-remaining: %d"
                    (-> r :moving1-time-remaining)))
        (ui/label (format "    :painting-time-remaining: %d"
                    (-> r :painting-time-remaining)))
        (ui/label (format "    :moving2-time-remaining: %d"
                    (-> r :moving2-time-remaining)))
        (ui/spacer 25)))))

(defn movers
  [states]
  (apply
    ui/vertical-layout
    (interpose (ui/spacer 10)
      (for [r (-> states last :movers)]
        (let [roomnum (-> r :at-room)]
          (ui/label (format "Mover %d -- In Room: %s"
                      (-> r :id)
                      (if roomnum
                        (str (-> r :at-room))
                        "---"))))))))

(defn painters
  [states]
  (apply
    ui/vertical-layout
    (interpose (ui/spacer 10)
      (for [r (-> states last :painters)]
        (let [roomnum (-> r :at-room)]
          (ui/label (format "Painter %d -- In Room: %s"
                      (-> r :id)
                      (if roomnum
                        (str (-> r :at-room))
                        "---"))))))))

(defn get-frame
  " get frame, and prevent overflow "
  [n frames]
  (let [maxn (count frames)]
    ;(println :get-frame :n n :maxn maxn)
    (if (>= n maxn)
      (last frames)
      (nth frames n))))

(comment
  (get-frame 100 @*sim-state)
  0)

(defn render-view
  [sim-state *app-state]
  (let [framenum (-> @*app-state :frame)
        state    (case framenum
                   :last-frame (last sim-state)
                   (get-frame framenum sim-state))]
    (ui/vertical-layout
      ; curr-page total-pages
      (selector (-> @*app-state :frame) (count sim-state))
      (turn state)
      (rooms sim-state)
      (movers sim-state)
      (painters sim-state))))


(defn dev-view
  " helper: put anything you're working in here in dev
    (for prod app, it'll just be another view, composing all your components "
  []
  (let [states @*sim-state]
    ;(selector (-> @*app-state :frame) (count states))
    (render-view @*sim-state *app-state)))

(comment
  (skia/run #'dev-view)
  (def w (skia/run #'dev-view))
  ((:membrane.skia/repaint v))
  @*app-state

  0)

(comment
  genek.sim2/*state

  (skia/run (fn [] (ui/label "hi"))))