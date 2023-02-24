(ns ui.menu
  (:require
    [membrane.basic-components :as basic]
    [membrane.component :refer [defui defeffect make-app]]
    [membrane.ui :as ui]
    [membrane.skia :as skia]))


(defui dropdown-list
  [{:keys [items]}]
  (let [labels (for [{:keys [text]} items]
                 (ui/label text))
        max-width (reduce max 0 (map ui/width labels))
        padding-y 8
        padding-x 12

        rows
        (apply
         ui/vertical-layout
         (for [{:keys [text on-click] :as item} items]
           (let [hover? (get extra [:hover? $item])
                 label (ui/label text)
                 
                 [_ h] (ui/bounds label)
                 row-height (+ h 4)
                 row-width (+ max-width (* 2 padding-x))]
             (basic/on-hover
              {:hover? hover?
               :body
               (ui/on
                :mouse-down
                (fn [_]
                  (when on-click
                    (on-click)))

                [(ui/spacer row-width row-height)
                 (cond
                   hover?
                   (ui/filled-rectangle [0.976 0.976 0.976]
                                        row-width row-height))
                 (ui/translate padding-x 2
                               label)])}))))
        [rows-width rows-height] (ui/bounds rows)]
    [(ui/with-style
       ::ui/style-stroke
       (ui/with-color [0.831
                       0.831
                       0.831]
         (ui/rounded-rectangle rows-width
                               (+ rows-height (* 2 padding-y))
                               4)))
     (ui/with-style
       ::ui/style-fill
       (ui/with-color [1 1 1]
         (ui/rounded-rectangle rows-width
                               (+ rows-height (* 2 padding-y))
                               4)))
     (ui/translate 0 (- padding-y 2)
                   rows)])
  )

(defn menu-button [text]
  (ui/bordered [10 10]
               (ui/label text)))

(def menu-height
  (ui/height
   (menu-button "HELLO")))

(defeffect ::do-nothing []
  ;; nada
  )

(defui menus [{:keys [menus
                      open-menu
                      ^:membrane.component/contextual
                      focus]}]
  (let [menu-buttons
        (for [[i {:keys [options text] :as menu}] (map-indexed vector menus)]
          (ui/on
           :mouse-down
           (fn [_]
             (prn "hi"  (= focus $menus))
             (if (= focus $menus)
               [[:set $focus nil]]
               [[:set $focus $menus]
                [:set $open-menu i]]))
           (menu-button text)))]
    
    (ui/vertical-layout
     (apply ui/horizontal-layout menu-buttons)
     [(when (= focus $menus)
        (when-let [menu (nth menus open-menu nil)]
          (let [xpos
                (transduce
                 (map ui/width)
                 +
                 0
                 (take open-menu menu-buttons))]
            (ui/translate
             xpos 0
             (ui/wrap-on
              ;; work around click handlers with side effects
              :mouse-down
              (fn [handler mpos]
                (let [intents (handler mpos)]
                  (if (seq intents)
                    intents
                    [[::do-nothing]])))
              (ui/on
               :mouse-up
               (fn [_]
                 [[:set $focus nil]])
               (dropdown-list menu)))))))])))

(comment

  (skia/run (make-app #'menus
                      
                      {:menus
                       [{:text "Account"
                         :items [{:text "View All"
                                  :on-click
                                  (fn []
                                    (prn "viewall")
                                    nil)}
                                 {:text "new"
                                  :on-click
                                  (fn []
                                    (prn "new")
                                    nil)}]}
                        {:text "Inventory"
                         :items [{:text "View All"
                                  :on-click
                                  (fn []
                                    (prn "viewall")
                                    nil)}
                                 {:text "new"
                                  :on-click
                                  (fn []
                                    (prn "new")
                                    nil)}]}]}))
  ,)
