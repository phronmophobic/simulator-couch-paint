(ns genek.sim2
  (:require
    [clojure.spec.alpha :as s]
    [com.fulcrologic.guardrails.core :refer [>defn >defn- >def | ? =>]]
    [com.rpl.specter :as sp]
    [genek.entities :as e]
    [genek.utils :as utils]))



(>defn create-state
  ([rooms movers painters]
   [sequential? sequential? sequential? => map?]
   {:turn     0
    :rooms    rooms
    :movers   movers
    :painters painters})
  ([turn rooms movers painters]
   [map? sequential? sequential? sequential? => map?]
   {:turn     (inc (:turn turn))
    :rooms    rooms
    :movers   movers
    :painters painters}))



(defonce *state (atom [(create-state (e/create-rooms 4) (e/create-movers 2) (e/create-painters 4))]))

(comment
  @*state
  (def *state (atom [(create-state (e/create-rooms 4) (e/create-movers 2) (e/create-painters 4))]))
  0)

(defn atom?
  [a]
  (instance? clojure.lang.Atom a))

(comment
  (atom? (atom nil))
  (atom? nil)
  0)

(>defn rooms-being-moved
  " given state, return vector of all rooms being moved "
  [state] [map? => sequential?]
  (-> state
    :movers
    ((fn [movers]
       (map :at-room movers)))))


(>defn advance-state
  " IMPORTANT: take care of things like
    - decrementing working counters (e.g., :moving1-time-remaining) of all rooms with movers/painters assigned
    - unassigning movers and painters "
  [state] [map? => map?]
  (let [rooms-moving (rooms-being-moved state)]
    (println :advance-state/entering :rooms-moving rooms-moving)
    (println :advance-state/entering :state state)
    (reduce (fn [s rs]
              (println :advance-state :reduce/entering :state :s s)
              (println :advance-state :reduce/entering :rs rs)
              (if-not (empty? rs)
                (let [room      (first rs)
                      new-state (-> s
                                  ; TODO: fix this someday
                                  ;(sp/transform [:rooms room :moving1-time-remaining] dec)
                                  ;(sp/transform [sp/ALL (sp/pred #(= room (:id %))) :moving1-time-remaining] inc)
                                  (assoc :rooms (utils/update-by-id-apply-fn (-> s :rooms)
                                                  room
                                                  #(sp/transform [:moving1-time-remaining] dec %))))]
                  (recur new-state (rest rs)))
                ; termination case
                s))
      state
      [rooms-moving])))


(comment
  (rooms-being-moved (-> @*state last))
  (advance-state (-> @*state last))
  (-> @*state last)

  ; clunky
  (-> @*state last
    ((fn [s]
       (println s)
       (println (-> s :rooms))
       (utils/update-by-id-apply-fn (-> s :rooms) 0 #(update-in % [:moving1-time-remaining] dec)))))

  ; try with specter
  (-> @*state last
    ((fn [s]
       (println s)
       (println (-> s :rooms))
       (utils/update-by-id-apply-fn (-> s :rooms) 0 #(update-in % [:moving1-time-remaining] dec)))))

  (def st {:turn 0,
           :rooms [{:id 0,
                    :role :room,
                    :state :removing-furniture
                    :moving1-time-remaining 10,
                    :painting-time-remaining 50,
                    :moving2-time-remaining 10}
                   {:id 1,
                    :role :room,
                    :state :waiting-for-movers1,
                    :moving1-time-remaining 10,
                    :painting-time-remaining 50,
                    :moving2-time-remaining 10}
                   {:id 2,
                    :role :room,
                    :state :waiting-for-movers1,
                    :moving1-time-remaining 10,
                    :painting-time-remaining 50,
                    :moving2-time-remaining 10}
                   {:id 3,
                    :role :room,
                    :state :waiting-for-movers1,
                    :moving1-time-remaining 10,
                    :painting-time-remaining 50,
                    :moving2-time-remaining 10}],
           :movers [{:id 0, :role :mover, :at-room nil} {:id 1, :role :mover, :at-room nil}],
           :painters [{:id 0, :role :painter, :at-room nil}
                      {:id 1, :role :painter, :at-room nil}
                      {:id 2, :role :painter, :at-room nil}
                      {:id 3, :role :painter, :at-room nil}]})


  (->> st
    (sp/transform [:turn] inc))
  (sp/transform [:turn] inc st)
  (->> st
    (sp/transform [:rooms 0 :moving1-time-remaining] dec))

  (->> st
    (sp/select [:rooms 0 :state]))
  (->> st
    ((fn [x]
       (let [rstate (-> (sp/select [:rooms 0 :state] x) last)]
         ;rstate))))
         (sp/setval [:rooms 0 :state] (get e/next-room-state rstate) x)))))

  0)


(>defn next-turn!
  " updates state atom: append the new state to end of *states vector
    if no args, copy state and increment turn"
  ([*states] [atom? => sequential?]
   (swap! *states conj
     (let [state (last @*states)]
       (assoc state :turn (inc (:turn state))))))
  ([*states newstate] [atom? map? => sequential?]
   (swap! *states conj
     (assoc newstate :turn (inc (:turn newstate))))))

(>defn increment-state
  " identity, but just increment turn "
  [state] [map? => map?]
  (assoc state :turn (inc (:turn state))))

(>defn next-turn-fn!
  " updates state atom, using f to update turn "
  [*states f] [atom? fn? => sequential?]
  (swap! *states conj
    (let [state (last @*states)]
      (f state))))

(comment
  (next-turn! *state)
  (swap! *state next-turn)
  0)




(comment
  (e/rooms-needing-movers (-> @*state last :rooms))
  0)


(s/def ::s-moving-assignment
  (s/keys :opt-un [::room ::mover]))

(>defn assign-room
  [[room mover]] [vector? => ::s-moving-assignment]
  ; case 1: no movers
  ; case 2: more movers than rooms
  ; case 3: mover rooms than mover
  ;
  ; put them into one vector
  (println :assign-room :room room)
  (println :assign-room :mover mover)
  (if (and room mover)
    (let [newroom (assoc room :state :removing-furniture)
                              ;:moving1-time-remaining (dec (-> room :moving1-time-remaining)))
          newmover (assoc mover :at-room (-> room :id))
          retval   {:room newroom
                    :mover newmover}]
      (println :assign-room "**** assign! " :retval retval)
      retval)))





(>defn assign-available-movers
  " for every room that needs mover/painter, assign one that is available
  "
  [state] [::e/s-state => ::e/s-state]
  (let [needs-movers (e/rooms-needing-movers (-> state :rooms))
        movers       (e/available-movers state)
        _            (println :assign-available-movers :needs-movers needs-movers)
        _            (println :assign-available-movers :movers movers)
        room-movers  (map vector needs-movers movers)
        ; this creates [{:room newroom :mover newmover}...]
        _             (println :assign-available-movers :rooms-movers room-movers)
        new-rooms-movers (->> room-movers
                           (map assign-room)
                           (remove nil?))
        _             (println :assign-available-movers :new-rooms-movers new-rooms-movers)
        newstate        (reduce
                          utils/update-rooms-movers state
                          [new-rooms-movers])]
    #_(println :assign-available-movers :new-room-movers
        (with-out-str (clojure.pprint/pprint new-rooms-movers)))
    newstate))


#trace
(>defn free-completed-movers
  " for every room that has done mover/painter:
      advance room state
      set mover :at-room to nil
  "
  [state] [::e/s-state => ::e/s-state]
  (let [done-rooms  (->> (e/rooms-done-with-movers (-> state :rooms))
                         (map :id))
        ; ^^ list of rooms that are done (0 1 2)
        ; now we need to
        newstate    (utils/free-room-movers state done-rooms)]
    (println :free-completed-movers :done-rooms done-rooms)
    (def ns2 newstate)
    newstate))

(comment
  (e/rooms-done-with-movers (-> @*state last :rooms))
  0)



