(ns towers.engine.core-api
  "Serve as the expose layer of the engine. Provide all the functions for player actions."
  (:require
    [towers.engine.construct :refer [create-game
                                     get-player
                                     get-square-attribute
                                     update-player]]
    [towers.engine.core :refer [place-tower
                                can-build-tower?
                                can-pick-start?
                                can-spawn-tower?
                                can-spawn-pawn?
                                claim-square
                                end-turn
                                player-in-turn?
                                register-player-order
                                register-last-action
                                summon-pawn]]
    [ysera.error :refer [error]]
    [ysera.test :refer [error? is is=]]))


(defn spawn-tower
  "During the beginning phase, given player place one tower on given square."
  {:test (fn []
           (let [game (-> (create-game)
                          (spawn-tower "p1" [1 2]))]
             (is= (get-square-attribute game :height [1 2])
                  1)
             (is (player-in-turn? game "p2")))
           ; Can't use the action if the player is not in turn.
           (error? (-> (create-game)
                       (spawn-tower "p2" [1 2])))
           ; Can only use the action in the beginning phase
           (error? (-> (create-game :phase :core)
                       (spawn-tower "p1" [1 2]))))}
  [game player-id location]
  (when-not (can-spawn-tower? game player-id)
    (error "Invalid play."))
  (-> game
      (place-tower location)
      (end-turn)))


(defn pick-start
  "During the beginning phase, given player pick a square to be its starting location."
  {:test (fn []
           (let [game (pick-start (create-game) "p1" [2 1])]
             (is= (get-square-attribute game :pawn [2 1])
                  "p1")
             (is= (get-square-attribute game :controlled-by [2 1])
                  "p1")
             (is (player-in-turn? game "p2")))
           ; Can only pick a start in the beginning phase
           (error? (-> (create-game :phase :core)
                       (pick-start "p1" [2 1])))
           ; A picked square can not be picked by another player nor any square in a range of 2.
           (error? (pick-start (create-game) "p2" [1 0]))
           (error? (pick-start (create-game) "p2" [2 0]))
           (error? (pick-start (create-game) "p2" [2 1]))
           (error? (pick-start (create-game) "p2" [2 2]))
           (error? (pick-start (create-game) "p2" [1 2]))
           ; Can only pick a start on your turn.
           (error? (pick-start (create-game) "p2" [2 1]))
           ; Can only pick a bordering square for start.
           (error? (pick-start (create-game) "p1" [1 1]))
           (error? (-> (create-game :settings {:dimensions 5})
                       (pick-start "p1" [2 1]))))}
  [game player-id location]
  (when-not (can-pick-start? game player-id location)
    (error "Invalid play."))
  (-> game
      (claim-square player-id location)
      (summon-pawn player-id location)
      (register-player-order player-id)
      (end-turn)))


(defn spawn-pawn
  "During the core phase, action of spawning a pawn on a controlled square."
  {:test (fn []
           (let [game (create-game :board [[{:controlled-by "p1" :pawn "p1"} {:controlled-by "p1"} 0]
                                           [{:controlled-by "p2"} 0 0]
                                           [0 0 0]]
                                   :phase :core
                                   :player-id-in-turn "p1")]
             (let [game (spawn-pawn game "p1" [0 1])]
               (is= (get-square-attribute game :pawn [0 1])
                    "p1")
               (is= (-> (get-player game "p1")
                        (:pawns))
                    5)
               (is (player-in-turn? game "p1"))
               (is= (:last-action game)
                    {:action        :spawn-pawn
                     :pawn-location [0 1]})
               ; Can't redo the very same action twice in the same turn.
               (error? (spawn-pawn game "p1" [0 1])))
             ; Can't use that action in the beginning phase
             (error? (-> (assoc game :phase :beginning)
                         (spawn-pawn "p1" [0 1])))
             ; Can't use that action if the player is not in turn
             (error? (spawn-pawn game "p2" [0 1]))
             ; Can't use that action on a square that is occupied
             (error? (spawn-pawn game "p1" [0 0]))
             ; Can't use that action on a square that is not controlled by the player
             (error? (spawn-pawn game "p1" [0 2]))
             (error? (spawn-pawn game "p1" [1 0]))
             ; Can't use the action if the player is running out of pawns
             (error? (-> (update-player game :pawns "p1" 0)
                         (spawn-pawn "p1" [0 1])))))}
  [game player-id location]
  (when-not (can-spawn-pawn? game player-id location)
    (error "Invalid play."))
  (as-> (summon-pawn game player-id location) $
      (if (:last-action $)
        (end-turn $)
        (register-last-action $ :spawn-pawn location))))


(defn build-tower
  "During the core phase, action of building a tower with a pawn."
  {:test (fn []
           (let [game (create-game :board [[{:controlled-by "p1" :pawn "p1" :height 1} {:controlled-by "p1"} 0]
                                           [{:controlled-by "p2" :pawn "p2"} 0 0]
                                           [0 0 0]]
                                   :phase :core
                                   :player-id-in-turn "p1")]
             (let [game (build-tower game "p1" [0 0])]
               (is= (get-square-attribute game :height [0 0])
                    2)
               (is= (:unbuilt-towers game)
                    49)
               (is (player-in-turn? game "p1"))
               (is= (:last-action game)
                    {:action        :build-tower
                     :pawn-location [0 0]})
               ; Can't redo the very same action twice in the same turn.
               (error? (build-tower game "p1" [0 0])))
             ; Can't use that action in the beginning phase
             (error? (-> (assoc game :phase :beginning)
                         (build-tower "p1" [0 0])))
             ; Can't use that action if the player is not in turn
             (error? (build-tower game "p2" [1 0]))
             ; Can't use that action if the player don't have a pawn on the square.
             (error? (build-tower game "p1" [0 1]))
             (error? (build-tower game "p1" [1 0]))
             (error? (build-tower game "p1" [1 1]))
             ))}
  [game player-id location]
  (when-not (can-build-tower? game player-id location)
    (error "Invalid play."))
  (as-> (place-tower game location) $
        (if (:last-action $)
          (end-turn $)
          (register-last-action $ :build-tower location))))
