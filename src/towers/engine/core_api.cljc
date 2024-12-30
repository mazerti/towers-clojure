(ns towers.engine.core-api
  "Serve as the expose layer of the engine. Provide all the functions for player actions."
  (:require
    [towers.engine.construct :refer [create-game
                                     get-player
                                     get-square-attribute
                                     update-player]]
    [towers.engine.core :refer [build-tower
                                can-pick-start?
                                can-place-tower?
                                can-spawn-pawn?
                                claim-square
                                end-turn
                                player-in-turn?
                                register-player-order
                                summon-pawn]]
    [ysera.error :refer [error]]
    [ysera.test :refer [error? is is=]]))


(defn place-tower
  "During the beginning phase, given player place one tower on given square."
  {:test (fn []
           (let [game (-> (create-game)
                          (place-tower "p1" [1 2]))]
             (is= (get-square-attribute game :height [1 2])
                  1)
             (is (player-in-turn? game "p2")))
           ; Can't use the action if the player is not in turn.
           (error? (-> (create-game)
                       (place-tower "p2" [1 2])))
           ; Can only use the action in the beginning phase
           (error? (-> (create-game :phase :core)
                       (place-tower "p1" [1 2]))))}
  [game player-id location]
  (when-not (can-place-tower? game player-id)
    (error "Invalid play."))
  (-> game
      (build-tower location)
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
               ; TODO: manage action count system.
               )
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
  (summon-pawn game player-id location))