(ns towers.engine.core-api
  "Serve as the expose layer of the engine. Provide all the functions for player actions."
  (:require
    [towers.engine.construct :refer [create-game
                                     get-square-attribute]]
    [towers.engine.core :refer [build-tower
                                can-pick-start?
                                can-place-tower?
                                claim-square
                                end-turn
                                player-in-turn?
                                summon-pawn]]
    [ysera.error :refer [error]]
    [ysera.test :refer [error? is is=]]))

(defn place-tower
  "During the first phase of the game, given player place one tower on given square."
  {:test (fn []
           (let [game (-> (create-game)
                          (place-tower "p1" [1 2]))]
             (is= (get-square-attribute game :height [1 2])
                  1)
             (is (player-in-turn? game "p2")))
           (error? (-> (create-game)
                       (place-tower "p2" [1 2]))))}
  [game player-id square-location]
  (when-not (can-place-tower? game player-id)
    (error (format "Invalid play." player-id)))
  (-> game
      (build-tower square-location)
      (end-turn)))


(defn pick-start
  "During the first phase of the game, given player pick a square to be its starting location."
  {:test (fn []
           (let [game (-> (create-game)
                          (pick-start "p1" [2 1]))]
             (is= (get-square-attribute game :pawn [2 1])
                  "p1")
             (is= (get-square-attribute game :controlled-by [2 1])
                  "p1")
             ; A picked square can not be picked by another player nor any square in a range of 2.
             (error? (pick-start game "p2" [1 0]))
             (error? (pick-start game "p2" [2 0]))
             (error? (pick-start game "p2" [2 1]))
             (error? (pick-start game "p2" [2 2]))
             (error? (pick-start game "p2" [1 2])))
           ; Can only pick a start on your turn.
           (error? (-> (create-game)
                       (pick-start "p2" [2 1])))
           ; Can only pick a bordering square for start.
           (error? (-> (create-game)
                       (pick-start "p1" [1 1])))
           (error? (-> (create-game :settings {:dimensions 5})
                       (pick-start "p1" [2 1]))))}
  [game player-id location]
  (when-not (and (can-pick-start? game player-id location))
    (error (format "Invalid play.")))
  (-> game
      (claim-square player-id location)
      (summon-pawn player-id location)
      (end-turn)))
