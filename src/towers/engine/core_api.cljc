(ns towers.engine.core-api
  "Serve as the expose layer of the engine. Provide all the functions for player actions."
  (:require
    [towers.engine.construct :refer [create-game
                                     get-case-attribute]]
    [towers.engine.core :refer [build-tower
                                can-pick-start?
                                claim-case
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
             (is= (get-case-attribute game :height [1 2])
                  1)
             (is (player-in-turn? game "p2")))
           (error? (-> (create-game)
                       (place-tower "p2" [1 2])))
           ; TODO: if the player is the only one that haven't picked a start yet, this action is unavailable (or only once for 2 or less player games).
           )}
  [game player-id case-location]
  (when-not (player-in-turn? game player-id)
    (error (format "Playing out of turn." player-id)))
  (-> game
      (build-tower case-location)
      (end-turn)))


(defn pick-start
  "During the first phase of the game, given player pick a case to be its starting location."
  {:test (fn []
           (let [game (-> (create-game)
                          (pick-start "p1" [2 1]))]
             (is= (get-case-attribute game :pawn [2 1])
                  "p1")
             (is= (get-case-attribute game :controlled-by [2 1])
                  "p1")
             ; A picked case can not be picked by another player nor any case in a range of 2.
             (error? (pick-start game "p2" [1 0]))
             (error? (pick-start game "p2" [2 0]))
             (error? (pick-start game "p2" [2 1]))
             (error? (pick-start game "p2" [2 2]))
             (error? (pick-start game "p2" [1 2])))
           ; Can only pick a start on your turn.
           (error? (-> (create-game)
                       (pick-start "p2" [2 1])))
           ; Can only pick a bordering case for start.
           (error? (-> (create-game)
                       (pick-start "p1" [1 1])))
           (error? (-> (create-game :settings {:dimensions 5})
                       (pick-start "p1" [2 1]))))}
  [game player-id location]
  (when-not (and (can-pick-start? game player-id location))
    (error (format "Invalid play.")))
  (-> game
      (claim-case player-id location)
      (summon-pawn player-id location)
      (end-turn)))
