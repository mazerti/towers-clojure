(ns towers.engine.core-api
  "Serve as the expose layer of the engine. Provide all the functions for player actions."
  (:require
    [towers.engine.construct :refer [create-game
                                     get-case-attribute]]
    [towers.engine.core :refer [build-tower
                                end-turn
                                player-in-turn?]]
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
                       (place-tower "p2" [1 2]))))}
  [game player-id case-location]
  (when-not (player-in-turn? game player-id)
    (error (format "Playing out of turn." player-id)))
  (-> game
      (build-tower case-location)
      (end-turn)))
