(ns towers.engine.core-api
  "Serve as the expose layer of the engine. Provide all the functions for player actions."
  (:require
    [towers.engine.construct :refer [create-game]]
    [towers.engine.core :refer [build-tower
                                end-turn
                                player-in-turn?]]))

(defn place-tower
  "During the first phase of the game, given player place one tower on given square."
  {:test (fn []
           (let [state (-> (create-game)
                           (place-tower "p1" [2 3]))]
             ())
           )}
  [state player-id case-location]
  (when-not (player-in-turn? state player-id)
    (throw (format "Player %s can't play. It is not their turn." player-id)))
  (-> state
      (build-tower case-location)
      (end-turn state)))
