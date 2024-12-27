(ns towers.gateway.repl.state)

(defonce game-atom (atom nil))


(defn create-game!
  "Generates a fresh game state and stores it as the currently ongoing game."
  [& params]
  (reset! game-atom (apply towers.engine.construct/create-game params)))


(defn place-tower!
  "During the beginning phase, given player place one tower on given square."
  [player-id location]
  (swap! game-atom towers.engine.core-api/place-tower player-id location))


(defn pick-start!
  "During the beginning phase, given player pick a square to be its starting location."
  [player-id location]
  (swap! game-atom towers.engine.core-api/pick-start player-id location))