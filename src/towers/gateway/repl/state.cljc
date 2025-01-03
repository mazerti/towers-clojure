(ns towers.gateway.repl.state)

(defonce game-atom (atom nil))


(defn create-game!
  "Generates a fresh game state and stores it as the currently ongoing game."
  [& params]
  (reset! game-atom (apply towers.engine.construct/create-game params)))


(defn spawn-tower!
  "During the beginning phase, given player place one tower on given square."
  [player-id location]
  (swap! game-atom towers.engine.core-api/spawn-tower player-id location))


(defn pick-start!
  "During the beginning phase, given player pick a square to be its starting location."
  [player-id location]
  (swap! game-atom towers.engine.core-api/pick-start player-id location))


(defn spawn-pawn!
  "During the core phase, given player spawn a pawn on one of it's controlled square."
  [player-id location]
  (swap! game-atom towers.engine.core-api/spawn-pawn player-id location))


(defn build-tower!
  "During the core phase, given player build a tower on one of its pawns' location."
  [player-id location]
  (swap! game-atom towers.engine.core-api/build-tower player-id location))


(defn move-pawn!
  "During the core phase, given player move one of its pawns to a neighboring square."
  [player-id from to]
  (swap! game-atom towers.engine.core-api/move-pawn player-id from to))
