(ns towers.gateway.repl.view
  (:require
    [towers.engine.construct :refer [preview-board]]
    [towers.gateway.repl.state :refer [game-atom
                                       create-game!
                                       spawn-tower!
                                       pick-start!
                                       spawn-pawn!
                                       build-tower!
                                       move-pawn!]]))


(defn player-in-turn
  "Return the player that is currently playing."
  []
  (-> (deref game-atom)
      (:player-id-in-turn)))


(defn display-game
  "Display the current state of the game."
  []
  (println (player-in-turn) "'s turn")
  (clojure.pprint/pprint (preview-board (deref game-atom))))


(defn play!
  "Play a move"
  [move-fn & args]
  (let [player-id (-> (deref game-atom)
                      (:player-id-in-turn))]
    (apply move-fn player-id args)
    (display-game)))
