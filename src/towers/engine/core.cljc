(ns towers.engine.core
  "A namespace for all the logic responsible for running the game."
  (:require
    [towers.engine.construct :refer [create-game
                                     get-case-attribute
                                     update-case]]
    [ysera.collections :refer [index-of]]
    [ysera.test :refer [is is=]]))

(defn build-tower
  "Add a tower level to the board at the given location."
  {:test (fn []
           (let [game (-> (create-game)
                          (build-tower [1 2]))]
             (is= (get-case-attribute game :height [1 2])
                  1)
             (is= (-> (build-tower game [1 2])
                      (get-case-attribute :height [1 2]))
                  2)))}
  [game location]
  (update-case game :height location inc))


(defn next-player-id
  "Return the id of the player playing after the player with given id."
  {:test (fn []
           (let [game (create-game)]
             (is= (next-player-id game "p1") "p2")
             (is= (next-player-id game "p3") "p1")))}
  [game id]
  (let [player-ids (->> (:players game)
                        (mapv :id))]
    (as-> player-ids $
          (index-of $ id)
          (inc $)
          (if (< $ (count player-ids))
            (get player-ids $)
            (first player-ids)))))


(defn end-turn
  "End the turn of the current player."
  {:test (fn []
           (is= (-> (create-game)
                    (end-turn)
                    (:player-id-in-turn))
                "p2"))}
  [game]
  (update game :player-id-in-turn (fn [id] (next-player-id game id))))

