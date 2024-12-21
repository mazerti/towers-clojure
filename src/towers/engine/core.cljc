(ns towers.engine.core
  "A namespace for all the logic responsible for running the game."
  (:require
    [towers.engine.construct :refer [create-game
                                     get-case-attribute
                                     update-case]]
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

