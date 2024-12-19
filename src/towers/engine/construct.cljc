(ns towers.engine.construct
  "A namespace for the non-gameplay related building blocks of the game."
  (:require [ysera.test :refer [is=]]))


(def default-settings
  "A map listing the default settings of a game."
  {:dimensions       [3 3]
   :player-ids       ["p1" "p2" "p3"]
   :number-of-towers (* 2 (reduce * 1 (:dimensions default-settings)))
   :number-of-pawns  6})


(defn create-case
  "Create a case on the board with given attributes."
  {:test (fn []
           (is= (create-case [0 1] :height 2 :pawn "p2")
                {:location [0 1]
                 :height   2
                 :pawn     "p2"}))}
  [location & key-values]
  (cond-> {:location location
           :height   0}
          (not (nil? key-values)) (#(apply assoc % key-values))))


(defn create-empty-game
  "Create a game instance matching the input setup."
  {:test (fn []
           ; Default game (for tests)
           (is= (create-empty-game)
                {:map               {[0 0] {:location [0 0] :height 0}
                                     [0 1] {:location [0 1] :height 0}
                                     [0 2] {:location [0 2] :height 0}
                                     [1 0] {:location [1 0] :height 0}
                                     [1 1] {:location [1 1] :height 0}
                                     [1 2] {:location [1 2] :height 0}
                                     [2 0] {:location [2 0] :height 0}
                                     [2 1] {:location [2 1] :height 0}
                                     [2 2] {:location [2 2] :height 0}}
                 :players           {"p1" {:pawns 6}
                                     "p2" {:pawns 6}
                                     "p3" {:pawns 6}}
                 :player-id-in-turn "p1"
                 :unbuilt-towers    18})
           ; Custom settings
           (is= (create-empty-game :dimensions 2
                                   :player-ids [:a "b"]
                                   :number-of-towers 30
                                   :number-of-pawns 5)
                {:map               {[0 0] {:location [0 0] :height 0}
                                     [0 1] {:location [0 1] :height 0}
                                     [1 0] {:location [1 0] :height 0}
                                     [1 1] {:location [1 1] :height 0}}
                 :players           {:a  {:pawns 5}
                                     "b" {:pawns 5}}
                 :player-id-in-turn :a
                 :unbuilt-towers    30}))}
  [& settings]
  (let [settings (as-> default-settings $
                       (if (nil? settings) $
                                           (apply assoc $ settings))
                       (if-not (number? (:dimensions $)) $
                                                         (update $ :dimensions (fn [x] [x x]))))]
    {:map               (->> (for [i (range (-> (:dimensions settings)
                                                (first)))
                                   j (range (-> (:dimensions settings)
                                                (second)))]
                               {[i j] (create-case [i j])})
                             (into {}))
     :players           (->> (for [player-id (:player-ids settings)]
                               {player-id {:pawns (:number-of-pawns settings)}})
                             (into {}))
     :player-id-in-turn (-> (:player-ids settings)
                            (first))
     :unbuilt-towers    (:number-of-towers settings)}))

