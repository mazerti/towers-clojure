(ns towers.engine.core
  "A namespace for all the logic responsible for running the game."
  (:require
    [towers.engine.construct :refer [create-game
                                     get-case-attribute
                                     get-dimensions
                                     update-case]]
    [ysera.collections :refer [index-of]]
    [ysera.test :refer [is is-not is=]]))

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


(defn player-in-turn?
  "Test if given player is currently in turn."
  {:test (fn []
           (is (-> (create-game :settings {:player-ids ["a" "b"]})
                   (player-in-turn? "a")))
           (is-not (-> (create-game :settings {:player-ids ["a" "b"]})
                       (player-in-turn? "b"))))}
  [state player-id]
  (= player-id (:player-id-in-turn state)))


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
                "p2")
           ; TODO: ensure that players that already picked a start are skipped.
           ; TODO: When all players have picked a start, the game goes in phase 2.
           )}
  [game]
  (update game :player-id-in-turn (fn [id] (next-player-id game id))))


(defn in-bound?
  "Test if given location is a valid location on the board."
  {:test (fn []
           (let [game (create-game)]
             (is (in-bound? game [1 1]))
             (is-not (in-bound? game [1 3])))
           ; test with edited dimensions
           (let [game (create-game :settings {:dimensions [4 1]})]
             (is (in-bound? game [1 0]))
             (is-not (in-bound? game [1 1]))))}
  [game location]
  (let [dimensions (get-dimensions game)]
    (and (>= (first location) 0)
         (>= (second location) 0)
         (< (first location) (first dimensions))
         (< (second location) (second dimensions)))))


(defn neighbors
  "Returns the locations of the cases neighboring given location."
  {:test (fn []
           (let [game (create-game)]
             (is= (neighbors game [1 1])
                  #{[0 1] [1 0] [2 1] [1 2]})
             (is= (neighbors game [1 2])
                  #{[0 2] [1 1] [2 2]})
             (is= (neighbors game [2 2])
                  #{[1 2] [2 1]}))
           ; test with edited dimensions
           (let [game (create-game :settings {:dimensions [4 1]})]
             (is= (neighbors game [1 0])
                  #{[0 0] [2 0]})
             (is= (neighbors game [3 0])
                  #{[2 0]})))}
  [game location]
  (->> (for [fn-mod [(fn [[i j]] [(dec i) j])
                     (fn [[i j]] [(inc i) j])
                     (fn [[i j]] [i (dec j)])
                     (fn [[i j]] [i (inc j)])]]
         (fn-mod location))
       (filter (fn [l] (in-bound? game l)))
       (into #{})))


(defn on-the-border?
  "Test if the given location is on the border of the board."
  {:test (fn []
           (let [game (create-game)]
             (is (on-the-border? game [0 1]))
             (is (on-the-border? game [2 1]))
             (is (on-the-border? game [1 0]))
             (is (on-the-border? game [1 2])))
           ; test with different size
           (let [game (create-game :settings {:dimensions 5})]
             (is (on-the-border? game [4 2]))
             (is (on-the-border? game [1 4])))
           )}
  [game location]
  (let [dimensions (get-dimensions game)]
    (or (= (first location) 0)
        (= (second location) 0)
        (= (inc (first location)) (first dimensions))
        (= (inc (second location)) (second dimensions)))))


(defn respects-start-distances?
  "Checks if the given location respects the start distances.
  (2 spaces on each side by the border)."
  {:test (fn []
           (let [game (create-game :board [[0 {:pawn "p2"} 0]
                                           [0 0 0]
                                           [0 0 0]])]
             (is (respects-start-distances? game [2 0]))
             (is-not (respects-start-distances? game [0 1])) ; issue: the pawn is not added to the board.
             (is-not (respects-start-distances? game [0 0]))
             (is-not (respects-start-distances? game [1 2]))))}
  ([game location range]
   (cond
     (get-case-attribute game :pawn location) false
     (= range 0) true
     :default (every? true?
                      (->> (neighbors game location)
                           (filter (fn [l] (on-the-border? game l)))
                           (map (fn [l] (respects-start-distances? game l (dec range))))))))
  ([game location]
   (respects-start-distances? game location 2)))


(defn can-pick-start?
  "Checks if the given arguments to a pick-start action are valid."
  {:test (fn []
           (let [game (create-game)]
             (is (can-pick-start? game "p1" [2 1]))
             ; A picked case can not be picked by another player nor any case in a range of 2.
             (is-not (can-pick-start? game "p2" [1 0]))
             (is-not (can-pick-start? game "p2" [2 0]))
             (is-not (can-pick-start? game "p2" [2 1]))
             (is-not (can-pick-start? game "p2" [2 2]))
             (is-not (can-pick-start? game "p2" [1 2])))
           ; Can only pick a start on your turn.
           (is-not (-> (create-game)
                       (can-pick-start? "p2" [2 1])))
           ; Can only pick a bordering case for start.
           (is-not (-> (create-game)
                       (can-pick-start? "p1" [1 1])))
           (is-not (-> (create-game :settings {:dimensions 5})
                       (can-pick-start? "p1" [2 1]))))}
  [game player-id location]
  (and (player-in-turn? game player-id)
       (on-the-border? game location)
       (respects-start-distances? game location)))
