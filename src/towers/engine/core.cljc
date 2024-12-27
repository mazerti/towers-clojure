(ns towers.engine.core
  "A namespace for all the logic responsible for running the game."
  (:require
    [towers.engine.construct :refer [apply-to-all-players
                                     create-game
                                     get-square-attribute
                                     get-dimensions
                                     get-player
                                     get-player-ids
                                     update-square
                                     update-player]]
    [ysera.collections :refer [index-of]]
    [ysera.test :refer [is is-not is=]]))

(defn build-tower
  "Add a tower level to the board at the given location."
  {:test (fn []
           (let [game (-> (create-game)
                          (build-tower [1 2]))]
             (is= (get-square-attribute game :height [1 2])
                  1)
             (is= (-> (build-tower game [1 2])
                      (get-square-attribute :height [1 2]))
                  2)))}
  [game location]
  (update-square game :height location inc))


(defn player-in-turn?
  "Test if given player is currently in turn."
  {:test (fn []
           (is (-> (create-game :settings {:player-ids ["a" "b"]})
                   (player-in-turn? "a")))
           (is-not (-> (create-game :settings {:player-ids ["a" "b"]})
                       (player-in-turn? "b"))))}
  [state player-id]
  (= player-id (:player-id-in-turn state)))


(defn start-picked?
  "Checks if given player has already picked a start.
  Practically a player is assumed to have picked a start if they control at least one square on the board."
  {:test (fn []
           (let [game (create-game :board [[0 1 0 {:pawn          "p2"
                                                   :controlled-by "p2"}]
                                           [0 0 0 0]
                                           [0 0 0 0]
                                           [0 0 0 {:pawn          "p3"
                                                   :controlled-by "p3"}]])]
             (is-not (start-picked? game "p1"))
             (is (start-picked? game "p2"))
             (is (start-picked? game "p3"))))}
  [game player-id]
  (->> (:board game)
       (filter (fn [[_ square]] (= (:controlled-by square) player-id)))
       (not-empty)))


(defn get-next-player-id
  "Return the id of the player playing after the player with given id or nil if it's the end of beginning phase."
  {:test (fn []
           (let [game (create-game)]
             (is= (get-next-player-id game "p1") "p2")
             (is= (get-next-player-id game "p3") "p1"))
           ; Players that already picked a start are skipped.
           (is= (-> (create-game :board [[0 1 0 {:pawn          "p2"
                                                 :controlled-by "p2"}]
                                         [0 0 0 0]
                                         [0 0 0 0]
                                         [0 0 0 0]])
                    (get-next-player-id "p1"))
                "p3")
           ; End of beginning phase when all players have picked a start.
           (is= (-> (create-game :board [[0 1 0 {:pawn          "p1"
                                                 :controlled-by "p1"}]
                                         [{:pawn          "p2"
                                           :controlled-by "p2"} 0 0 0]
                                         [0 0 0 0]
                                         [0 0 {:pawn          "p3"
                                               :controlled-by "p3"} 0]])
                    (get-next-player-id "p1"))
                nil))}
  [game base-id]
  (let [player-ids (get-player-ids game)]
    (loop [id base-id]
      (let [next-player-id (as-> player-ids $
                                 (index-of $ id)
                                 (inc $)
                                 (if (< $ (count player-ids))
                                   (get player-ids $)
                                   (first player-ids)))]
        (cond
          (= next-player-id base-id) nil
          (start-picked? game next-player-id) (recur next-player-id)
          :else next-player-id)))))


(defn start-core-phase
  "Start the core phase."
  {:test (fn []
           (let [game (-> (create-game :players [{:id "p1" :playing-order 2}
                                                 {:id "p2" :playing-order 1}
                                                 {:id "p3" :playing-order 3}])
                          (start-core-phase))]
             (is= (:phase game)
                  :core)
             ; The order of the players have been updated
             (is= (->> (:players game)
                       (map :id))
                  ["p2" "p1" "p3"])
             ; The player in turn has been updated
             (is= (:player-id-in-turn game)
                  "p2")
             ; :playing-order attributes are removed since we don't need them anymore.
             (is (->> (:players game)
                      (map keys)
                      (reduce into)
                      (filter (fn [x] (= x :playing-order)))
                      (empty?)))))}
  [game]
  (as-> game game
        (assoc game :phase :core)
        (update game :players (fn [players] (->> players
                                                 (sort-by :playing-order)
                                                 (vec))))
        (apply-to-all-players game (fn [player] (dissoc player :playing-order)))
        (assoc game :player-id-in-turn (-> (:players game)
                                           (first)
                                           (:id)))))


(defn end-turn
  "End the turn of the current player."
  {:test (fn []
           (let [game (-> (create-game)
                          (end-turn))]
             (is= (:player-id-in-turn game)
                  "p2")
             (is= (:phase game)
                  :beginning)
             )
           ; players that already picked a start are skipped.
           (is= (-> (create-game :board [[0 1 0 {:pawn          "p2"
                                                 :controlled-by "p2"}]
                                         [0 0 0 0]
                                         [0 0 0 0]
                                         [0 0 0 0]])
                    (end-turn)
                    (:player-id-in-turn))
                "p3")
           ; When all players have picked a start, the game goes in core phase.
           (let [game (-> (create-game :board [[0 1 0 {:pawn          "p1"
                                                       :controlled-by "p1"}]
                                               [{:pawn          "p2"
                                                 :controlled-by "p2"} 0 0 0]
                                               [0 0 0 0]
                                               [0 0 {:pawn          "p3"
                                                     :controlled-by "p3"} 0]]
                                       :players [{:id "p1" :playing-order 2}
                                                 {:id "p2" :playing-order 3}
                                                 {:id "p3" :playing-order 1}])
                          (end-turn))]
             (is= (:phase game)
                  :core)
             (is= (:player-id-in-turn game)
                  "p3")))}
  [game]
  (let [next-player-id (get-next-player-id game (:player-id-in-turn game))
        phase (:phase game)]
    (if (and (= phase :beginning)
             (nil? next-player-id))
      (start-core-phase game)
      (assoc game :player-id-in-turn next-player-id))))


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
  "Returns the locations of the squares neighboring given location."
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
     (get-square-attribute game :pawn location) false
     (= range 0) true
     :default (every? true?
                      (->> (neighbors game location)
                           (filter (fn [l] (on-the-border? game l)))
                           (map (fn [l] (respects-start-distances? game l (dec range))))))))
  ([game location]
   (respects-start-distances? game location 2)))


(defn can-place-tower?
  "Checks if the given arguments to a pick-start action are valid."
  {:test (fn []
           (is (-> (create-game)
                   (can-place-tower? "p1")))
           ; can only place a tower on your turn.
           (is-not (-> (create-game)
                       (can-place-tower? "p2")))
           ; can not place a tower if all other players have picked a start.
           (is-not (-> (create-game :settings {:player-ids ["p1"]})
                       (can-place-tower? "p1")))
           (is-not (-> (create-game :board [[0 1 0 {:pawn          "p2"
                                                    :controlled-by "p2"}]
                                            [0 0 0 0]
                                            [0 0 0 0]
                                            [0 0 0 {:pawn          "p3"
                                                    :controlled-by "p3"}]])
                       (can-place-tower? "p1")))
           ; TODO: exception for player count = 2
           )}
  [game player-id]
  (and (player-in-turn? game player-id)
       (->> (get-player-ids game)
            (remove (fn [id] (= id player-id)))
            (remove (fn [id] (start-picked? game id)))
            (not-empty))))


(defn can-pick-start?
  "Checks if the given arguments to a pick-start action are valid."
  {:test (fn []
           (let [game (create-game)]
             (is (can-pick-start? game "p1" [2 1]))
             ; A picked square can not be picked by another player nor any square in a range of 2.
             (is-not (can-pick-start? game "p2" [1 0]))
             (is-not (can-pick-start? game "p2" [2 0]))
             (is-not (can-pick-start? game "p2" [2 1]))
             (is-not (can-pick-start? game "p2" [2 2]))
             (is-not (can-pick-start? game "p2" [1 2])))
           ; Can only pick a start on your turn.
           (is-not (-> (create-game)
                       (can-pick-start? "p2" [2 1])))
           ; Can only pick a bordering square for start.
           (is-not (-> (create-game)
                       (can-pick-start? "p1" [1 1])))
           (is-not (-> (create-game :settings {:dimensions 5})
                       (can-pick-start? "p1" [2 1]))))}
  [game player-id location]
  (and (player-in-turn? game player-id)
       (on-the-border? game location)
       (respects-start-distances? game location)))


(defn claim-square
  "Given player get the control of given square.
  i.e. placing a flag in the board game."
  {:test (fn []
           (let [game (-> (create-game)
                          (claim-square "p2" [2 1]))]
             (is= (get-square-attribute game :controlled-by [2 1])
                  "p2")
             (is= (-> (claim-square game "p1" [2 1])
                      (get-square-attribute :controlled-by [2 1]))
                  "p1")))}
  [game player-id location]
  (update-square game :controlled-by location player-id))


(defn remaining-pawns
  "Returns the number of pawn a player have in its reserve."
  {:test (fn []
           (is= (-> (create-game :settings {:number-of-pawns 3})
                    (remaining-pawns "p1"))
                3))}
  [game player-id]
  (-> (get-player game player-id)
      (:pawns)))


(defn summon-pawn
  "Given player summon a pawn at given location."
  {:test (fn []
           (let [game (-> (create-game)
                          (summon-pawn "p2" [2 1]))]
             (is= (get-square-attribute game :pawn [2 1])
                  "p2")
             (is= (remaining-pawns game "p2")
                  5)))}
  [game player-id location]
  (-> game
      (update-square :pawn location player-id)
      (update-player :pawns player-id dec)))


(defn register-player-order
  "Give given player a turn number.
  The playing order in the core phase will respect the order this function is called with in the beginning phase."
  {:test (fn []
           (is= (-> (create-game)
                    (register-player-order "p2")
                    (get-player "p2")
                    (:playing-order))
                1)
           (is= (-> (create-game :players [{:id "p1" :playing-order 1}
                                           {:id "p2"}
                                           {:id "p3"}])
                    (register-player-order "p2")
                    (get-player "p2")
                    (:playing-order))
                2)
           )}
  [game player-id]
  (let [order (->> (:players game)
                   (map :playing-order)
                   (remove nil?)
                   (apply max 0)
                   (inc))]
    (update-player game :playing-order player-id order)))
