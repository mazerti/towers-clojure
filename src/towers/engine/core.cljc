(ns towers.engine.core
  "A namespace for all the logic responsible for running the game."
  (:require
    [towers.engine.construct :refer [apply-to-all-players
                                     create-game
                                     dissoc-square-attribute
                                     get-square
                                     get-square-attribute
                                     get-dimensions
                                     get-player
                                     get-player-ids
                                     update-square
                                     update-player]]
    [ysera.collections :refer [index-of]]
    [ysera.test :refer [is is-not is=]]))

(defn place-tower
  "Add a tower level to the board at the given location."
  {:test (fn []
           (let [game (-> (create-game)
                          (place-tower [1 2]))]
             (is= (get-square-attribute game :height [1 2])
                  1)
             (is= (:unbuilt-towers game)
                  49)
             (is= (-> (place-tower game [1 2])
                      (get-square-attribute :height [1 2]))
                  2)))}
  [game location]
  (-> (update-square game :height location inc)
      (update :unbuilt-towers dec)))


(defn player-in-turn?
  "Test if given player is currently in turn."
  {:test (fn []
           (is (-> (create-game :settings {:player-ids ["a" "b"]})
                   (player-in-turn? "a")))
           (is-not (-> (create-game :settings {:player-ids ["a" "b"]})
                       (player-in-turn? "b"))))}
  [game player-id]
  (= player-id (:player-id-in-turn game)))


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
           ; Beginning phase
           (let [game (-> (create-game)
                          (end-turn))]
             (is= (:player-id-in-turn game)
                  "p2")
             (is= (:phase game)
                  :beginning))
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
                  "p3"))

           ; Core Phase
           (let [game (-> (create-game :phase :core
                                       :player-id-in-turn "p1"
                                       :last-action {:action        :spawn-pawn
                                                     :pawn-location [0 1]})
                          (end-turn))]
             (is= (:player-id-in-turn game)
                  "p2")
             (is= (:phase game)
                  :core)))}
  [game]
  (let [next-player-id (get-next-player-id game (:player-id-in-turn game))
        phase (:phase game)]
    (if (and (= phase :beginning)
             (nil? next-player-id))
      (start-core-phase game)
      (assoc game :player-id-in-turn next-player-id))))


(defn is-last-action?
  "Checks if the given action is the same as the last played action."
  {:test (fn []
           (is-not (-> (create-game)
                       (is-last-action? :spawn-pawn [0 2])))
           (let [game (create-game :board [[{:controlled-by "p1" :pawn "p1"} {:controlled-by "p1" :pawn "p1"} 0]
                                           [{:controlled-by "p2"} 0 0]
                                           [0 0 0]]
                                   :phase :core
                                   :player-id-in-turn "p1"
                                   :last-action {:action        :spawn-pawn
                                                 :pawn-location [0 0]})]
             (is (is-last-action? game :spawn-pawn [0 0]))
             (is-not (is-last-action? game :build-tower [0 1]))
             (is-not (is-last-action? game :build-tower [2 0]))
             (is-not (is-last-action? game :move-pawn [2 0])))
           (is (-> (create-game :board [[{:controlled-by "p1" :pawn "p1"} {:controlled-by "p1" :pawn "p1"} 0]
                                        [{:controlled-by "p2"} 0 0]
                                        [0 0 0]]
                                :phase :core
                                :player-id-in-turn "p1"
                                :last-action {:action        :build-tower
                                              :pawn-location [0 0]})
                   (is-last-action? :build-tower [0 0])))
           (is (-> (create-game :board [[{:controlled-by "p1" :pawn "p1"} {:controlled-by "p1" :pawn "p1"} 0]
                                        [{:controlled-by "p2" :pawn "p2"} 0 0]
                                        [0 0 0]]
                                :phase :core
                                :player-id-in-turn "p1"
                                :last-action {:action        :move-pawn
                                              :pawn-location [0 0]})
                   (is-last-action? :move-pawn [0 0]))))}
  [game action pawn-location]
  (let [last-action (:last-action game)]
    (and (= (:action last-action) action)
         (= (:pawn-location last-action) pawn-location))))


(defn register-last-action
  "Memorize the last action taken so that it can not be done twice."
  {:test (fn []
           (is (-> (create-game :phase :core)
                   (register-last-action :spawn-pawn [0 1])
                   (is-last-action? :spawn-pawn [0 1]))))}
  [game action pawn-location]
  (assoc game :last-action {:action        action
                            :pawn-location pawn-location}))


(defn end-action
  "End a player's action, registering it or ending the turn depending
  on if the action was the first or the second this turn."
  {:test (fn []
           (let [game (-> (create-game :phase :core
                                       :player-id-in-turn "p1")
                          (end-action :spawn-pawn [0 0]))]
             (is= (:player-id-in-turn game)
                  "p1")
             ; Register the resolved action
             (is-last-action? game :spawn-pawn [0 0]))
           (let [game (-> (create-game :phase :core
                                       :player-id-in-turn "p1"
                                       :last-action {:action        :spawn-pawn
                                                     :pawn-location [0 1]})
                          (end-action :spawn-pawn [0 0]))]
             (is= (:player-id-in-turn game)
                  "p2")
             ; Remove the last-action attribute.
             (is-not (contains? game :last-action))))}
  [game action end-location]
  (if (:last-action game)
    (-> game
        (end-turn)
        (dissoc :last-action))
    (register-last-action game action end-location)))


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


(defn can-spawn-tower?
  "Checks if the given arguments to a pick-start action are valid."
  {:test (fn []
           (is (-> (create-game)
                   (can-spawn-tower? "p1")))
           ; can only place a tower on your turn.
           (is-not (-> (create-game)
                       (can-spawn-tower? "p2")))
           ; can not place a tower if all other players have picked a start.
           (is-not (-> (create-game :settings {:player-ids ["p1"]})
                       (can-spawn-tower? "p1")))
           (is-not (-> (create-game :board [[0 1 0 {:pawn          "p2"
                                                    :controlled-by "p2"}]
                                            [0 0 0 0]
                                            [0 0 0 0]
                                            [0 0 0 {:pawn          "p3"
                                                    :controlled-by "p3"}]])
                       (can-spawn-tower? "p1")))
           ; TODO: exception for player count = 2
           ; Can only use the action in the beginning phase
           (is-not (-> (create-game :phase :core)
                       (can-spawn-tower? "p1"))))}
  [game player-id]
  (and (= (:phase game) :beginning)
       (player-in-turn? game player-id)
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
           ; Can only pick a start in the beginning phase
           (is-not (-> (create-game :phase :core)
                       (can-pick-start? "p1" [2 1])))
           ; Can only pick a bordering square for start.
           (is-not (-> (create-game)
                       (can-pick-start? "p1" [1 1])))
           (is-not (-> (create-game :settings {:dimensions 5})
                       (can-pick-start? "p1" [2 1]))))}
  [game player-id location]
  (and (= (:phase game) :beginning)
       (player-in-turn? game player-id)
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


(defn adjacent?
  "Checks if two given location are adjacents"
  {:test (fn []
           (is (adjacent? [1 0] [1 1]))
           (is (adjacent? [1 0] [0 0]))
           (is-not (adjacent? [1 0] [0 1]))
           (is-not (adjacent? [1 0] [0 2])))}
  [location1 location2]
  (= (->> (mapv - location1 location2)
          (map abs)
          (apply +))
     1))


(defn is-square-accessible-from
  "Checks if given square is accessible by given player from a given location."
  {:test (fn []
           (let [game (create-game :board [[{:pawn "p1"} 2 0]
                                           [{:pawn "p2" :height 1} {:pawn "p1" :height 2} {:pawn "p1"}]
                                           [0 {:pawn "p2" :height 3} 0]])]
             ; Can access lower squares without a friendly pawn on it
             (is (is-square-accessible-from game "p2" [2 1] [2 0]))
             (is (is-square-accessible-from game "p2" [2 1] [1 1]))
             (is (is-square-accessible-from game "p2" [2 1] [2 2]))
             ; Can not access higher squares with a pawn on it
             (is-not (is-square-accessible-from game "p2" [1 0] [1 1]))
             ; Can not access squares with a friendly pawn on it
             (is-not (is-square-accessible-from game "p1" [1 1] [1 2]))))}
  [game player-id from to]
  (let [defending-pawn (get-square-attribute game :pawn to)]
    (or (not defending-pawn)
        (and (not= defending-pawn player-id)
             (> (get-square-attribute game :height from)
                (get-square-attribute game :height to))))))


(defn accessible-squares-from
  "Returns the list of locations that can be accessed by given player from a given location."
  {:test (fn []
           (let [game (create-game :board [[{:pawn "p1"} 2 0]
                                           [{:pawn "p2" :height 1} {:pawn "p1" :height 2} {:pawn "p1"}]
                                           [0 {:pawn "p2" :height 3} 0]])]
             ; Does only return squares in range.
             (is= (-> (accessible-squares-from game "p1" [0 0])
                      (sort))
                  [[0 1]])
             ; Can not access higher squares with a pawn on it
             (is= (-> (accessible-squares-from game "p2" [1 0])
                      (sort))
                  [[0 0] [2 0]])
             ; Can not access squares with a friendly pawn on it
             (is= (-> (accessible-squares-from game "p1" [1 1])
                      (sort))
                  [[0 1] [1 0]])
             ; Can access lower squares with a non-friendly pawn on it
             (is= (-> (accessible-squares-from game "p2" [2 1])
                      (sort))
                  [[1 1] [2 0] [2 2]])))}
  [game player-id starting-location]
  (->> (neighbors game starting-location)
       (filter (fn [to] (is-square-accessible-from game player-id starting-location to)))))


(defn can-spawn-pawn?
  "Checks if the given arguments to a spawn-pawn action are valid."
  {:test (fn []
           (let [game (create-game :board [[{:controlled-by "p1" :pawn "p1"} {:controlled-by "p1"} 0]
                                           [{:controlled-by "p2"} 0 0]
                                           [0 0 0]]
                                   :phase :core
                                   :player-id-in-turn "p1")]
             (is (can-spawn-pawn? game "p1" [0 1]))
             ; Can't use that action in the beginning phase
             (is-not (-> (assoc game :phase :beginning)
                         (can-spawn-pawn? "p1" [0 1])))
             ; Can't redo the very same action twice in the same turn.
             (is-not (-> (assoc game :last-action {:action        :spawn-pawn
                                                   :pawn-location [0 1]})
                         (can-spawn-pawn? "p1" [0 1])))
             ; Can't use that action if the player is not in turn
             (is-not (can-spawn-pawn? game "p2" [0 1]))
             ; Can't use that action on a square that is occupied
             (is-not (can-spawn-pawn? game "p1" [0 0]))
             ; Can't use that action on a square that is not controlled by the player
             (is-not (can-spawn-pawn? game "p1" [0 2]))
             (is-not (can-spawn-pawn? game "p1" [1 0]))
             ; Can't use the action if the player is running out of pawns
             (is-not (-> (update-player game :pawns "p1" 0)
                         (can-spawn-pawn? "p1" [0 1])))))}
  [game player-id location]
  (and (= (:phase game) :core)
       (not (is-last-action? game :spawn-pawn location))
       (player-in-turn? game player-id)
       (nil? (get-square-attribute game :pawn location))
       (= (get-square-attribute game :controlled-by location) player-id)
       (> (-> (get-player game player-id)
              (:pawns)) 0)))


(defn can-build-tower?
  "Checks if the given arguments to a build-tower action are valid."
  {:test (fn []
           (let [game (create-game :board [[{:controlled-by "p1" :pawn "p1" :height 1} {:controlled-by "p1"} 0]
                                           [{:controlled-by "p2" :pawn "p2"} 0 0]
                                           [{:controlled-by "p1" :pawn "p1" :height 2} 0 0]]
                                   :phase :core
                                   :player-id-in-turn "p1")]
             (is (can-build-tower? game "p1" [0 0]))
             ; Can't use that action in the beginning phase
             (is-not (-> (assoc game :phase :beginning)
                         (can-build-tower? "p1" [0 0])))
             ; Can't use that action if the player is not in turn
             (is-not (can-build-tower? game "p2" [1 0]))
             ; Can't redo the very same action twice in the same turn.
             (is-not (-> (assoc game :last-action {:action        :build-tower
                                                   :pawn-location [0 0]})
                         (can-build-tower? "p1" [0 0])))
             ; Can't use that action if the player don't have a pawn on the square.
             (is-not (can-build-tower? game "p1" [0 1]))
             (is-not (can-build-tower? game "p1" [1 0]))
             (is-not (can-build-tower? game "p1" [1 1]))))}
  [game player-id location]
  (and (= (:phase game) :core)
       (player-in-turn? game player-id)
       (not (is-last-action? game :build-tower location))
       (= (get-square-attribute game :pawn location) player-id)))


(defn can-move-pawn?
  "Checks if the given arguments to a move-pawn action are valid."
  {:test (fn []
           (let [game (create-game :board [[{:controlled-by "p1" :pawn "p1"} {:controlled-by "p1" :pawn "p1" :height 1} 0]
                                           [{:controlled-by "p2" :height 4} {:controlled-by "p1" :pawn "p1" :height 2} {:controlled-by "p2" :pawn "p2" :height 2}]
                                           [0 {:controlled-by "p2" :pawn "p2"} {:controlled-by "p1"}]
                                           [0 0 0]]
                                   :phase :core
                                   :player-id-in-turn "p1")]
             ; Can move a pawn into a free space
             (is (can-move-pawn? game "p1" [0 1] [0 2]))
             ; Can move a pawn into a non-defended square
             (is (can-move-pawn? game "p1" [1 1] [1 0]))
             ; Can move a pawn into a lower defended square
             (is (can-move-pawn? game "p1" [1 1] [2 1]))
             ; Can't use that action in the beginning phase
             (is-not (-> (assoc game :phase :beginning)
                         (can-move-pawn? "p1" [0 1] [0 2])))
             ; Can't use that action if the player is not in turn
             (is-not (can-move-pawn? game "p2" [1 2] [0 2]))
             ; Can't redo the very same action twice in the same turn.
             (is-not (-> (assoc game :last-action {:action        :move-pawn
                                                   :pawn-location [0 1]})
                         (can-move-pawn? "p1" [0 1] [0 2])))
             ; Can't use that action if the player don't have a pawn on the square.
             (is-not (can-move-pawn? game "p1" [2 0] [3 0]))
             (is-not (can-move-pawn? game "p1" [1 0] [2 0]))
             (is-not (can-move-pawn? game "p1" [2 2] [3 2]))
             ; Can't move into a non-adjacent square
             (is-not (can-move-pawn? game "p1" [1 1] [1 1]))
             (is-not (can-move-pawn? game "p1" [1 1] [0 2]))
             (is-not (can-move-pawn? game "p1" [1 1] [3 1]))
             ; Can't move into a square occupied by a friendly pawn
             (is-not (can-move-pawn? game "p1" [0 1] [0 0]))
             ; Can't move into a non-lower defended square
             (is-not (can-move-pawn? game "p1" [1 1] [1 2]))
             ; Can't move into a square occupied by an allied pawn
             (is-not (can-move-pawn? game "p1" [1 1] [0 1]))))}
  [game player-id from to]
  (and (= (:phase game) :core)
       (player-in-turn? game player-id)
       (not (is-last-action? game :move-pawn from))
       (= (get-square-attribute game :pawn from) player-id)
       (adjacent? from to)
       (is-square-accessible-from game player-id from to)))


(defn add-to-captures
  "Add given pawn to the count of given player's captures."
  {:test (fn []
           (is= (-> (create-game)
                    (add-to-captures "p1" "p2")
                    (get-player "p1")
                    (:captures))
                {"p2" 1})
           (is= (-> (create-game :players [{:id "p1"} {:id "p2" :captures {"p1" 1}}])
                    (add-to-captures "p2" "p1")
                    (get-player "p2")
                    (:captures))
                {"p1" 2})
           (is= (-> (create-game :players [{:id "p1"} {:id "p2" :captures {"p1" 2}} {:id "p3"}])
                    (add-to-captures "p2" "p3")
                    (get-player "p2")
                    (:captures))
                {"p1" 2
                 "p3" 1}))}
  [game player-id captured-pawn]
  (update-player game :captures player-id (fn [captures]
                                            (update captures captured-pawn (fnil inc 0)))))


(defn capture-pawn
  "Given player capture pawn at given location.
  If there is no pawn on the square, nothing happens."
  {:test (fn []
           (-> (create-game :board [[{:pawn "p1"} {:pawn "p2"} 0]]
                            :players [{:id "p1"}
                                      {:id "p2"}
                                      {:id "p3" :captures {"p1" 1}}])
               (capture-pawn "p3" [0 0]))
           (let [game (create-game :board [[{:pawn "p1"} {:pawn "p2"} 0]]
                                   :players [{:id "p1"}
                                             {:id "p2"}
                                             {:id "p3" :captures {"p1" 1}}])]
             (let [game (capture-pawn game "p3" [0 2])]
               (is= (-> (get-player game "p3")
                        (:captures))
                    {"p1" 1}))
             (let [game (capture-pawn game "p3" [0 0])]
               (is-not (-> (get-square game [0 0])
                           (contains? :pawn)))
               (is= (-> (get-player game "p3")
                        (:captures)
                        (get "p1"))
                    2))
             (let [game (capture-pawn game "p3" [0 1])]
               (is (nil? (get-square-attribute game :pawn [0 1])))
               (is= (-> (get-player game "p3")
                        (:captures)
                        (get "p2"))
                    1))))}
  [game player-id location]
  (let [captured-pawn (get-square-attribute game :pawn location)]
    (if-not captured-pawn
      game
      (-> game
          (dissoc-square-attribute :pawn location)
          (add-to-captures player-id captured-pawn)))))


(defn change-pawn-position
  "Move pawn from given player from given square to given destination.
  Note that this function would replace any pawn present on the destination.
  To be precise, this function removes the pawn at from and place one of given player's pawn at the destination"
  {:test (fn []
           (let [game (-> (create-game :board [[{:controlled-by "p1" :pawn "p1"} 0]])
                          (change-pawn-position "p1" [0 0] [0 1]))]
             (is-not (-> (get-square game [0 0])
                         (contains? :pawn)))
             (is= (get-square-attribute game :pawn [0 1])
                  "p1")))}
  [game player-id from to]
  (-> game
      (dissoc-square-attribute :pawn from)
      (update-square :pawn to player-id)))
