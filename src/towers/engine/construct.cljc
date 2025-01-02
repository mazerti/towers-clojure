(ns towers.engine.construct
  "A namespace for the non-gameplay related building blocks of the game."
  (:require [ysera.test :refer [is is=]]))


(def default-settings
  "A map listing the default settings of a game."
  {:dimensions       [3 3]
   :player-ids       ["p1" "p2" "p3"]
   :number-of-towers 50
   :number-of-pawns  6})


(defn create-square
  "Create a square on the board with given attributes."
  {:test (fn []
           (is= (create-square [0 1] :height 2 :pawn "p2")
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
                {:board             {[0 0] {:location [0 0] :height 0}
                                     [0 1] {:location [0 1] :height 0}
                                     [0 2] {:location [0 2] :height 0}
                                     [1 0] {:location [1 0] :height 0}
                                     [1 1] {:location [1 1] :height 0}
                                     [1 2] {:location [1 2] :height 0}
                                     [2 0] {:location [2 0] :height 0}
                                     [2 1] {:location [2 1] :height 0}
                                     [2 2] {:location [2 2] :height 0}}
                 :players           [{:id "p1" :pawns 6}
                                     {:id "p2" :pawns 6}
                                     {:id "p3" :pawns 6}]
                 :player-id-in-turn "p1"
                 :unbuilt-towers    50
                 :phase             :beginning})
           ; Custom settings
           (is= (create-empty-game :dimensions 2
                                   :player-ids [:a "b"]
                                   :number-of-towers 30
                                   :number-of-pawns 5)
                {:board             {[0 0] {:location [0 0] :height 0}
                                     [0 1] {:location [0 1] :height 0}
                                     [1 0] {:location [1 0] :height 0}
                                     [1 1] {:location [1 1] :height 0}}
                 :players           [{:id :a :pawns 5}
                                     {:id "b" :pawns 5}]
                 :player-id-in-turn :a
                 :unbuilt-towers    30
                 :phase             :beginning}))}
  [& settings]
  (let [settings (as-> default-settings $
                       (cond
                         (nil? settings) $
                         (and (= (count settings) 1)
                              (map? (first settings))) (into $ settings)
                         :else (apply assoc $ settings))
                       (if-not (number? (:dimensions $)) $
                                                         (update $ :dimensions (fn [x] [x x]))))]
    {:board             (->> (for [i (range (-> (:dimensions settings)
                                                (first)))
                                   j (range (-> (:dimensions settings)
                                                (second)))]
                               {[i j] (create-square [i j])})
                             (into {}))
     :players           (->> (for [player-id (:player-ids settings)]
                               {:id player-id :pawns (:number-of-pawns settings)})
                             (into []))
     :player-id-in-turn (-> (:player-ids settings)
                            (first))
     :unbuilt-towers    (:number-of-towers settings)
     :phase             :beginning}))


(defn get-dimensions
  "Returns the game's board dimensions."
  {:test (fn []
           (is= (-> (create-empty-game :dimensions [2 5])
                    (get-dimensions))
                [2 5]))}
  [game]
  (->> (:board game)
       (keys)
       (reduce (fn [[max-x max-y] [x y]] [(max max-x (inc x)) ; inc because locations are indexed from 0
                                          (max max-y (inc y))]) ; inc because locations are indexed from 0
               [1 1])))


(defn preview-board
  "Returns a simple view of the board."
  {:test (fn []
           (is= (-> (create-empty-game :dimensions [1 2])
                    (assoc :board {[0 0] {:location [0 0] :height 1}
                                   [0 1] {:location [0 1] :height 3}})
                    (preview-board))
                [[1 3]]))}
  [game]
  (->> (:board game)
       (vals)
       (sort-by :location)
       (map :height)
       (partition (second (get-dimensions game)))
       (map vec)
       (vec)))


(defn square-preview->game-square
  "Return a square whose format matches the one used to store a game.
  Args can take the shape of either a number to represent the height or a map of key-values that will be passed on
  to the square."
  {:test (fn []
           (is= (square-preview->game-square [1 2] 3)
                (create-square [1 2] :height 3))
           (is= (square-preview->game-square [1 2] {:height 4})
                (create-square [1 2] :height 4))
           (is= (square-preview->game-square [1 2] {:non-existing-key :a})
                (create-square [1 2] :height 0 :non-existing-key :a))
           )}
  [location args]
  (cond
    (number? args) (create-square location :height args)
    (map? args) (-> (create-square location :height 0)
                    (into args))))


(defn board-preview->game-board
  "Return a board whose format matches the one used to store a game."
  {:test (fn []
           (is= (-> [[1 3 2]
                     [0 {:height 4 :pawn "p1"} 0]]
                    (board-preview->game-board))
                {[0 0] {:location [0 0] :height 1}
                 [0 1] {:location [0 1] :height 3}
                 [0 2] {:location [0 2] :height 2}
                 [1 0] {:location [1 0] :height 0}
                 [1 1] {:location [1 1] :height 4 :pawn "p1"}
                 [1 2] {:location [1 2] :height 0}}))}
  [preview]
  (->> (for [[i row] (map-indexed vector preview)
             [j val] (map-indexed vector row)]
         {[i j] (square-preview->game-square [i j] val)})
       (into {})))


(defn replace-board
  "Update the game's board to match the given one.
  Make sure that given board's dimensions matches the game's ones."
  {:test (fn []
           (is= (-> (create-empty-game)
                    (replace-board [[1 2 3]
                                    [2 0 1]
                                    [3 0 2]])
                    (preview-board))
                [[1 2 3]
                 [2 0 1]
                 [3 0 2]]))}
  [game board]
  (assoc game :board (board-preview->game-board board)))


(defn board-dimensions
  "Returns the dimensions of a board on input format."
  {:test (fn []
           (is= (board-dimensions [[1 0 2]
                                   [2 1 2]])
                [2 3])
           (is= (board-dimensions [[1]
                                   [2 1 2]])
                [2 3])
           )}
  [board]
  [(count board)
   (apply max (map count board))])


(defn create-game
  "Create a game instance matching the input setup."
  {:test (fn []
           ; default game
           (is= (create-game)
                (create-empty-game))
           ; changing arguments
           (is= (create-game :board [[2 0 1]
                                     [0 1 2]
                                     [1 0 1]]
                             :players [{:id "pa" :pawns 4 :non-existing-key :a}
                                       {:id "pb" :pawns 5}
                                       {:id "pc"}]
                             :unbuilt-towers 40
                             :phase :core
                             :last-action {:action        :spawn-pawn
                                           :pawn-location [0 1]})
                {:board             {[0 0] {:location [0 0] :height 2}
                                     [0 1] {:location [0 1] :height 0}
                                     [0 2] {:location [0 2] :height 1}
                                     [1 0] {:location [1 0] :height 0}
                                     [1 1] {:location [1 1] :height 1}
                                     [1 2] {:location [1 2] :height 2}
                                     [2 0] {:location [2 0] :height 1}
                                     [2 1] {:location [2 1] :height 0}
                                     [2 2] {:location [2 2] :height 1}}
                 :players           [{:id "pa" :pawns 4 :non-existing-key :a}
                                     {:id "pb" :pawns 5}
                                     {:id "pc" :pawns 6}]
                 :player-id-in-turn "pa"
                 :unbuilt-towers    40
                 :phase             :core
                 :last-action       {:action        :spawn-pawn
                                     :pawn-location [0 1]}})
           ; changing settings
           (is= (create-game :settings {:dimensions       1
                                        :player-ids       ["a" "b"]
                                        :number-of-pawns  2
                                        :number-of-towers 12})
                {:board             {[0 0] {:location [0 0] :height 0}}
                 :players           [{:id "a" :pawns 2}
                                     {:id "b" :pawns 2}]
                 :player-id-in-turn "a"
                 :unbuilt-towers    12
                 :phase             :beginning}))}
  [& args]
  (let [{:keys [settings board players] :as kvs} (if args (apply assoc {} args) {})
        other-kvs (apply dissoc kvs [:settings :board :players])
        settings (as-> settings $
                       (into {} $)
                       (if board (assoc $ :dimensions (board-dimensions board)) $)
                       (if players (assoc $ :player-ids (map :id players)) $))]
    (as-> (create-empty-game settings) $
          (reduce (fn [acc [k v]] (assoc acc k v)) $ other-kvs)
          (cond-> $
                  board (replace-board board)
                  players (update :players (fn [base-players]
                                             (reduce (fn [players [index player]]
                                                       (update players index (fn [base] (into base player))))
                                                     base-players
                                                     (map-indexed vector players))))))))


(defn get-square
  "Returns the square at given location."
  {:test (fn []
           (is= (-> (create-game :board [[2 0]
                                         [1 3]])
                    (get-square [1 0])
                    (:location))
                [1 0]))}
  [game location]
  (-> game
      (:board)
      (get location)))


(defn get-square-attribute
  "Returns the asked attribute of the square at given location."
  {:test (fn []
           (is= (-> (create-game :board [[2 0]
                                         [1 1]])
                    (get-square-attribute :height [0 0]))
                2))}
  [game attribute location]
  (-> game
      (get-square location)
      (attribute)))


(defn replace-square
  "Update given square on the board."
  {:test (fn []
           (is= (-> (create-game)
                    (replace-square (create-square [1 2] :height 2 :non-existing-key :a))
                    (get-square [1 2]))
                {:location         [1 2]
                 :height           2
                 :non-existing-key :a}))}
  [game new-square]
  (let [location (:location new-square)]
    (assoc-in game [:board location] new-square)))

(defn update-square
  "Update given attribute of given square."
  {:test (fn []
           (let [game (create-game)]
             (is= (-> (update-square game :height [1 2] inc)
                      (get-square-attribute :height [1 2]))
                  1)
             (is= (-> (update-square game :height [2 1] + 2)
                      (get-square-attribute :height [2 1]))
                  2)
             (is= (-> (update-square game :height [1 0] 3)
                      (get-square-attribute :height [1 0]))
                  3)
             (is= (-> (update-square game :non-existing-key [1 0] :a)
                      (get-square-attribute :non-existing-key [1 0]))
                  :a)
             ))}
  [game attribute location function-or-val & args]
  (if (fn? function-or-val)
    (apply update-in game [:board location attribute] function-or-val args)
    (assoc-in game [:board location attribute] function-or-val)))


(defn get-player-ids
  "Return the list of all player ids from a game."
  {:test (fn []
           (is= (-> (create-game :settings {:player-ids ["a" "b" "c"]})
                    (get-player-ids))
                ["a" "b" "c"]))}
  [game]
  (->> (:players game)
       (map :id)
       (vec)))


(defn get-player
  "Return the information of the given player."
  {:test (fn []
           (is= (-> (create-game)
                    (get-player "p2"))
                {:id "p2" :pawns 6}))}
  [game player-id]
  (->> game
       (:players)
       (filter (fn [x] (= (:id x) player-id)))
       (first)))


(defn update-player
  "Update given attribute of given player."
  {:test (fn []
           (is= (-> (create-game)
                    (update-player :non-existing-key "p2" :a)
                    (get-player "p2"))
                {:id "p2" :pawns 6 :non-existing-key :a})
           (is= (-> (create-game)
                    (update-player :pawns "p2" dec)
                    (get-player "p2"))
                {:id "p2" :pawns 5})
           (is= (-> (create-game)
                    (update-player :pawns "p2" + 2)
                    (get-player "p2"))
                {:id "p2" :pawns 8})
           )}
  [game attribute player-id function-or-val & args]
  (update game :players
          (fn [players]
            (map (fn [player]
                   (if (= (:id player) player-id)
                     (cond
                       (fn? function-or-val) (apply update player attribute function-or-val args)
                       :value (assoc player attribute function-or-val))
                     player))
                 players))))


(defn apply-to-all-players
  "Apply given function to all the players in the game."
  {:test (fn []
           (is= (as-> (create-game) $
                      (apply-to-all-players $ (fn [x] (assoc x :non-existing-key :a)))
                      (:players $)
                      (map :non-existing-key $))
                [:a :a :a])
           (is (as-> (create-game :players [{:id "p1" :non-existing-key :a}
                                            {:id "p2" :non-existing-key :b}]) $
                     (apply-to-all-players $ (fn [x] (dissoc x :non-existing-key)))
                     (:players $)
                     (map keys $)
                     (reduce into $)
                     (filter (fn [x] (= x :non-existing-key)) $)
                     (empty? $)))
           )}
  [game function]
  (update game :players (partial map function)))
