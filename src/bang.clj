(ns bang
  (:import [java.lang Math Integer]))

;;; Circular hit detection

(defn things-hit?
  [this that]
  (let [this-pos (:position this)
        that-pos (:position that)
        diff-x (- (first this-pos) (first that-pos))
        diff-y (- (second this-pos) (second that-pos))
        radii (+ (:size this) (:size that))]
    ;; thanks, Pythagoras!
    (< (+ (* diff-x diff-x)
          (* diff-y diff-y))
       (* radii radii))))

;;; Movement of entities - characters and bullets

(defn cos [θ]
  (let [c (Math/cos θ)]
    (if (#{1.0 -1.0} c)
      c
      (if (#{1.0 -1.0} (Math/sin θ))
        0.0
        c))))

(defn move
  [[x y] speed direction]
  (let [direction-radians (Math/toRadians direction)]
    [(+ x (* speed (cos direction-radians)))
     (+ y (* speed (Math/sin direction-radians)))]))

(defn move-thing
  [thing]
  (update-in thing [:position] move (:speed thing) (:direction thing)))

;; Boss phase, determined by current life total

(defn life-within-phase?
  [life phase]
  (let [from (:from phase 0)]
    (if-let [to (:to phase nil)]
      (contains? (set (range from (inc to))) life) 
      (> life from))))

(defn boss-phase
  [boss]
  (first (filter (partial life-within-phase? (:life boss))
                 (:phases boss))))

;; Using weapon

(defn current-weapon
  [character]
  (case (:type character)
    :boss (:weapon (boss-phase character))
    (:weapon character)))

(defn character-fires-bullet-in-direction
  [character direction]
  (let [weapon (current-weapon character)]
    {:type :bullet
     :owner-type (:type character)
     :damage (:damage weapon)
     :speed (:speed weapon)
     :position (:position character)
     :direction direction}))

;; Dealing damage

(defn deal-damage
  [damage life]
  (let [life (- life damage)]
    (if (< life 0)
      0
      life)))

(defn damage-character
  [character damage]
  (update-in character [:life] (partial deal-damage damage)))

(defn bullet-hit-character
  [bullet character]
  (if (not= (:owner-type bullet) (:type character))
    (damage-character character (:damage bullet))
    character))

(defn update-character-status
  [character]
  (when (zero? (:life character))
    (assoc character :status :dead)))

(defn things-of-type
  [things type]
  (filter #(= type (:type %)) things))

(defn game-ended?
  [things]
  (or (empty? (things-of-type things :player))
      (empty? (things-of-type things :enemy))))

;; Game loop

(defn process-game-step
  ([world]
     (process-game-step world nil))
  ([world rules]
     (let [world (update-in world [:step] inc)
           world (if (fn? rules)
                   (rules world)
                   world)]
       world)))

(defn check-collisions
  [things]
  (let [things-of-type (partial things-of-type things)]
    (for [bullet (things-of-type :bullet)]
      (let [targets (case (:owner-type bullet)
                      :player (concat (things-of-type :enemy)
                                      (things-of-type :boss))
                      :enemy (things-of-type :player))]
        (map (partial things-hit? bullet) targets)))))

(defn step-game
  [world rules]
  (Thread/sleep 1000)
  (println (:step world))
  (when-not (game-ended? (:things world))
    (let [new-world (process-game-step world rules)]
      (step-game new-world rules))))

(defn start-game!
  [world rules]
  (step-game world rules))

(comment
  (.start (Thread. stop-game!))

  ;; Simple game where player kills enemy
  (start-game! (let [player {:type :player :life 50 :weapon {:damage 4 :speed 5} :position [0 0]}]
                 {:step 0
                  :things [player
                           {:type :enemy :life 10 :position [50 0]}
                           (character-fires-bullet-in-direction player 0)]})
               (fn [world]
                 (-> world
                     (update-in [:things] (partial map move-thing))
                     (update-in [:things] (partial map check-collisions)))))
  )
