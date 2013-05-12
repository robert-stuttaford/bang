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

(defn move
  [[x y] speed direction]
  (let [direction-radians (Math/toRadians direction)]
    [(+ x (* speed (Math/cos direction-radians)))
     (+ y (* speed (Math/sin direction-radians)))]))

(defn move-thing
  [thing direction]
  (update-in thing [:position] move (:speed thing) direction))

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

(declare running)

(defn step-game
  [world rules]
  (Thread/sleep 1000)
  (when @running
    (let [new-world (process-game-step world rules)]
      (println world)
      (step-game new-world rules))))

(defn start-game!
  [world rules]
  (binding [running (atom true)]
    (reset! running true)
    (step-game world rules)))

(defn stop-game!
  []
  (when (= clojure.lang.Atom running)
    (reset! running false)))

(comment
  (stop-game!)

  ;; Simple game where player kills enemy
  (start-game! {:step 0
                :things [{:type :player :life 50 :weapon {:damage 4 :speed 5} :position [0 0]}
                         {:type :enemy :life 10 :position [50 0]}]}
               (fn [world]
                 (let [player (first (filter #(= :player (:type %))
                                             (:things world)))
                       bullets (filter #(= :bullet (:type %))
                                       (:things world))]
                   (-> world
                       (when (empty? bullets)
                         (update-in world [:things] conj (character-fires-bullet-in-direction player 0)))))))
  )
