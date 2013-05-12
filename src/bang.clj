(ns bang
  (:import [java.lang Math Integer]))

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
    {:type (:type character)
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
  (if (not= (:type bullet) (:type character))
    (damage-character character (:damage bullet))
    character))

(defn update-character-status
  [character]
  (when (zero? (:life character))
    (assoc character :status :dead)))
