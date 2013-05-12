(ns bang-spec
  (:require [speclj.core :refer :all]
            [bang :as b]))

(describe "Circular hit detection"
          (it "Close circles should hit"
              (should (b/things-hit? {:position [0 0] :size 5}
                                     {:position [3 3] :size 1})))
          (it "Far circles should not hit"
              (should-not (b/things-hit? {:position [0 0] :size 5}
                                     {:position [10 10] :size 1}))))

(describe "Moving things"
          (it "Moves 10 units across the x axis"
              (should= {:position [10.0 0.0] :speed 10}
                       (b/move-thing {:position [0 0] :speed 10} 0)))
 
          (it "Moves 30 units across the x axis and 30 units up the y axis"
              (should= {:position [30.0 30.0] :speed 10}
                       (-> {:position [0 0] :speed 10}
                           (b/move-thing 0)
                           (b/move-thing 90)
                           (b/move-thing 0)
                           (b/move-thing 90)
                           (b/move-thing 0)
                           (b/move-thing 90)))))

(def boss-phases [{         :to 20 :weapon {:damage 1  :speed 10}}
                  {:from 20 :to 40 :weapon {:damage 10 :speed 2}}
                  {:from 40        :weapon {:damage 5  :speed 6}}])

(describe "Boss phase"
          (it "Life total within phase"
              (should (b/life-within-phase? 10 (first boss-phases)))
              (should-not (b/life-within-phase? 10 (second boss-phases)))
              (should-not (b/life-within-phase? 10 (last boss-phases)))
              
              (should-not (b/life-within-phase? 30 (first boss-phases)))
              (should (b/life-within-phase? 30 (second boss-phases)))
              (should-not (b/life-within-phase? 30 (last boss-phases)))
              
              (should-not (b/life-within-phase? 50 (first boss-phases)))
              (should-not (b/life-within-phase? 50 (second boss-phases)))
              (should (b/life-within-phase? 50 (last boss-phases))))
          
          (it "Boss phase depends on life total"
              (should= (first boss-phases) (b/boss-phase {:life 10 :phases boss-phases}))
              (should= (second boss-phases) (b/boss-phase {:life 30 :phases boss-phases}))
              (should= (last boss-phases) (b/boss-phase {:life 50 :phases boss-phases})))) 
  
(describe "Character's current weapon"
          (it "Non-boss characters have one weapon"
              (let [weapon {:damage 3 :speed 5}]
                (should= weapon
                         (b/current-weapon {:weapon weapon}))))
          (it "Boss weapon depends on boss' phase"
              (should= (:weapon (first boss-phases))
                       (b/current-weapon {:type :boss :life 10 :phases boss-phases}))
              (should= (:weapon (second boss-phases))
                       (b/current-weapon {:type :boss :life 30 :phases boss-phases}))
              (should= (:weapon (last boss-phases))
                       (b/current-weapon {:type :boss :life 50 :phases boss-phases}))))

(describe "Firing weapons"
          (it "Player fires a weapon"
              (should= {:type :bullet
                        :owner-type :player
                        :damage 3
                        :speed 5
                        :position [0 0]
                        :direction 0}
                       (let [player {:type :player
                                     :weapon {:damage 3 :speed 5}
                                     :position [0 0]}]
                         (b/character-fires-bullet-in-direction player 0))))
          (it "Enemy fires a weapon"
              (should= {:type :bullet
                        :owner-type :enemy
                        :damage 1
                        :speed 2
                        :position [0 0]
                        :direction 90}
                       (let [enemy {:type :enemy
                                    :weapon {:damage 1 :speed 2}
                                    :position [0 0]}]
                         (b/character-fires-bullet-in-direction enemy 90))))
          (it "Boss fires weapon"
              (should= {:type :bullet
                        :owner-type :boss
                        :damage 10
                        :speed 2
                        :position [0 0]
                        :direction 90}
                       (let [boss {:type :boss
                                   :life 30
                                   :phases boss-phases
                                   :position [0 0]}]
                         (b/character-fires-bullet-in-direction boss 90)))
              (should= {:type :bullet
                        :owner-type :boss
                        :damage 1
                        :speed 10
                        :position [0 0]
                        :direction 90}
                       (let [boss {:type :boss
                                   :life 10
                                   :phases boss-phases
                                   :position [0 0]}]
                         (b/character-fires-bullet-in-direction boss 90)))))

(describe "Dealing damage"
          (it "Damage reduces life."
              (should= 10
                       (b/deal-damage 5 15)))
          (it "Damage can't reduce life lower than zero"
              (should= 0
                       (b/deal-damage 2 1)))
          (it "Damaging character reduces character's life."
              (should= 2
                       (:life (b/damage-character {:life 5} 3))))
          (it "Don't allow friendly fire"
              (let [enemy1 {:type :enemy
                            :weapon {:damage 3 :speed 5}
                            :position [0 0]}
                    bullet (b/character-fires-bullet-in-direction enemy1 0)
                    enemy2 {:type :enemy}]
                (should= enemy2 (b/bullet-hit-character bullet enemy2))))
          (it "Rivals damage each other"
              (let [rival1 {:type :player
                            :weapon {:damage 3 :speed 5}
                            :position [0 0]}
                    bullet (b/character-fires-bullet-in-direction rival1 0)
                    rival2 {:type :enemy
                            :life 10}
                    damaged-rival2 (b/bullet-hit-character bullet rival2)]
                (should= 7
                         (:life damaged-rival2))
                (should-not= rival2 damaged-rival2))))

(describe "Character status"
          (it "Character with positive life isn't dead"
              (should-not= :dead
                           (:status (b/update-character-status {:life 1}))))
          (it "Character with zero life is dead"
              (should= :dead
                       (:status (b/update-character-status {:life 0})))))

(describe "Process game step"
          (it "Stepping the game increases the step counter"
              (should= 1
                       (:step (b/process-game-step {:step 0})))))

(run-specs) 
