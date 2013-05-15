(ns football.test.engine
  (:require [football.engine :as engine]
            [football.rules  :as rules])
  (:use     [clojure.test]))

;;*****************************************************
;; Game definition Tests
;;*****************************************************

(deftest new-game
  (let [game (engine/new-game)]
    (is (= 1 (:quarter game)))
    (is (= rules/time-in-quarter (:quarter-remaining game)))))


;;*****************************************************
;; Transition Tests
;;*****************************************************

(deftest transition-to-kickoff-mode
  (let [game (engine/new-game)]
    (is (= :kickoff (:game-mode game)))
    (is (= rules/yard-line-at-kickoff (:yard-line game))))
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 2)
                 (assoc :quarter-remaining 1))
        outcome {:duration 10 :yards-diff 0 :playtype :normal}
        updated (engine/update-game game outcome)]
    (is (= 3 (:quarter updated)))
    (is (= :kickoff (:game-mode updated)))
    (is (= rules/yard-line-at-kickoff (:yard-line updated)))))

(deftest transition-to-first-down
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 1)
                 (assoc :down 3)
                 (assoc :togo 9)
                 (assoc :yard-line 20)
                 (assoc :poss :home))]
    (let [outcome {:duration 10 :yards-diff 9 :playtype :normal}
          updated (engine/update-game game outcome)]
      (is (= 1  (:down updated)))
      (is (= 10 (:togo updated)))
      (is (= :normal (:game-mode updated)))
      (is (= 11 (:yard-line updated)))
      (is (= :home (:poss updated))))
   (let [outcome {:duration 10 :yards-diff 8 :playtype :normal}
         updated (engine/update-game game outcome)]
      (is (= 4  (:down updated)))
      (is (= 1  (:togo updated)))
      (is (= :normal (:game-mode updated)))
      (is (= 12 (:yard-line updated)))
      (is (= :home (:poss updated))))))

(deftest transition-to-first-down2
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 1)
                 (assoc :down 2)
                 (assoc :togo 4)
                 (assoc :yard-line 74))]
    (let [outcome {:duration 10 :yards-diff 8 :playtype :normal}
          updated (engine/update-game game outcome)]
      (is (= 1  (:down updated)))
      (is (= 10 (:togo updated)))
      (is (= :normal (:game-mode updated))))))

(deftest transition-to-quarter-end
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 1)
                 (assoc :quarter-remaining 4))]
    (let [outcome {:duration 10 :yards-diff 1 :playtype :normal}
          updated (engine/update-game game outcome)]
      (is (= false (:completed updated)))
      (is (= 2 (:quarter updated)))
      (is (= rules/time-in-quarter (:quarter-remaining updated))))))

(deftest transition-to-completed
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 4)
                 (assoc :quarter-remaining 4))]
    (let [outcome {:duration 10 :yards-diff 1 :playtype :normal}
          updated (engine/update-game game outcome)]
      (is (= true (:completed updated)))
      (is (= 4 (:quarter updated)))
      (is (= 0 (:quarter-remaining updated))))))

(deftest transition-to-touchdown
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 4)
                 (assoc :down 4)
                 (assoc :togo 2)
                 (assoc :yard-line 2)
                 (assoc :poss :home)
                 (assoc :home-score 0)
                 (assoc :away-score 0))]
    (let [outcome {:duration 10 :yards-diff 2 :playtype :normal}
          updated (engine/update-game game outcome)]
      (is (= 6 (:home-score updated)))
      (is (= 0 (:away-score updated)))
      (is (= 6 (:scorediff updated)))
      (is (= :home (:poss updated)))
      ;(is (= 0 (:down updated)))
      (is (= rules/yard-line-at-extrapoint (:yard-line updated)))
      (is (= :extrapoint (:game-mode updated))))
    (let [outcome {:duration 10 :yards-diff 1 :playtype :normal}
          updated (engine/update-game game outcome)]
      (is (= 0 (:home-score updated)))
      (is (= 0 (:away-score updated)))
      (is (= 0 (:scorediff updated)))
      (is (= :away (:poss updated)))
      (is (= 99 (:yard-line updated))))
    (let [outcome {:duration 10 :yards-diff -99 :playtype :normal}
          updated (engine/update-game game outcome)]
      ;; interception for touchdown
      (is (= 0 (:home-score updated)))
      (is (= 6 (:away-score updated)))
      (is (= 6 (:scorediff updated)))
      (is (= :away (:poss updated)))
      ;(is (= 0 (:down updated)))
      (is (= rules/yard-line-at-extrapoint (:yard-line updated)))
      (is (= :extrapoint (:game-mode updated))))))

(deftest transition-at-fieldgoal
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 1)
                 (assoc :down 4)
                 (assoc :togo 10)
                 (assoc :yard-line 20)
                 (assoc :poss :home)
                 (assoc :home-score 0)
                 (assoc :away-score 0))]
    (let [outcome {:duration 10 :kick-good true :playtype :fieldgoal}
          updated (engine/update-game game outcome)]
      (is (= 3 (:home-score updated)))
      (is (= 0 (:away-score updated)))
      (is (= 3 (:scorediff updated)))
      (is (= rules/yard-line-at-kickoff (:yard-line updated)))
      (is (= 0 (:down updated)))
      (is (= :kickoff (:game-mode updated)))
      (is (= :home (:poss updated))))
    (let [outcome {:duration 10 :kick-good false :playtype :fieldgoal}
          updated (engine/update-game game outcome)]
      (is (= 0 (:home-score updated)))
      (is (= 0 (:away-score updated)))
      (is (= 0 (:scorediff updated)))
      (is (= 80 (:yard-line updated)))
      (is (= 1 (:down updated)))
      (is (= :normal (:game-mode updated)))
      (is (= :away (:poss updated))))))

(deftest transition-at-punt
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :poss :away)
                 (assoc :quarter 1)
                 (assoc :togo 10)
                 (assoc :yard-line 60)
                 (assoc :down 4))]
    (let [outcome {:duration 10 :yards-diff 30 :playtype :punt}
          updated (engine/update-game game outcome)]
      (is (= :home (:poss updated)))
      (is (= 1 (:down updated)))
      (is (= 70 (:yard-line updated))))))


;;*****************************************************
;; Game mode tests
;;*****************************************************

(deftest kickoff-mode
  (let [game (-> (engine/new-game)
                 (assoc :yard-line 80)
                 (assoc :poss :home))]
    (let [outcome {:duration 10 :yards-diff 75 :playtype :kickoff}
          updated (engine/update-game game outcome)]
      (is (= :away (:poss updated)))
      (is (= :normal (:game-mode updated)))
      (is (= 95 (:yard-line updated))))
    (let [outcome {:duration 10 :yards-diff 85 :playtype :kickoff}
          updated (engine/update-game game outcome)]
      (is (= :away (:poss updated)))
      (is (= :normal (:game-mode updated)))
      (is (= rules/yard-line-at-touchback (:yard-line updated))))))

(deftest extrapoint-mode
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :extrapoint)
                 (assoc :poss :home)
                 (assoc :home-score 6)
                 (assoc :away-score 0)
                 (assoc :quarter 1)
                 (assoc :quarter-remaining 2))]
    (let [outcome {:duration 10 :kick-good true :playtype :fieldgoal}
          updated (engine/update-game game outcome)]
      (is (= :kickoff (:game-mode updated)))
      (is (= rules/yard-line-at-kickoff (:yard-line updated)))
      (is (= 7 (:home-score updated)))
      (is (= 0 (:away-score updated)))
      (is (= :home (:poss updated))))
    (let [outcome {:duration 10 :kick-good false :playtype :fieldgoal}
          updated (engine/update-game game outcome)]
      (is (= :kickoff (:game-mode updated)))
      (is (= rules/yard-line-at-kickoff (:yard-line updated)))
      (is (= 6 (:home-score updated)))
      (is (= 0 (:away-score updated)))
      (is (= :home (:poss updated))))))


;;*****************************************************
;; Duration tests
;;*****************************************************

(deftest normal-duration
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 2)
                 (assoc :quarter-remaining 200))]
    (let [outcome {:duration 40 :yards-diff 0 :playtype :normal}
          updated (engine/update-game game outcome)]
      (is (= 160 (:quarter-remaining updated))))))


;;*****************************************************
;; Miscellaneous tests
;;*****************************************************

(deftest sim-play
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 1)
                 (assoc :down 2)
                 (assoc :togo 10))]
    (let [off-play {:type :rush :yardsdiff 11 :playtype :normal}
          def-play {}
          outcome (engine/sim-play game off-play def-play)]
      (is (= :normal (:game-mode outcome)))
      (is (= :normal (:playtype outcome))))))


;;*****************************************************
;; Football rules tests
;;*****************************************************

(deftest use-remaining-yards-as-togo-and-not-10
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :down 4)
                 (assoc :poss :home)
                 (assoc :togo 9))]
    (let [game (assoc game :yard-line 99)
          outcome {:duration 10 :yards-diff 1 :playtype :normal}
          updated (engine/update-game game outcome)]
      (is (= 1 (:down updated)))
      (is (= 2 (:togo updated)))
      (is (= :away (:poss updated)))
      (is (= :normal (:game-mode updated))))
    (let [game (assoc game :yard-line 15)
          outcome {:duration 10 :yards-diff 11 :playtype :normal}
          updated (engine/update-game game outcome)]
      (is (= 1 (:down updated)))
      (is (= 4 (:togo updated)))
      (is (= :home (:poss updated)))
      (is (= :normal (:game-mode updated))))))

(deftest touchback-if-punt-over-endzone
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :down 4)
                 (assoc :poss :home)
                 (assoc :home-score 0)
                 (assoc :away-score 0)
                 (assoc :yard-line 50))]
    (let [outcome {:duration 10 :yards-diff 55 :playtype :punt}
          updated (engine/update-game game outcome)]
      (is (= 0 (:home-score updated)))
      (is (= 0 (:away-score updated)))
      (is (= :away (:poss updated)))
      (is (= rules/yard-line-at-touchback (:yard-line updated))))))
