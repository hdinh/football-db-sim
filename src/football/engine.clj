(ns football.engine
  (:require [football.rules :as rules]
            [football.ai    :as ai]))

(defn new-game []
  {:quarter 1
   :down 1
   :togo 10
   :yard-line rules/yard-line-at-kickoff
   :poss :home
   :home-score 0
   :away-score 0
   :scorediff 0
   :quarter-remaining rules/time-in-quarter
   :play-count 0
   :game-mode :kickoff
   :completed false})

(defn completed? [game]
  (= (:completed game) true))

(defn- other-poss [poss]
  (case poss
    :away :home
    :home :away))

(defn- make-relative-to-next-poss [next-poss prev-poss yard-line]
  (if (= next-poss prev-poss)
    yard-line
    (- 100 yard-line)))

(defprotocol ModeHandler
  (get-yard-line-after [this game outcome])
  (get-down-changes [this game outcome])
  (get-score-and-mode [this game outcome yard-line-after]))

(defrecord NormalModeHandler [] ModeHandler
  (get-yard-line-after [this game outcome]
    (let [after-play (fn [] (- (:yard-line game) (:yards-diff outcome)))]
      (case (:playtype outcome)
        :fieldgoal
          (:yard-line game)
        :punt
          (if (<= (after-play) 0)
            (- 100 rules/yard-line-at-touchback)
            (after-play))
        (after-play))))
  (get-down-changes [this game outcome]
    (case (:playtype outcome)
      :normal
        (let [next-down-off (if (>= (:yards-diff outcome) (:togo game)) 1 (inc (:down game)))
              next-poss (if (<= next-down-off 4) (:poss game) (other-poss (:poss game)))
              next-down-norm (if (= (:poss game) next-poss) next-down-off 1)]
           {:next-down next-down-norm
            :next-poss next-poss
            :first-down (= 1 next-down-norm)})
      :fieldgoal
        {:next-poss (:poss game)
         :first-down true}
      :punt
        {:next-poss (other-poss (:poss game))
         :next-down 1
         :first-down true}))
  (get-score-and-mode [this game outcome yard-line-after]
    (cond
      (= :fieldgoal (:playtype outcome))
        (if (:kick-good outcome)
          {:score-team (:poss game)
           :score-type :fieldgoal
           :next-game-mode :kickoff
           :yard-line-after rules/yard-line-at-kickoff
           :next-down 0}
          {:next-game-mode :normal
           :next-poss (other-poss (:poss game))
           :next-down 1})
      (<= yard-line-after 0)
        {:score-team (:poss game)
         :score-type :touchdown
         :next-game-mode :extrapoint
         :yard-line-after rules/yard-line-at-extrapoint}
      (>= yard-line-after 100)
        {:score-team (other-poss (:poss game))
         :score-type :touchdown
         :next-game-mode :extrapoint
         :next-poss (other-poss (:poss game))
         :yard-line-after (- 100 rules/yard-line-at-extrapoint)}
      :default
        {:next-game-mode :normal})))

(defrecord KickoffModeHandler [] ModeHandler
  (get-yard-line-after [this game outcome]
    (- (:yard-line game) (:yards-diff outcome)))
  (get-down-changes [this game outcome]
    {:next-down 1
     :next-poss (other-poss (:poss game))
     :first-down true})
  (get-score-and-mode [this game outcome yard-line-after]
    (cond
      (<= yard-line-after 0)
        ;; TODO - this doesn't handle fumble and touchdown
        {:yard-line-after (- 100 rules/yard-line-at-touchback)
         :next-game-mode :normal}
      (>= yard-line-after 100)
        {:score-team (:poss game)
         :score-type :touchdown
         :next-game-mode :extrapoint}
      :default
        {:next-game-mode :normal})))

(defrecord ExtrapointModeHandler [] ModeHandler
  (get-yard-line-after [this game outcome]
    rules/yard-line-at-kickoff)
  (get-down-changes [this game outcome]
    {:next-down 0
     :next-poss (:poss game)
     :first-down false})
  (get-score-and-mode [this game outcome yard-line-after]
    (let [scoring (if (= :fieldgoal (:playtype outcome))
                    (if (:kick-good outcome)
                      {:score-team (:poss game)
                       :score-type :extrapoint})
                      {})]
      (assoc scoring :next-game-mode :kickoff))))


;;*****************************************************
;; Game processing
;;*****************************************************

(defn- update-time [outcome game]
  (let [time-after (- (:quarter-remaining game) (:duration outcome))
        q (:quarter game)
        next-quarter (if (<= time-after 0)
                       (if (= 4 q) 4 (inc q))
                       q)
        maybe-second-half (if (and (= 2 q) (= 3 next-quarter))
                            {:next-game-mode :kickoff
                             :yard-line rules/yard-line-at-kickoff})]
    (-> outcome
        (conj maybe-second-half)
        (assoc :next-quarter next-quarter)
        (assoc :next-quarter-remaining (if (<= time-after 0)
                                         (if (= 4 q) 0 rules/time-in-quarter)
                                         time-after))
        (assoc :game-completed (if (<= time-after 0)
                                 (if (= 4 q) true false)
                                 false)))))

(defn- update-next-yard-line-norm [outcome game]
  (let [x (make-relative-to-next-poss (:poss game) (:next-poss outcome) (:yard-line-after outcome))]
    (assoc outcome :next-yard-line-norm x)))

(defn- update-scores [outcome game]
  (letfn [(f [team]
            (if (= (:score-team outcome) team)
              (rules/get-score (:score-type outcome))
              0))]
    (-> outcome
        (assoc :next-home-score (+ (:home-score game) (f :home)))
        (assoc :next-away-score (+ (:away-score game) (f :away))))))

(defn- update-score-diff [outcome game]
  (let [home (:next-home-score outcome)
        away (:next-away-score outcome)
        next (:next-poss outcome)
        scorediff (* (- home away) (if (= next :home) 1 -1))]
    (assoc outcome :next-scorediff scorediff)))

(defn- get-next-togo [game outcome]
  (if (:first-down outcome)
    (min 10 (:next-yard-line-norm outcome))
    (- (:togo game) (or (:yards-diff outcome) 0))))

(def normal-mode-handler (NormalModeHandler.))
(def kickoff-mode-handler (KickoffModeHandler.))
(def extrapoint-mode-handler (ExtrapointModeHandler.))

(defn- get-handler-at-mode [game-mode]
  (case game-mode
    :normal normal-mode-handler
    :kickoff kickoff-mode-handler
    :extrapoint extrapoint-mode-handler))

(defn- apply-outcome [outcome game]
  (let [handler (get-handler-at-mode (:game-mode game))
        yard-line-after (.get-yard-line-after handler game outcome)
        down-changes (.get-down-changes handler game outcome)
        yard-line-changes {:yard-line-after yard-line-after}
        score-and-mode-changes (.get-score-and-mode handler game outcome yard-line-after)]
    (merge outcome
           down-changes
           yard-line-changes
           score-and-mode-changes)))

(defn update-game [game outcome]
  (let [r (-> (apply-outcome outcome game)
              (update-time game)
              (update-next-yard-line-norm game)
              (update-scores game)
              (update-score-diff game))]
    (-> game
        (update-in [:play-count] inc)
        (assoc :quarter (:next-quarter r))
        (assoc :quarter-remaining (:next-quarter-remaining r))
        (assoc :down (:next-down r))
        (assoc :togo (get-next-togo game r))
        (assoc :yard-line (:next-yard-line-norm r))
        (assoc :home-score (:next-home-score r))
        (assoc :away-score (:next-away-score r))
        (assoc :game-mode (:next-game-mode r))
        (assoc :poss (:next-poss r))
        (assoc :scorediff (:next-scorediff r))
        (assoc :completed (:game-completed r)))))

(defn sim-play [game off-play def-play]
  (let [outcome (ai/simulate-outcome game off-play def-play)]
    {:duration (:duration outcome)
     :yards-diff (:yardsgained outcome)
     :kick-good (:kick-good outcome)
     :game-mode (:game-mode game)
     :playtype (:playtype off-play)
     :off-type (:type off-play)}))

(defn run-play [game off-play def-play]
  (println game)
  (let [outcome (sim-play game off-play def-play)]
    (println outcome)
    (update-game game outcome)))
