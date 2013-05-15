(ns football.rules)

(def yard-line-at-kickoff 65)

(def yard-line-at-extrapoint 3)

(def yard-line-at-touchback 20)

(def time-in-quarter 900)

(def quarters-in-game 4)

(defn get-score [score-type]
  (case score-type
    :touchdown 6
    :fieldgoal 3
    :extrapoint 1
    0))
