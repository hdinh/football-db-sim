(ns football.simapp
  (:require [football.engine :as engine]
            [football.ai     :as ai]))

(defn simulate-game [g]
  (loop [game g]
    (if (not (engine/completed? game))
      (let [offense-play (ai/pick-offense-play game)
            defense-play (ai/pick-defense-play game)]
        (recur (engine/run-play game offense-play defense-play))))))

(defn -main []
  (println "lets get it on!")
  (let [game (engine/new-game)]
    (time (simulate-game game))))
