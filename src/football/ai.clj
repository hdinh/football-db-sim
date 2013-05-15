(ns football.ai
  (require [football.ai-db :as ai-db]
           [football.rules :as rules]))

(def default-duration 20)

;;*****************************************************
;; AI database helpers
;;*****************************************************

(defn- get-rand-dist
  ([dist key]
     (get-rand-dist dist key nil))
  ([dist key default]
     (if (= 0 (count dist))
       default
       (let [total-count (reduce #(+ %1 (:count %2)) 0 dist)
             r (rand-int total-count)]
         (loop [x 0
                c dist]
           (let [y (+ x (:count (first c)))]
             (if (> y r)
               (if (key (first c)) (key (first c)) default)
               (recur y (rest c)))))))))

(defn- get-game-remaining [game]
  (+ (:quarter-remaining game) (* rules/time-in-quarter (- rules/quarters-in-game (:quarter game)))))

(defn- get-playtype-from-db [game]
  (let [pdist (ai-db/get-play-distribution {:qtr (:quarter game)
                                            :down (:down game)
                                            :togo (:togo game)
                                            :ydline (:yard-line game)
                                            :gameremaining (get-game-remaining game)
                                            :scorediff (:scorediff game)})
        pdist (filter (fn [x] (nil? (some #{(:playtype x)} '("noplay" "unknown")))) pdist)
        type-final (keyword (get-rand-dist pdist :playtype :rush))]
    {:type type-final}))

(defn- get-fieldgoal-from-db [game offense-play defense-play]
  (let [sdist (ai-db/get-scorechange-distribution {:qtr (:quarter game)
                                                   :ydline (:yard-line game)
                                                   :scorediff (:scorediff game)
                                                   :playtype (:type offense-play)})
        rscorechange (get-rand-dist sdist :scorechange)
        ddist (ai-db/get-duration-distribution2 {:playtype (:type offense-play)})
        duration-final (get-rand-dist ddist :duration default-duration)]
    {:kick-good (= rscorechange 3)
     :duration duration-final}))

(defn- get-yardsgained-from-db [game offense-play defense-play]
  (let [td-dist (ai-db/get-touchdown-distribution {:qtr (:quarter game)
                                                   :down (:down game)
                                                   :togo (:togo game)
                                                   :ydline (:yard-line game)
                                                   :gameremaining (get-game-remaining game)
                                                   :scorediff  (:scorediff game)
                                                   :playtype (:type offense-play)})
        td-this (get-rand-dist td-dist :touchdown 0)
        ydist (ai-db/get-yardlinechange-distribution {:qtr (:quarter game)
                                                      :down (:down game)
                                                      :togo (:togo game)
                                                      :ydline (:yard-line game)
                                                      :touchdown td-this
                                                      :gameremaining (get-game-remaining game)
                                                      :scorediff  (:scorediff game)
                                                      :playtype (:type offense-play)})
        ryardsgained (get-rand-dist ydist :yardlinechange 0)
        yardsgained-final (if (= td-this 1) (:yard-line game) ryardsgained)
        ddist (ai-db/get-duration-distribution {:qtr (:quarter game)
                                                :scorediff (:scorediff game)
                                                :yardsgained yardsgained-final
                                                :playtype (:type offense-play)
                                                :touchdown td-this})
        duration-final (get-rand-dist ddist :duration default-duration)]
    {:yardsgained yardsgained-final
     :duration duration-final}))


;;*****************************************************
;; AI pick plays
;;*****************************************************

(defmulti pick-offense-play :game-mode)
(defmulti pick-defense-play :game-mode)

(defmethod pick-offense-play :normal [game]
  (let [from-db (get-playtype-from-db game)
        type-final (:type from-db)]
    (cond
      (= type-final :fieldgoal)
        {:type type-final
         :formation :fieldgoal
         :playtype :fieldgoal}
      (= type-final :punt)
        {:type type-final
         :formation :punt
         :playtype :punt}
      :default
        {:type type-final
         :formation :punt
         :playtype :normal})))

(defmethod pick-offense-play :kickoff [game]
  ;; TODO: You really want to first figure out if it is an onside.
  {:type :kickoff
   :playtype :kickoff})

(defmethod pick-offense-play :extrapoint [game]
  ;; TODO: You really want to first figure out if it is an 2 point attempt
  {:type :extrapoint
   :playtype :fieldgoal})

(defmethod pick-defense-play :normal [game]
  {:formation :3-4})

(defmethod pick-defense-play :kickoff [game]
  {:type :kickoff})

(defmethod pick-defense-play :extrapoint [game]
  {:type :extrapoint})


;;*****************************************************
;; AI simulate outcome
;;*****************************************************
 
(defmulti simulate-outcome (fn [game offense-play defense-play] (:game-mode game)))

(defmethod simulate-outcome :normal [game offense-play defense-play]
  (cond
    (= (:type offense-play) :fieldgoal)
      (let [from-db (get-fieldgoal-from-db game offense-play defense-play)]
        {:kick-good (:kick-good from-db)
         :duration (:duration from-db)})
    :default
      (let [from-db (get-yardsgained-from-db game offense-play defense-play)]
        {:yardsgained (:yardsgained from-db)
         :duration (:duration from-db)})))

(defmethod simulate-outcome :kickoff [game offense-play defense-play]
  (let [ydist (ai-db/get-yardlinechange-distribution2 {:scorediff (:scorediff game)})
        ryardsgained (get-rand-dist ydist :yardlinechange)]
    {:yardsgained ryardsgained
     :duration 10}))

(defmethod simulate-outcome :extrapoint [game offense-play defense-play]
  {:kick-good true
   :duration 0})
