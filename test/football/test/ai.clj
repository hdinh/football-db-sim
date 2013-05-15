(ns football.test.ai
  (:require [football.ai :as ai]
            [football.engine :as engine])
  (:use     [clojure.test]))

(defn- non-numbers [x]
  (or (keyword? (second x)) (= Boolean (class (second x))) (nil? (second x))))

(defn- get-average* [n thunk]
  (let [trials (repeatedly n #(thunk))
        trials-num (map (fn [x] (into {} (remove non-numbers x))) trials)
        summed-num (reduce (fn [x y] (merge-with + x y)) {} trials-num)
        avg-fn-num #(assoc %1 (first %2) (/ (second %2) n))
        average-num (reduce avg-fn-num {} summed-num)
        trials-occur (map (fn [x] (into {} (filter non-numbers x))) trials)
        keys-occur (keys (first trials-occur))
        fn-occur #(first (last (sort-by val (frequencies (map % trials-occur)))))
        most-occur (into {} (map (fn [x] [x (fn-occur x)]) keys-occur))]
     (merge average-num most-occur)))

(defmacro run-times [n & body]
  `(get-average* ~n (fn [] ~@body)))

(def num-iterations 7)

(deftest normal-sanity
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 1)
                 (assoc :togo 10)
                 (assoc :yard-line 50)
                 (assoc :down 1)
                 (assoc :quarter-remaining 1000))
        offense (ai/pick-offense-play game)]
    (is (= true (or (= :rush (:type offense))
                    (= :pass (:type offense)))))))

(deftest kickoff-sanity
  (let [game (engine/new-game)
        offense (ai/pick-offense-play game)
        defense (ai/pick-defense-play game)
        r (run-times num-iterations (engine/sim-play game offense defense))]
    (is (> (:yards-diff r) 25))))

(deftest fourth-down-sanity
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 1)
                 (assoc :togo 10)
                 (assoc :yard-line 80)
                 (assoc :down 4)
                 (assoc :quarter-remaining 1500))
        offense (ai/pick-offense-play game)
        defense (ai/pick-defense-play game)
        r (run-times num-iterations (engine/sim-play game offense defense))]
    (is (= :punt (:playtype offense)))
    (is (> (:yards-diff r) 15))))

(deftest fieldgoal-sanity
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :quarter 1)
                 (assoc :togo 10)
                 (assoc :yard-line 14)
                 (assoc :down 4)
                 (assoc :quarter-remaining 500))
        offense (ai/pick-offense-play game)
        defense (ai/pick-defense-play game)
        r (run-times 15 (engine/sim-play game offense defense))]
    (is (= :fieldgoal (:playtype offense))
    (is (= true (:kick-good r))))))

(deftest punt-sanity
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :poss :away)
                 (assoc :quarter 1)
                 (assoc :togo 10)
                 (assoc :yard-line 80)
                 (assoc :down 4))
        offense (ai/pick-offense-play game)
        defense (ai/pick-defense-play game)
        r (run-times num-iterations (engine/sim-play game offense defense))]
    (is (= :punt (:playtype offense)))
    (is (> (:yards-diff r) 15))))

(deftest duration-sanity
  (let [game (-> (engine/new-game)
                 (assoc :game-mode :normal)
                 (assoc :poss :away)
                 (assoc :quarter 1)
                 (assoc :togo 10)
                 (assoc :yard-line 80)
                 (assoc :down 1))
        offense (ai/pick-offense-play game)
        defense (ai/pick-defense-play game)
        r (run-times num-iterations (engine/sim-play game offense defense))]
    (is (> (:duration r) 3))))
