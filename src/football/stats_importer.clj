(ns football.stats-importer
  (:require [football.db-common :as db]
            [football.rules :as rules]))

;;*****************************************************
;; CSV Processing
;;*****************************************************

(def csv-filenames ["2002_nfl_pbp_data.csv",
                    "2004_nfl_pbp_data.csv", 
                    "2005_nfl_pbp_data.csv",
                    "2006_nfl_pbp_data.csv",
                    "2007_nfl_pbp_data.csv",
                    "2008_nfl_pbp_data.csv",
                    "2009_nfl_pbp_data.csv",
                    "2011_nfl_pbp_data.csv",
                    "2012_nfl_pbp_data_reg_season.csv"])

(def csv-files
  (map (fn [x] (clojure.java.io/file "data" x)) csv-filenames))

(defn- create-tokens [line]
  (clojure.string/split line #","))

(defn- get-play-columns [all-plays]
  (reduce #(into %1 (keys (first %2))) #{} all-plays))

(defn- isnum [x]
  (if (string? x)
    (re-find #"^\d+$" x)
    x))

(defn- try-get-int [x]
  (if (and x (isnum x)) 
    (Integer. x)
    0))

(defn- parse-time [description]
  (if description
    (if-let [temp-reg (re-find #"^\((\d+)?\:(\d+)\)" description)]
      (+ (* 60 (Integer. (or (nth temp-reg 1) "0"))) (Integer. (nth temp-reg 2))))
    nil))


;;*****************************************************
;; Additional Columns
;;*****************************************************

(defn- add-coll [play query coll-func]
  (let [element (query play)]
    {:current
       (conj
         (:current play)
         (if (vector? element)
           (apply coll-func element)
           (if element
             (coll-func element))))
     :last
       (:last play)
     :next
       (:next play)}))

(defn- set-description-to-lower [play]
  (add-coll play #(:description (:current %)) (fn [x] {:description (.toLowerCase x)})))

(defn- fix-kickoffyardline-coll [play]
  (let [f (fn [description]
            (when (and (not (.contains description "extra point"))
                       (not (.contains description "field goal"))
                       (.contains description "kicks"))
              {:ydline rules/yard-line-at-kickoff}))]
    (add-coll play #(:description (:current %)) f)))

(defn- replace-keyword-with-name [play]
  (let [f (fn [x]
            (let [k (first x)
                  v (second x)]
              [k (if (keyword? v) (name v) v)]))
        final-current (into {} (map f (:current play)))]
    (assoc play :current final-current)))

(defn- add-play-type-coll [play]
  (let [f (fn [description]
            {:playtype
             (cond
               (.contains description "no play") :noplay
               (.contains description "intercepted") :pass
               (.contains description "right tackle") :rush
               (.contains description "right guard") :rush
               (.contains description "left guard") :rush
               (.contains description "up the middle") :rush
               (.contains description "left tackle") :rush
               (.contains description "left end") :rush
               (.contains description "right end") :rush
               (.contains description "pass") :pass
               (.contains description "kneel") :kneel
               (.contains description "punt") :punt
               (.contains description "extra point") :extrapoint
               (.contains description "sacked") :pass
               (.contains description "field goal") :fieldgoal
               (.contains description "spiked") :spike
               (.contains description "kicks") :kickoff
               :else :unknown)})]
    (add-coll play #(:description (:current %)) f)))

(defn- add-turnover-coll [play]
  (let [f (fn [description]
            {:turnover
             (or (.contains description "intercepted")
                 (.contains description "fumble"))})]
    (add-coll play #(:description (:current %)) f)))

(defn- add-sack-coll [play]
  (let [f (fn [description]
            {:sack (.contains description "sack")})]
    (add-coll play #(:description (:current %)) f)))

(defn- add-touchdown-coll [play]
  (let [f (fn [description]
            {:touchdown (.contains description "touchdown")})]
    (add-coll play #(:description (:current %)) f)))

(defn- add-penalty-coll [play]
  (let [f (fn [description]
            {:penalty (.contains description "penalty")})]
    (add-coll play #(:description (:current %)) f)))

(defn- add-yards-gained-coll [play]
  (let [f (fn [description play next-play]
            (let [current-raw (:ydline play)
                  next-raw (:ydline next-play)
                  current-ydline (try-get-int current-raw)
                  next-ydline (try-get-int next-raw)
                  diff1 (- next-ydline current-ydline)
                  diff2 (- current-ydline next-ydline)]
              {:yardsgained
               (cond
                 (.contains description "touchdown") current-ydline
                 (.contains description "fumble") diff1
                 (.contains description "intercepted") diff1
                 (.contains description "kicks") 0
                 (.contains description "extra point") 0
                 (.contains description "punt") 0
                 :else diff2)}))]
    (add-coll play (fn [x] [(:description (:current x)) (:current x) (:next x)]) f)))

(defn- add-qtr-remaining-coll [play]
  (let [f (fn [current last]
            (let [current-time (parse-time (:description current))]
              {:qtrremaining
               (or current-time
                   (if last
                     (or (:qtrremaining last) 0) ;; TODO: this doesn't do anything
                     rules/time-in-quarter))}))]
    (add-coll play (fn [x] [(:current x) (:last x)]) f)))

(defn- add-game-remaining-coll [play]
  (let [f (fn [qtrremaining qtr]
            (let [quarters-remaining (max (- rules/quarters-in-game (try-get-int qtr)) 0)
                  remaining-quarter-time (* rules/time-in-quarter quarters-remaining)]
              {:gameremaining (+ qtrremaining remaining-quarter-time)}))]
    (add-coll play (fn [x] [(:qtrremaining (:current x)) (:qtr (:current x))]) f)))

(defn- add-duration-coll [play]
  (let [f (fn [current next]
            (let [current-time (parse-time (:description current))
                  next-time (parse-time (:description next))
                  knows-time (and current-time next-time (= (:qtr current) (:qtr next)))]
              {:duration
               (when knows-time
                 (max 0 (- current-time next-time)))}))]
    (add-coll play (fn [x] [(:current x) (:next x)]) f)))

(defn- add-scorediff-coll [play]
  (let [f (fn [p]
            (let [offscore (try-get-int (:offscore p))
                  defscore (try-get-int (:defscore p))]
              {:scorediff (- offscore defscore)}))]
    (add-coll play (fn [x] (:current x)) f)))

(defn- add-scorechange-coll [play]
  (let [f (fn [current next]
            (if (= (:gameid current) (:gameid next))
              (let [get-diff (fn [x] (- (try-get-int (:offscore x)) (try-get-int (:defscore x))))
                    offscore-diff-current (get-diff current)
                    offscore-diff-next (if (= (:off next) (:off current))
                                         (get-diff next)
                                         (- (get-diff next)))]
                {:scorechange (- offscore-diff-next offscore-diff-current)})))]
    (add-coll play (fn [x] [(:current x) (:next x)]) f)))

(defn- add-yardlinechange-coll [play]
  (let [f (fn [current next]
            (let [yardline-current (try-get-int (:ydline current))
                  yardline-next (if (= (:off next) (:off current))
                                  (try-get-int (:ydline next))
                                  (- 100 (try-get-int (:ydline next))))]
              {:yardlinechange (- yardline-current yardline-next)}))]
    (add-coll play (fn [x] [(:current x) (:next x)]) f)))

(defn- add-passer-coll [play]
  (let [f (fn [current]
            {:passer
             (when (= (:playtype current) :pass)
               (first (re-seq #"\S\.\S*" (:description current))))})]
    (add-coll play (fn [x] (:current x)) f)))

(defn- add-receiver-coll [play]
  (let [f (fn [current]
            {:receiver
              (when (= (:playtype current) :pass)
                (if-let [raw (first (re-seq #"to \S\.\S*" (:description current)))]
                  (subs raw 3)))})]
    (add-coll play (fn [x] (:current x)) f)))

(defn- add-rusher-coll [play]
  (let [f (fn [current]
            {:rusher
             (when (= (:playtype current) :rush)
               (first (re-seq #"\S\.\S*" (:description current))))})]
    (add-coll play (fn [x] (:current x)) f)))

(defn- extend-with-extra-cols [original]
  (-> original
      set-description-to-lower
      fix-kickoffyardline-coll
      add-play-type-coll
      add-turnover-coll
      add-sack-coll
      add-touchdown-coll
      add-penalty-coll
      add-yards-gained-coll
      add-qtr-remaining-coll
      add-game-remaining-coll
      add-duration-coll
      add-scorediff-coll
      add-scorechange-coll
      add-yardlinechange-coll
      add-passer-coll
      add-receiver-coll
      add-rusher-coll
      replace-keyword-with-name
      :current))

(defn- create-play-structure [plays header-tokens]
  (let [indexes (range (count plays))
        f (fn [x] 
             (let [current (nth plays x)
                   last (nth plays (- x 1) nil)
                   next (nth plays (+ x 1) nil)
                   samegame (fn [x] (= (:gameid current) (:gameid x)))]
               {:current (zipmap header-tokens current)
                :last (if (samegame last) (zipmap header-tokens last) nil)
                :next (if (samegame next) (zipmap header-tokens next) nil)}))]
    (map f indexes)))

(defn- read-season-plays [file]
  (let [lines (clojure.string/split-lines (slurp file))
        header-tokens (map keyword (create-tokens (first lines)))
        content-tokens (map create-tokens (rest lines))
        plays-structure (create-play-structure content-tokens header-tokens)]
    (map extend-with-extra-cols plays-structure)))

(defn- read-all-plays []
  (map read-season-plays csv-files))


;;*****************************************************
;; Import plays to DB
;;*****************************************************

(defn import-plays-into-db []
  (let [all-plays (read-all-plays)
        schema (vec (get-play-columns all-plays))
        rows (flatten all-plays)]
    (db/create-table-and-insert-rows :plays schema rows)))

(defn -main []
  (println "importing stats into database...")
  (time (import-plays-into-db)))
