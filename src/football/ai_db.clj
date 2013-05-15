(ns football.ai-db
  (:use     [korma.db]
            [korma.core])
  (:require [clojure.java.jdbc :as sql]
            [football.db-common :as db]
            [football.utils :as utils]))

;;*****************************************************
;; Primitive clustering
;;*****************************************************

(defn get-togo-range [togo]
  (cond
    (>= togo 16) [16 100]
    (>= togo 11) [11 15]
    (>= togo 10) [10 10]
    (>= togo 7)  [7 9]
    (>= togo 4)  [4 6]
    (>= togo 2)  [2 3]
    :else        [1 1]))

(defn get-ydline-range [ydline]
  (cond
    (>= ydline 90) [90 100]
    (>= ydline 60) [60 90]
    (>= ydline 40) [40 59]
    (>= ydline 30) [30 39]
    (>= ydline 20) [20 29]
    (>= ydline 10) [10 19]
    (>= ydline 5)  [ 5  9]
    :else          [ 0  4]))

(defn get-gameremaining-range [gameremaining]
  (cond
    (>= gameremaining 2700) [2700 3600]
    (>= gameremaining 2000) [2000 2699]
    (>= gameremaining 1800) [1800 1999]
    (>= gameremaining 1200) [1200 1799]
    (>= gameremaining  900) [ 900 1199]
    (>= gameremaining  500) [ 500 899]
    (>= gameremaining  200) [ 200 499]
    :else                   [   1 199]))

(defn get-scorediff-range [scorediff]
  (cond
    (>= scorediff 24)  [24 100]
    (>= scorediff 14)  [14 23]
    (>= scorediff 10)  [10 13]
    (>= scorediff 7)   [7 9]
    (>= scorediff 4)   [4 6]
    (>= scorediff 3)   [3 3]
    (>= scorediff 1)   [1 2]
    (=  scorediff 0)   [0 0]
    (>= scorediff -2)  [-2 -1]
    (>= scorediff -3)  [-3 -3]
    (>= scorediff -6)  [-6 -4]
    (>= scorediff -9)  [-9 -7]
    (>= scorediff -13) [-13 -10]
    (>= scorediff -23) [-23 -14]
    :else              [-100 -24]))

(defn get-yardlinechange-range [yardsgained]
  (cond
    (>= yardsgained 10)  [10  99]
    (>= yardsgained 5)   [ 5   9]
    (>= yardsgained 1)   [ 1   4]
    (=  yardsgained 0)   [ 0   0]
    (>= yardsgained -1)  [-1  -4]
    :else                [-5 -99]))

;;*****************************************************
;; Database summary logic
;;*****************************************************

(defdb football-db (sqlite3 db/sqlite-db))
(defentity plays (database football-db))
(defentity playtype_summary (database football-db))
(defentity touchdown_summary (database football-db))
(defentity yardlinechange_summary (database football-db))
(defentity yardlinechange_summary2 (database football-db))
(defentity scorechange_summary (database football-db))
(defentity duration_summary (database football-db))
(defentity duration_summary2 (database football-db))

(defn- remove-nils [coll]
  (into {} (filter #(comp not nil? %) coll)))

(defn- replace-range-with [coll pred]
  (letfn [(f [x]
            (if (coll? (second x))
              [(first x) (pred (second x))]
              x))]
    (into {} (map f coll))))

;; http://stackoverflow.com/questions/1879885/clojure-how-to-to-recur-upon-exception
(defn- try-times* [n thunk]
  (loop [n n]
    (if-let [result (try
                      [(thunk)]
                      (catch Exception e
                        (when (zero? n)
                          (throw e))))]
      (result 0)
      (do
        (Thread/sleep 200)
        (recur (dec n))))))

(defmacro try-times [n & body]
  `(try-times* ~n (fn [] ~@body)))

(defn- get-from-cache [summary-table criterias]
  (let [q (remove-nils criterias)
        q (replace-range-with q first)
        row (try-times 3 (select summary-table
                             (fields :summaryblob)
                             (where q)))]
    (when-let [blob (first row)]
      (utils/deserialize (:summaryblob blob)))))

(defn- save-to-cache [summary-table criterias data]
  (let [q (remove-nils criterias)
        q (replace-range-with q first)
        blob (utils/serialize data)
        row (conj q {:summaryblob blob})]
    (println "*** " q)
    (try
      (try-times 2
        (insert summary-table
          (values row)))
      (catch Exception e (str "ERROR: could not save to cache: " (.getMessage e))))
    data))

(defn- distribution-sql-internal [field criterias]
  (let [q (remove-nils criterias)
        q (replace-range-with q (fn [x] ['between x]))]
    (try-times 15
      (select plays
        (fields field [(sqlfn count) :count])
        (where q)
        (group field)
        (order :count)))))

(defn- distribution-sql
  [field summary-table criterias]
  (let [cached (get-from-cache summary-table criterias)]
    (if cached
      cached
      (save-to-cache summary-table criterias (distribution-sql-internal field criterias)))))

(defn get-play-distribution
  [{:keys [qtr down togo ydline gameremaining scorediff]}]
  (distribution-sql :playtype playtype_summary {:qtr qtr
                                                :down down
                                                :togo (get-togo-range togo)
                                                :ydline (get-ydline-range ydline)
                                                :gameremaining (get-gameremaining-range gameremaining)
                                                :scorediff (get-scorediff-range scorediff)}))

(defn get-touchdown-distribution
  ([{:keys [qtr down togo ydline gameremaining scorediff playtype]}]
     (distribution-sql :touchdown touchdown_summary {:qtr qtr
                                                     :down down
                                                     :playtype (name playtype)
                                                     :togo (get-togo-range togo)
                                                     :ydline (get-ydline-range ydline)
                                                     :gameremaining (get-gameremaining-range gameremaining)
                                                     :scorediff (get-scorediff-range scorediff)})))

(defn get-yardlinechange-distribution
  ([{:keys [qtr down togo ydline gameremaining scorediff playtype touchdown]}]
     (distribution-sql :yardlinechange yardlinechange_summary {:qtr qtr
                                                               :down down
                                                               :playtype (name playtype)
                                                               :touchdown touchdown
                                                               :togo (get-togo-range togo)
                                                               :ydline (get-ydline-range ydline)
                                                               :gameremaining (get-gameremaining-range gameremaining)
                                                               :scorediff (get-scorediff-range scorediff)})))

(defn get-yardlinechange-distribution2
  ([{:keys [scorediff]}]
    (distribution-sql :yardlinechange yardlinechange_summary2 {:playtype "kickoff"
                                                               :scorediff (get-scorediff-range scorediff)})))

(defn get-scorechange-distribution
  [{:keys [qtr ydline scorediff playtype]}]
  (distribution-sql :scorechange scorechange_summary {:qtr qtr
                                                      :ydline (get-ydline-range ydline)
                                                      :scorediff (get-scorediff-range scorediff)
                                                      :playtype (name playtype)}))

(defn get-duration-distribution
  ([{:keys [qtr scorediff yardsgained playtype touchdown]}]
    (distribution-sql :duration duration_summary {:qtr qtr ;; TODO - this sucks, but no crashses
                                                  :scorediff (get-scorediff-range scorediff)
                                                  :yardlinechange (get-yardlinechange-range yardsgained)
                                                  :playtype (name playtype)
                                                  :touchdown touchdown})))

(defn get-duration-distribution2
  ([{:keys [playtype]}]
    (distribution-sql :duration duration_summary2 {:playtype (name playtype)})))

(defn initialize-summary-tables []
  (db/create-table :playtype_summary [:qtr :down :togo :ydline :gameremaining :scorediff :summaryblob])
  (db/create-table :touchdown_summary [:qtr :down :togo :ydline :gameremaining :scorediff :playtype :summaryblob])
  (db/create-table :yardlinechange_summary [:qtr :down :togo :ydline :gameremaining :scorediff :playtype :touchdown :summaryblob])
  (db/create-table :yardlinechange_summary2 [:playtype :scorediff :summaryblob])
  (db/create-table :scorechange_summary [:qtr :ydline :scorediff :playtype :summaryblob])
  (db/create-table :duration_summary [:qtr :scorediff :yardlinechange :playtype :touchdown :summaryblob])
  (db/create-table :duration_summary2 [:playtype :summaryblob]))

(defn -main [& args]
  (get-play-distribution {:qtr 4 :down 1 :togo 10 :ydline 50 :gameremaining 500 :scorediff 3})
  (get-yardlinechange-distribution {:qtr 4 :down 1 :playtype 10 :togo 50 :ydline 500 :gameremaining 3 :scorediff "rush"})
  (get-yardlinechange-distribution2 {:scorediff 3})
  (get-scorechange-distribution {:qtr 1 :ydline 15 :scorediff 0 :playtype "fieldgoal"}))
