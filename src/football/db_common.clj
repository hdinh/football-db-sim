(ns football.db-common
  (:require [clojure.java.jdbc :as sql]))

(def sqlite-db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "db/database.db"})

(def column-type-mapping
  {:ydline         :int
   :nextscore      :int
   :scorediff      :int
   :defscore       :int
   :qtr            :int
   :scoreschange   :int
   :series1stdn    :int
   :teamwin        :int
   :gameid         :int
   :season         :int
   :offscore       :int
   :off            :text
   :sec            :int
   :def            :text
   :min            :int
   :togo           :int
   :down           :int
   :description    :text
   :playtype       :text ;; todo - index to another column will be more efficient
   :turnover       :int
   :sack           :int
   :touchdown      :int
   :penalty        :text
   :yardsgained    :int
   :yardlinechange :int
   :scorechange    :int
   :qtrremaining   :int
   :duration       :int
   :gameremaining  :int
   :passer         :text ;; todo - index to another column will be more efficient
   :receiver       :text
   :rusher         :text
   :summaryblob    :blob})

(defn- table-exists [table]
  (sql/with-connection sqlite-db
    (let [tables (-> (sql/find-connection)
                     (.getMetaData)
                     (.getTables nil nil (name table) nil))]
      (.next tables))))

(defn- ensure-empty-table [table]
  (when (table-exists table)
    (sql/with-connection sqlite-db
      (sql/drop-table table))))

(defn- schema-to-string [specs]
  (let [get-type (fn [x] [x (get column-type-mapping x :text)])
        specs-with-type (map get-type specs)]
    (apply str
      (map name
        (apply concat
          (interpose [", "]
            (map (partial interpose " ") specs-with-type)))))))

(defn- create-table-internal [dbname schema]
  (sql/do-commands
    (format "CREATE TABLE %s (%s)"
      (name dbname)
      (schema-to-string schema))))

(defn create-table [dbname schema]
  (ensure-empty-table dbname)
  (sql/with-connection sqlite-db
    (sql/transaction
      (create-table-internal dbname schema))))

(defn create-table-and-insert-rows [dbname schema play-rows]
  (ensure-empty-table dbname)
  (sql/with-connection sqlite-db
    (sql/transaction
      (create-table-internal dbname schema)
      (doseq [x play-rows]
        (sql/insert-records dbname x)))))
