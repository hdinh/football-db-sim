(ns football.init-data
  (:require [clojure.java.jdbc :as sql]
            [football.db-common :as db]
            [football.stats-importer :as stats-importer]
            [football.ai-db :as ai-db]))

(defn -main []
  (println "initializing all data for football... (this could take a long time)")
  (do
    (println "stats_importer... ")
    (time (stats-importer/import-plays-into-db)))
  (do
    (println "ai_db... ")
    (time (ai-db/initialize-summary-tables)))
  "success!")
