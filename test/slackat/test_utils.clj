(ns slackat.test-utils
  (:require [clojure.test :refer :all]
            [taoensso.timbre :as t]
            [clojure.java.jdbc :as jdbc]
            [hikari-cp.core :refer [make-datasource
                                    close-datasource]]
            [slackat.database.core :as db]
            [slackat.utils :as u]
            [migratus.core :as migratus]))


(defn truncate-db [test-db]
  (let [conn (:conn @test-db)]
    (try
      (t/info "truncating test db tables")
      (->> (jdbc/query conn
                        ["select truncate_tables()"])
           (first)
           (:truncate_tables)
           (t/info "truncated tables"))
      (catch Exception e
        (t/warn "ignoring error from truncating:" {:exc-info e})))))

(defn teardown-db [test-db]
  (swap! test-db
         (fn [db]
           (when (not (nil? db))
             (let [conn (:conn db)
                   db-name (:name db)]
               (try
                 (t/info "closing test db conn")
                 (close-datasource (:datasource conn))
                 (t/info "dropping test db")
                 (jdbc/execute! (db/conn)
                                [(str "drop database " db-name)]
                                {:transaction? false})
                 (catch Exception e
                   (t/warn "ignoring error from teardown:" {:exc-info e})))))
           nil)))

(defn setup-db [test-db]
  (teardown-db test-db)
  (let [db-name (->> (u/uuid)
                     u/format-uuid
                     (str "slackat_test_"))]
    (t/info "setting up test db" {:db-name db-name})
    (jdbc/execute! (db/conn)
                   [(str "create database " db-name)]
                   {:transaction? false})
    (let [ds (-> db/db-config
                 (assoc :database-name db-name)
                 (assoc :maximum-pool-size 1)
                 (make-datasource))
          conn {:datasource ds}
          config (db/migration-config conn)]
      (reset! test-db {:conn conn
                       :name db-name})
      (migratus/init config)
      (migratus/migrate config))))

