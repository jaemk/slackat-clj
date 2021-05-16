(ns slackat.database.core
  (:require [slackat.config :as config]
            [clojure.spec.alpha :as s]
            [hikari-cp.core :refer [make-datasource]]
            [clojure.string :as string]
            [clojure.java.jdbc :as j]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [honeysql-postgres.format :refer :all]
            [honeysql-postgres.helpers :as pg]
            [slackat.utils :as u]
            [taoensso.timbre :as t]))
  ;(:import (clojure.lang Keyword)
  ;         (org.postgresql.util PGobject)
  ;         (java.sql ResultSetMetaData)))


; ----- datasource config ------
(def db-config
  {:adapter           "postgresql"
   :username          (config/v :db-user)
   :password          (config/v :db-password)
   :database-name     (config/v :db-name)
   :server-name       (config/v :db-host)
   :port-number       (config/v :db-port)
   :maximum-pool-size (max 10 (config/v :num-threads))})

(defonce datasource (delay (make-datasource db-config)))

(defn conn [] {:datasource @datasource})

(defn migration-config
  ([] (migration-config (conn)))
  ([connection] {:store         :database
                 :migration-dir "migrations"
                 :init-script   "init.sql"
                 :db            connection}))


; ----- postgres/jdbc/honeysql extensions ------
;(defn kw-namespace->enum-type [namespace']
;  (s/assert ::types/registered-kw-namespace namespace')
;  (u/kebab->under namespace'))
;
;(defn kw->pg-enum [kw]
;  "Converts a namespaced keyword to a jdbc/postgres enum"
;  (let [type (-> (namespace kw)
;                 (kw-namespace->enum-type))
;        value (name kw)]
;    (doto (PGobject.)
;      (.setType type)
;      (.setValue value))))
;
;
;(extend-type Keyword
;  j/ISQLValue
;  (sql-value [kw]
;    "Extends keywords to be auto-converted by jdbc to postgres enums"
;    (kw->pg-enum kw)))
;
;
;(defn kw-to-sql [kw]
;  "Copy of honeysql's internal Keyword to-sql functionality"
;  (let [s (name kw)]
;    (case (.charAt s 0)
;      \% (let [call-args (string/split (subs s 1) #"\." 2)]
;           (honeysql.format/to-sql (apply honeysql.types/call (map keyword call-args))))
;      \? (honeysql.format/to-sql (honeysql.types/param (keyword (subs s 1))))
;      (honeysql.format/quote-identifier kw))))
;
;(extend-protocol honeysql.format/ToSql
;  Keyword
;  (to-sql [kw]
;    "Extends honeysql to convert namespaced keywords to pg enums"
;    (let [type (namespace kw)]
;      (if (nil? type)
;        (kw-to-sql kw) ;; do default honeysql conversions
;        (let [type (kw-namespace->enum-type type)
;              enum-value (format "'%s'::%s" (name kw) type)]
;          enum-value)))))
;
;
;(def +schema-enums+
;  "A set of all PostgreSQL enums in schema.sql. Used to convert
;  enum-values back into namespaced keywords."
;  (->> types/kw-namespaces
;       (map u/kebab->under)
;       (set)))
;
;
;(extend-type String
;  j/IResultSetReadColumn
;  (result-set-read-column [val
;                           ^ResultSetMetaData rsmeta
;                           idx]
;    "Hook in enum->keyword conversion for all registered `schema-enums`"
;    (let [type (.getColumnTypeName rsmeta idx)]
;      (if (contains? +schema-enums+ type)
;        (keyword (u/under->kebab type) val)
;        val))))


; ----- helpers ------
(defn first-or-err [ty]
  "create a fn for retrieving a single row or throwing an error"
  (fn [result-set]
    (if-let [one (first result-set)]
      one
      (u/ex-does-not-exist! ty))))


(defn pluck [rs & {:keys [empty->nil]
                   :or   {empty->nil false}}]
  "Plucks the first item from a result-set if it's a seq of only one item.
   Asserts the result-set, `rs`, has something in it, unless `:empty->nil true`"
  (let [empty-or-nil (or (nil? rs)
                         (empty? rs))]
    (cond
      (and empty-or-nil empty->nil) nil
      empty-or-nil (u/ex-error!
                     (format "Expected a result returned from database query, found %s" rs))
      :else (let [[head tail] [(first rs) (rest rs)]]
              (if (empty? tail)
                head
                rs)))))


(defn insert! [conn stmt]
  "Executes insert statement returning a single map if
  the insert result is a seq of one item"
  (j/query conn
           (-> stmt
               (pg/returning :*)
               sql/format)
           {:result-set-fn pluck}))


(defn update! [conn stmt]
  (j/query conn
           (-> stmt
               (pg/returning :*)
               sql/format)
           {:result-set-fn #(pluck % :empty->nil true)}))


(defn delete! [conn stmt]
  (j/query conn
           (-> stmt
               (pg/returning :*)
               sql/format)
           {:result-set-fn #(pluck % :empty->nil true)}))


(defn query [conn stmt & {:keys [first-err-key row-fn]
                          :or   {first-err-key  nil
                                 row-fn identity}}]
  (let [rs-fn (if (nil? first-err-key)
                nil
                (first-or-err first-err-key))]
    (j/query conn
             (-> stmt
                 sql/format)
             {:row-fn        row-fn
              :result-set-fn rs-fn})))



; ----- database queries ------
(defn create-token [conn {nonce      :nonce
                          iv         :iv
                          bot_id     :bot-id
                          bot_token  :bot-token
                          bot_scope  :bot-scope
                          user_id    :user-id
                          user_scope :user-scope
                          user_token :user-token}]
  (j/with-db-transaction [trans conn]
                         (let [token
                               (insert!
                                 trans
                                 (-> (h/insert-into :slackat.tokens)
                                     (h/values [{
                                                 :nonce nonce
                                                 :iv iv
                                                 :bot_id bot_id
                                                 :bot_scope bot_scope
                                                 :bot_token bot_token
                                                 :user_id user_id
                                                 :user_scope user_scope
                                                 :user_token user_token}])))]
                           token)))


(defn get-token-for-user [conn user-id]
  (t/info "loading user token" {:user-id user-id})
  (query conn
         (-> (h/select :*)
             (h/from :slackat.tokens)
             (h/where [:= :user_id user-id])
             (h/order-by [:created :desc]))
         :first-err-key :db-get/token))

(defn get-tokens-for-user [conn user-id]
  (t/info "loading user token" {:user-id user-id})
  (query conn
         (-> (h/select :*)
             (h/from :slackat.tokens)
             (h/where [:= :user_id user-id])
             (h/order-by [:created :desc]))))
