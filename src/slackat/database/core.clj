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
            [taoensso.timbre :as t])
  (:import (clojure.lang Keyword)
           (org.postgresql.util PGobject)
           (java.sql ResultSetMetaData)))


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
;; todo: move this to a different namespace
(defn kw-namespace->enum-type [namespace']
  "Convert a keyword's namespace to a postgres enum type's name"
  ;; todo: add the schema here if necessary, define in the +schema-enums+ map
  (u/kebab->under namespace'))

(defn kw->pg-enum [kw]
  "Converts a namespaced keyword to a jdbc/postgres enum"
  (let [type (-> (namespace kw)
                 (kw-namespace->enum-type))
        value (name kw)]
    (doto (PGobject.)
      (.setType type)
      (.setValue value))))


(extend-type Keyword
  j/ISQLValue
  (sql-value [kw]
    "Extends keywords to be auto-converted by jdbc to postgres enums"
    (kw->pg-enum kw)))


(defn kw-to-sql [kw]
  "Copy of honeysql's internal Keyword to-sql functionality so we can extend
  the ToSql protocol below"
  (let [s (name kw)]
    (case (.charAt s 0)
      \% (let [call-args (string/split (subs s 1) #"\." 2)]
           (honeysql.format/to-sql (apply honeysql.types/call (map keyword call-args))))
      \? (honeysql.format/to-sql (honeysql.types/param (keyword (subs s 1))))
      (honeysql.format/quote-identifier kw))))

(extend-protocol honeysql.format/ToSql
  Keyword
  (to-sql [kw]
    "Extends honeysql to convert namespaced keywords to pg enums"
    (let [type (namespace kw)]
      (if (nil? type)
        (kw-to-sql kw) ;; do default honeysql conversions
        (let [type (kw-namespace->enum-type type)
              enum-value (format "'%s'::%s" (name kw) type)]
          enum-value)))))


(def +schema-enums+
  "A set of all PostgreSQL enums in schema.sql. Used to convert
  enum-values back into namespaced keywords."
  #{"slack_token_type"})


(extend-type String
  j/IResultSetReadColumn
  (result-set-read-column [val
                           ^ResultSetMetaData rsmeta
                           idx]
    "Hook in enum->keyword conversion for all registered `schema-enums`"
    (let [type (.getColumnTypeName rsmeta idx)]
      (if (contains? +schema-enums+ type)
        (keyword (u/under->kebab type) val)
        val))))


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
                 sql/format
                 u/spy)
             {:row-fn        row-fn
              :result-set-fn rs-fn})))



; ----- database queries ------
(defn upsert-slack-token [conn {nonce         :nonce
                                iv            :iv
                                type          :type
                                slack-id      :slack-id
                                slack-team-id :slack-team-id
                                scope         :scope
                                encrypted     :encrypted}]
  (insert!
   conn
   (-> (h/insert-into :slackat.slack_tokens)
       (h/values [{
                   :nonce nonce
                   :iv iv
                   :type type
                   :slack_id slack-id
                   :slack_team_id slack-team-id
                   :scope scope
                   :encrypted encrypted}])
       (pg/upsert (-> (pg/on-conflict :type :slack-id :slack-team-id)
                      (pg/do-update-set! [:modified (sql/call :now)] [:nonce nonce] [:iv iv] [:encrypted encrypted] [:scope scope]))))))

(defn get-all-slack-tokens-count [conn]
  (query
    conn
    (-> (h/select [(sql/call :count :*) :count])
        (h/from :slackat.slack_tokens))
    :first-err-key :db-get/all-slack-tokens-count))


(defn get-token-for-user [conn slack-user-id slack-team-id]
  (t/info "loading user token" {:user-id slack-user-id :team-id slack-team-id})
  (query conn
         (-> (h/select :*)
             (h/from :slackat.slack_tokens)
             (h/where [:= :slack_id slack-user-id]
                      [:= :slack_team_id slack-team-id]
                      [:= :type :slack-token-type/user])
             (h/order-by [:created :desc]))
         :first-err-key :db-get/token))

(defn get-tokens-for-user [conn slack-user-id slack-team-id]
  (t/info "loading user token" {:user-id slack-user-id :team-id slack-team-id})
  (query conn
         (-> (h/select :*)
             (h/from :slackat.slack_tokens)
             (h/where [:= :slack_id slack-user-id]
                      [:= :slack_team_id slack-team-id]
                      [:= :type :slack-token-type/user])
             (h/order-by [:created :desc]))))
