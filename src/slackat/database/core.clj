(ns slackat.database.core
  (:require [slackat.config :as config]
            [hikari-cp.core :refer [make-datasource]]
            [clojure.java.jdbc :as j]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [honeysql.format]
            [honeysql.types]
    #_:clj-kondo/ignore
            [honeysql-postgres.format :refer :all]
            [honeysql-postgres.helpers :as pg]
            [slackat.utils :as u]
            [taoensso.timbre :as t]))


; ----- datasource config ------
(def db-config
  {:adapter           "postgresql"
   :username          (config/v :db-user)
   :password          (config/v :db-password)
   :database-name     (config/v :db-name)
   :server-name       (config/v :db-host)
   :port-number       (config/v :db-port)
   :maximum-pool-size (or (config/v :db-max-connections) (config/v :num-threads))})

(defonce datasource (delay (make-datasource db-config)))

(defn conn [] {:datasource @datasource})

(defn migration-config
  ([] (migration-config (conn)))
  ([connection] {:store         :database
                 :migration-dir "migrations"
                 ;:init-script   "init.sql"
                 :db            connection}))


; ----- helpers ------
(defn first-or-err
  "create a fn for retrieving a single row or throwing an error"
  [ty]
  (fn [result-set]
    (if-let [one (first result-set)]
      one
      (u/ex-does-not-exist! ty))))


(defn pluck
  "Plucks the first item from a result-set if it's a seq of only one item.
   Asserts the result-set, `rs`, has something in it, unless `:empty->nil true`"
  [rs & {:keys [empty->nil]
         :or   {empty->nil false}}]
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


(defn insert!
  "Executes insert statement returning a single map if
  the insert result is a seq of one item"
  [conn stmt]
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


(defn query [conn stmt & {:keys [first-err-key row-fn result-set-fn]
                          :or   {first-err-key nil
                                 row-fn        identity}}]
  (let [rs-fn (if (nil? first-err-key)
                result-set-fn
                (first-or-err first-err-key))]
    (j/query conn
             (-> stmt
                 sql/format)
             {:row-fn        row-fn
              :result-set-fn rs-fn})))



; ----- database queries ------
(defn upsert-user
  "Insert a new or existing user, called after slack login redirect"
  [conn {slack-id      :slack-id
         slack-team-id :slack-team-id}]
  (-> (insert!
        conn
        (-> (h/insert-into :slackat.users)
            (h/values [{:slack-id      slack-id
                        :slack-team-id slack-team-id}])
            (pg/upsert (-> (pg/on-conflict :slack-id :slack-team-id)
                           (pg/do-update-set! [:modified (sql/call :now)])))))
      ((fn [user] (assoc user :new? (= (:created user) (:modified user)))))))


;; todo: expire old auth tokens in a scheduled job
(defn insert-user-auth
  "Save a user's auth token signature"
  [conn {user-id   :user-id
         signature :signature}]
  (-> (insert!
        conn
        (-> (h/insert-into :slackat.auth_tokens)
            (h/values [{:user-id   user-id
                        :signature signature}])))))


(defn get-user-for-auth
  "Load a user by their auth token signature"
  [conn signature]
  (query
    conn
    ;; todo: join on users
    (-> (h/select :*)
        (h/from :slackat.auth_tokens)
        (h/where [:= :signature signature]))
    :result-set-fn #(pluck % :empty->nil true)))



(defn upsert-slack-token
  "Save or overwrite a slack token for a given (type:user, user-id, team-id)
  or (type:bot, bot-id, team-id)"
  [conn {iv            :iv
         salt          :salt
         type          :type
         slack-id      :slack-id
         slack-team-id :slack-team-id
         scope         :scope
         encrypted     :encrypted}]
  (let [scope (into-array scope)]
    (insert!
      conn
      (-> (h/insert-into :slackat.slack_tokens)
          (h/values [{:iv            iv
                      :salt          salt
                      :type          type
                      :slack_id      slack-id
                      :slack_team_id slack-team-id
                      :scope         scope
                      :encrypted     encrypted}])
          (pg/upsert (-> (pg/on-conflict :type :slack-id :slack-team-id)
                         (pg/do-update-set! [:modified (sql/call :now)] [:iv iv] [:salt salt] [:encrypted encrypted] [:scope scope])))))))

(defn get-all-slack-tokens-count [conn]
  (query
    conn
    (-> (h/select [(sql/call :count :*) :count])
        (h/from :slackat.slack_tokens))
    :first-err-key :db-get/all-slack-tokens-count))


(defn get-slack-token-for-user [conn slack-user-id slack-team-id]
  (t/info "loading user token" {:user-id slack-user-id :team-id slack-team-id})
  (query conn
         (-> (h/select :*)
             (h/from :slackat.slack_tokens)
             (h/where [:= :slack_id slack-user-id]
                      [:= :slack_team_id slack-team-id]
                      [:= :type "user"])
             (h/order-by [:created :desc]))
         :result-set-fn #(pluck % :empty->nil true)))

(defn get-slack-tokens-for-user [conn slack-user-id slack-team-id]
  (t/info "loading user token" {:user-id slack-user-id :team-id slack-team-id})
  (query conn
         (-> (h/select :*)
             (h/from :slackat.slack_tokens)
             (h/where [:= :slack_id slack-user-id]
                      [:= :slack_team_id slack-team-id]
                      [:= :type :user])
             (h/order-by [:created :desc]))))
