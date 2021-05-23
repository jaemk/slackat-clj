(ns slackat.database-test
  (:use midje.sweet)
  (:require [slackat.database.core :as db]
            [slackat.utils :as u]
            [slackat.test-utils :refer [setup-db
                                        teardown-db
                                        truncate-db]]))

(defonce test-db (atom nil))
(defonce state (atom {}))

(with-state-changes
  [(before :contents (do
                       (setup-db test-db)
                       (reset! state {})))
   (after :contents (teardown-db test-db))
   (before :facts (truncate-db test-db))]
  (facts
    (fact
      "we can upsert slack tokens"
      ; db is setup
      (nil? @test-db) => false

      ; create and save a token
      (db/upsert-slack-token
        (:conn @test-db)
        {:salt "1"
         :iv "1"
         :type :slack-token-type/user
         :slack-id "USER1"
         :slack-team-id "TEAM1"
         :scope "read,write"
         :encrypted "xxx"}) =>
      (fn [result]
        (swap! state #(assoc % :slack-token result)) ; save for later
        (and
          (= (-> result :slack_id) "USER1")
          (= (-> result :type) :slack-token-type/user)
          (= (-> result :slack_team_id) "TEAM1")))

      ; lookup by user & team
      (db/get-token-for-user
        (:conn @test-db)
        "USER1"
        "TEAM1") =>
      (fn [result]
        (= (-> result :id) (-> @state :slack-token :id)))

      ; upsert a new token for this user & team
      (db/upsert-slack-token
        (:conn @test-db)
        {:salt "2"
         :iv "2"
         :type :slack-token-type/user
         :slack-id "USER1"
         :slack-team-id "TEAM1"
         :scope "read,write,smell"
         :encrypted "yyy"}) =>
      (fn [result]
        (swap! state #(assoc % :new-token-id (:id result)))
        (= (:id result) (-> @state :slack-token :id)))

      ; there's only one in the db
      (db/get-all-slack-tokens-count (:conn @test-db)) =>
      (fn [result]
        (= (:count result) 1)))


    (fact
      "we can truncate the db"
      (db/get-tokens-for-user (:conn @test-db) "USER1" "TEAM1") =>
        (fn [result]
          (empty? result)))))
