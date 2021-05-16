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
      "we can create users and items"
      ; db is setup
      (nil? @test-db) => false

      ; create and save a user
      (db/create-token
        (:conn @test-db)
        {:nonce "1"
         :iv "1"
         :bot-id "1"
         :bot-token "1"
         :bot-scope "1"
         :user-id "bean"
         :user-scope "1"
         :user-token "1"}) =>
        (fn [result]
          (swap! state #(merge % result)) ; save for later
          (= (-> result :user_id) "bean"))

      ; lookup by user-id
      (db/get-token-for-user
        (:conn @test-db)
        "bean") =>
        (fn [result]
          (= (-> result :id) (:id @state))))

    (fact
      "we can truncate the db"
      (db/get-tokens-for-user (:conn @test-db) "bean") =>
        (fn [result]
          (empty? result)))))
