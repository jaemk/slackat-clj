(ns user
  (:require [slackat.core :as app]
            [slackat.utils :as u]
            [slackat.config :as config]
            [slackat.database.core :as db]
            [slackat.commands.core :as cmd]
            [slackat.crypto :as cr]
            [slackat.handlers :as h]
            [manifold.deferred :as d]
            [java-time :as jt]
            [cheshire.core :as json]
            [byte-streams :as bs]
            [aleph.http :as http]
            [slackat.slack :as slack])
  (:use [midje.repl]))


(defn do-post []
  @(slack/post-message
     {:channel (config/v :slack-user-id)
      :as-user true
      :text "hey there"
      :token (config/v :slack-user-token)}))


(defn -main []
  (app/-main))
