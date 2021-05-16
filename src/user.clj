(ns user)


(defn initenv []
  (require '[slackat.core :as app]
           '[slackat.utils :as u]
           '[slackat.config :as config]
           '[slackat.database.core :as db]
           '[slackat.commands.core :as cmd]))
