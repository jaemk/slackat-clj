(ns slackat.router
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [slackat.utils :as u]
            [slackat.handlers :as h]
            [slackat.config :as config]))

(defn load-routes []
  (routes
    (ANY "/" [] h/index)
    (ANY "/status" [] (u/->json {:status :ok
                                 :version (config/v :app-version)}))
    (GET "/login" _ h/login)
    (GET "/login/slack" _ h/login-slack)
    (POST "/slack/command" _ h/slack-command)
    (route/not-found (u/->resp
                       :body "nothing to see here"
                       :status 404))))
