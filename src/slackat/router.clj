(ns slackat.router
  (:require [compojure.core :refer [routes ANY GET POST]]
            [compojure.route :as route]
            [slackat.handlers :as h]))

(defn load-routes []
  (routes
    ;; todo: make a cute favicon
    (ANY "/" _ h/index)
    (ANY "/status" _ h/status)
    (GET "/login" _ h/login)
    (GET "/login/slack" _ h/login-slack)
    (POST "/slack/command" _ h/slack-command)
    (route/not-found h/not-found)))
