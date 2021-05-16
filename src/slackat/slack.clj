(ns slackat.slack
  (:require [aleph.http :as http]
            [manifold.deferred :as d]
            [slackat.config :as config]
            [slackat.utils :as u]
            [slackat.execution :as ex]
            [cheshire.core :as json]))

(def bot-scope "commands,channels:read,chat:write")
(def user-scope "channels:read,chat:write")

(defn build-redirect [state]
  (format
    "https://slack.com/oauth/v2/authorize?scope=%s&user_scope=%s&state=%s&client_id=%s"
    bot-scope
    user-scope
    state
    (slackat.config/v :slack-client-id)))

(defn exchange-access [code]
  "https://api.slack.com/authentication/oauth-v2
  https://api.slack.com/methods/oauth.v2.access"
  (d/chain
    (http/post
      "https://slack.com/api/oauth.v2.access"
      {:pool ex/cp
       :form-params {"client_id"     (config/v :slack-client-id)
                     "client_secret" (config/v :slack-client-secret)
                     "code"          code}})
    u/parse-json-body))

(defn post-message
  "https://api.slack.com/methods/chat.postMessage"
  [{:keys [channel
           url
           as-user
           text
           token
           markdown]
    :or {url "https://slack.com/api/chat.postMessage"
         as-user true
         markdown true}
    :as options}]
  (d/chain
    (http/post
      url
      {:headers {"authorization" (str "Bearer " token)
                 "content-type"  "application/json; charset=utf-8"}
       :body    (json/encode
                  {:channel channel
                   :as_user as-user
                   :text    text
                   :mrkdwn  markdown})})
    u/parse-json-body))


(defn schedule-message
  "https://api.slack.com/messaging/scheduling#listing"
  [])