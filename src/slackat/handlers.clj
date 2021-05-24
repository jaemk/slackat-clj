(ns slackat.handlers
  (:require [taoensso.timbre :as t]
            [manifold.deferred :as d]
            [manifold.stream :as s]
            [manifold.time :as dtime]
            [byte-streams :as bs]
            [aleph.http :as http]
            [java-time :as jt]
            [clojure.java.jdbc :as j]
            [clojure.java.io :as io]
            [clojure.core.async :as a]
            [clojure.core.cache :as cache]
            [cheshire.core :as json]
            [slackat.execution :as ex]
            [slackat.database.core :as db]
            [slackat.crypto :as cr]
            [slackat.utils :as u :refer [->resp ->text ->json ->redirect]]
            [slackat.slack :as slack]
            [slackat.config :as config]))


(defn -user-for-req [req]
  "load a user based on their auth token, or null"
  (u/spy req :on-index)
  (let [cookie (u/get-some-> req :headers :cookie)
        [_ token] (some->> cookie (re-find #"auth_token=(\w+)"))
        user-id (some->> token cr/sign (db/get-user-for-auth (db/conn)))]
    user-id))


(defn index [req]
  (d/let-flow [user (d/future (-user-for-req req))]
              (if (some? user)
                (->text "Welcome back!")
                (->redirect "/login?redirect=/"))))

;; 2m cache ttl
(defonce state-tokens (atom (cache/ttl-cache-factory {} :ttl 120000)))

(defn new-state-token []
  (-> (cr/rand-bytes 32)
      buddy.core.codecs/bytes->hex
      ((fn [hex]
         (swap! state-tokens (fn [s]
                               (cache/evict s hex)
                               (cache/through-cache s hex (constantly nil))))
         hex))))

(defn valid-state-token? [token]
  (let [valid (atom false)]
    (swap!
      state-tokens
      (fn [s]
        (swap!
          valid
          (fn [_]
            (and
              (cache/has? s token))))
        (cache/evict s token)))
    @valid))


(defn login
  "redirect to slack to login"
  [req]
  (-> (slack/build-redirect (new-state-token))
      ->redirect))


;; ------ example:
;  {"token_type" "bot",
;  "enterprise" nil,
;  "bot_user_id" "UUUU",
;  "access_token"
;  "xoxb-xxx-xxx-xxx-xxxx",
;  "is_enterprise_install" false,
;  "scope" "commands,channels:read,chat:write",
;  "authed_user"
;  {"id" "UUUUU",
;   "scope" "channels:read,chat:write",
;   "access_token"
;   "xoxp-xxx-xxx-xxx-xxxx",
;   "token_type" "user"},
;  "ok" true,
;  "team" {"id" "TTT", "name" "THE TEAM"},
;  "app_id" "APPP"}}
(defn -login-user [slack-auth-info]
  (let [sla slack-auth-info
        bot-id (sla "bot_user_id")
        bot-token (sla "access_token")
        bot-scope (sla "scope")
        team-id (u/get-some-> sla "team" "id")
        user-id (u/get-some-> sla "authed_user" "id")
        user-token (u/get-some-> sla "authed_user" "access_token")
        user-scope (u/get-some-> sla "authed_user" "scope")

        bot-token-enc (cr/encrypt bot-token)
        user-token-enc (cr/encrypt user-token)]
    (j/with-db-transaction
      [tr (db/conn)]
      (let [user (db/upsert-user tr {:slack-id user-id
                                     :slack-team-id team-id})
            auth-token (buddy.core.codecs/bytes->hex (cr/rand-bytes 32))
            _ (db/insert-user-auth tr {:user-id (:id user)
                                       :signature (cr/sign auth-token)})
            _ (db/upsert-slack-token tr {:iv (:iv bot-token-enc)
                                         :salt (:salt bot-token-enc)
                                         :encrypted (:data bot-token-enc)
                                         :type :slack_token_type/bot
                                         :slack-id bot-id
                                         :slack-team-id team-id
                                         :scope bot-scope})
            _ (db/upsert-slack-token tr {:iv (:iv user-token-enc)
                                         :salt (:salt user-token-enc)
                                         :encrypted (:data user-token-enc)
                                         :type :slack_token_type/user
                                         :slack-id user-id
                                         :slack-team-id team-id
                                         :scope user-scope})]
          (u/spy user :after-login)
          {:auth-token auth-token}))))


(defn -build-cookie [auth-token]
  (format
    "auth_token=%s; Secure; HttpOnly; Max-age=%s; Path=/; Domain=%s; SameSite=Lax"
    auth-token
    (* 60 24 30)
    (config/v :domain)))


(defn login-slack
  "redirected here from slack's authorization"
  [req]
  (let [{:strs [code state]} (req :params)]
    (if (nil? code)
      (->text "Login failed. Please try logging in again")
      (if (not (valid-state-token? state))
        (->text "Login redirect token expired. Please try logging in again")
        (d/chain
          (slack/exchange-access code)
          #(u/spy % :after-exchange)
          (fn [auth-info] (d/future (-login-user auth-info)))
          (fn [{auth-token :auth-token}]
            (let [cookie (-build-cookie auth-token)]
              ;; todo: handle ?redirect= query param that we set when we redirected
              ;;       to our internal /login endpoint. This means that we need to
              ;;       update the slack-redirect "state" to be a composite key that
              ;;       has some base64 data encoded on the end of it.
              (->text "Welcome!"
                      :headers {:set-cookie cookie}))))))))


(defn parse-cmd [command]
  {})

(defn exec-cmd [command text]
  (let [opts (parse-cmd text)]
    (if-let [err (opts :errors)]
      {:result nil
       :error err}
      {:result (format "you said, \"%s\"" text)
       :error  nil})))


(defn slack-command
  "slash cmd webhook"
  [req]
  (let [req (u/parse-form-body req)
        body (:body req)
        _ (clojure.pprint/pprint body)
        {:strs [command text user_id channel_id response_url]} body]
    ;; todo: look up the user to tell them if we need them to login
    ;;       so we can get a slack-user-token that we can use to post
    ;;       on their behalf
    (t/info (json/encode {:pretty true}) {:body body})
    (t/info "slash command"
            {:command      command :text text
             :user-id      user_id :channel-id channel_id
             :response-url response_url})
    (a/go
      (let [_ (a/<! (a/timeout 100))
            {res :result} (exec-cmd command text)]
        @(slack/slash-respond response_url res)))
    (->resp)))


;; -- testing
(defn make-requests [n uri]
  (-> (fn [i]
        (d/chain
          (http/get uri {:pool ex/cp
                         :body (json/encode {:count i})})
          :body
          bs/to-string
          #(json/decode % true)
          :data
          #(json/decode % true)
          :count))
      (map (range n))))

(defn flob [req]
  (let [start (:aleph/request-arrived req)
        -count (-> req :params :count u/parse-int)]
    (->
      (d/chain
        (apply d/zip (make-requests -count "https://httpbin.org/delay/2"))
        (fn [resps] (->json {:elap  (-> (System/nanoTime)
                                        (- start)
                                        (/ 1000000.))
                             :resps (clojure.string/join "|" resps)})))
      (d/catch Exception #(t/error "no luck.." :exc-info %)))))


(defn delay-seconds [req]
  (let [delay-ms (-> req :params :seconds u/parse-int (* 1000))]
    (dtime/in delay-ms #(->json {:msg "yo"}))))
