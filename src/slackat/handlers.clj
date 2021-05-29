(ns slackat.handlers
  (:require [taoensso.timbre :as t]
            [manifold.deferred :as d]
            [manifold.time :as time]
            [java-time :as jt]
            [clojure.java.jdbc :as j]
            [clojure.core.cache :as cache]
            [clojure.walk :as walk]
            [buddy.core.codecs :as codecs]
            [slackat.database.core :as db]
            [slackat.crypto :as cr]
            [slackat.utils :as u :refer [->resp ->text ->json ->redirect]]
            [slackat.slack :as slack]
            [slackat.config :as config]
            [clojure.string :as string])
  (:import (java.time Instant)
           (java.sql Timestamp)))


(defn user-for-req
  "load a user based on their auth token, or null"
  [req]
  (let [cookie (u/get-some-> req :headers :cookie)
        [_ token] (some->> cookie (re-find #"auth_token=(\w+)"))
        user-auth (some->> token cr/sign (db/get-user-for-auth (db/conn)))
        user-id (some-> user-auth :id)
        ^Instant created (some-> user-auth :created ((fn [^Timestamp ts] (.toInstant ts))))
        ^Instant month-ago (jt/instant (jt/minus (u/utc-now) (jt/days 30)))]
    (if (or (nil? created) (.isBefore created month-ago))
      (do
        (t/info "cookie expired" {:created created :user-id user-id})
        nil)
      user-id)))


(defn index [req]
  (d/chain
    (d/future (user-for-req req))
    (fn [user]
      (if (some? user)
        (->text "Welcome back!")
        (->redirect "/login?redirect=/")))))


(defn status [_]
  (->json {:status  :ok
           :version (config/v :app-version)}))


(defn not-found [_]
  (->resp {:body   "nothing to see here"
           :status 404}))

;; -- slack authentication unique state token store
;; 2m ttl of one-time state-tokens used for slack logins
(defonce state-tokens (atom (cache/ttl-cache-factory {} :ttl 120000)))

(defn new-state-token
  "Make a new one-time state-token and cache it temporarily"
  []
  (-> (cr/rand-bytes 32)
      codecs/bytes->hex
      ((fn [hex]
         (swap! state-tokens (fn [s]
                               (cache/evict s hex)
                               (cache/through-cache s hex (constantly nil))))
         hex))))

(defn valid-state-token?
  "Check if a state-token is still valid (still cached).
  Expire the token so it can't be used again."
  [token]
  (let [valid (atom false)]
    (swap!
      state-tokens
      (fn [s]
        (swap!
          valid
          (fn [_]
            (cache/has? s token)))
        (cache/evict s token)))
    @valid))


;; -- formatting and parsing state tokens used for stripe logins
(defn format-state-str
  "Format a state-token and metadata map as
  $state-token.$metadata-as-b64"
  ([state-token] (format-state-str state-token {}))
  ([state-token metadata]
   (let [metadata (or metadata {})
         meta-str (-> metadata u/map->b64-str cr/encrypt u/map->b64-str)]
     (format "%s.%s" state-token meta-str))))

(defn parse-state-str
  "Parse a formatted state token string into the
  separate state-token and metadata parts"
  [state-str]
  (let [[state-token metadata] (string/split state-str #"\.")]
    (if (or (nil? state-token) (nil? metadata))
      (u/ex-error! (format "Found invalid state token in slack login redirect: %s" state-str))
      [state-token (-> metadata
                       u/b64-str->map
                       walk/keywordize-keys
                       cr/decrypt
                       u/b64-str->map)])))


(defn login
  "Initiate slack login/oauth request.
  Redirect to slack's auth endpoint with our state and metadata."
  [req]
  ;(u/spy req :on-login)
  (let [redirect (u/get-some-> req :query-params "redirect")
        state-str (format-state-str (new-state-token) {"redirect" redirect})]
    (-> (slack/build-redirect state-str)
        ->redirect)))


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
(defn save-user-get-cookie
  "Given slack auth info about a user, upsert a user and
  their api tokens and cookie in the db, return the new auth cookie"
  [slack-auth-info]
  (let [sla slack-auth-info
        bot-id (sla "bot_user_id")
        bot-token (sla "access_token")
        bot-scope (some-> (sla "scope") u/trim-to-nil (string/split #","))
        team-id (u/get-some-> sla "team" "id")
        user-id (u/get-some-> sla "authed_user" "id")
        user-token (u/get-some-> sla "authed_user" "access_token")
        user-scope (some-> (u/get-some-> sla "authed_user" "scope")
                           u/trim-to-nil
                           (string/split #","))

        bot-token-enc (cr/encrypt bot-token)
        user-token-enc (cr/encrypt user-token)]
    (j/with-db-transaction
      [tr (db/conn)]
      (let [user (db/upsert-user tr {:slack-id      user-id
                                     :slack-team-id team-id})
            auth-token (buddy.core.codecs/bytes->hex (cr/rand-bytes 32))
            _ (db/insert-user-auth tr {:user-id   (:id user)
                                       :signature (cr/sign auth-token)})
            _ (db/upsert-slack-token tr {:iv            (:iv bot-token-enc)
                                         :salt          (:salt bot-token-enc)
                                         :encrypted     (:data bot-token-enc)
                                         :type          "bot"
                                         :slack-id      bot-id
                                         :slack-team-id team-id
                                         :scope         bot-scope})
            _ (db/upsert-slack-token tr {:iv            (:iv user-token-enc)
                                         :salt          (:salt user-token-enc)
                                         :encrypted     (:data user-token-enc)
                                         :type          "user"
                                         :slack-id      user-id
                                         :slack-team-id team-id
                                         :scope         user-scope})]
        ;(u/spy user :after-login)
        {:auth-token auth-token}))))


(defn build-cookie [auth-token]
  (format
    "auth_token=%s; Secure; HttpOnly; Max-age=%s; Path=/; Domain=%s; SameSite=Lax"
    auth-token
    (* 60 24 30)
    (config/v :domain)))


(defn login-slack
  "redirected here from slack's authorization"
  [req]
  ;(u/spy req :on-slack-redirect)
  (let [{:strs [code state]} (req :params)
        [state metadata] (parse-state-str state)]
    (if (nil? code)
      (->text "Login failed. Please try logging in again")
      (if (not (valid-state-token? state))
        (->text "Login redirect token expired. Please try logging in again")
        (d/chain
          ;; get the user info and api token for this user
          (slack/exchange-access code)
          ;#(u/spy % :after-exchange)

          ;; upsert this user's (and team's) api tokens
          (fn [auth-info] (d/future (save-user-get-cookie auth-info)))

          ;; generate a new auth cookie and handle any post-login redirects
          (fn [{auth-token :auth-token}]
            (let [cookie (build-cookie auth-token)
                  headers {:set-cookie cookie}]
              ;(u/spy metadata :state-metadata)
              (if-let [redirect (u/get-some-> metadata "redirect")]
                ;; handle ?redirect= query param that we set when we did our initial _internal_
                ;; redirect from some page (e.g. /) to /login?redirect=/
                (->redirect redirect {:headers headers})
                (->text "Welcome!" {:headers headers})))))))))


(def login-url (format "%s/login" (config/v :host)))


(def help
  "Ex. /at 5a tomorrow *send* Good Morning!")

(defn needs-help? [s]
  (->> ["h" "help" "-h" "--help" "" nil]
       (map = (repeat s))
       (filter true?)
       seq))

(defn is-list-cmd? [s]
  (->> ["l" "list" "-l" "--list"]
       (map = (repeat s))
       (filter true?)
       seq))


(def CANCEL-RE #"cancel\s?(.*)\s?")


(defn parse-cmd-str
  "Parse a command-string into the {:text :post-at} that should be
  scheduled and a message that should be sent back immediately
  as an ephemeral response"
  [command-text]
  (let [s (-> command-text string/lower-case u/trim-to-nil)
        [is-cancel cancel-id] (re-find CANCEL-RE s)
        cancel-id (u/trim-to-nil cancel-id)
        send-s (if (re-find #"send" s) s nil)
        [time-str text] (some-> send-s (string/split #"send" 2))
        time (some-> time-str u/parse-time)
        text (some-> text string/triml)
        cmd {:command  {:type nil
                        :args nil}
             :response nil}]
    (cond
      (needs-help? s) (assoc-in cmd [:command :type] :help)
      (is-list-cmd? s) (assoc-in cmd [:command :type] :list)
      is-cancel (-> cmd
                    (assoc-in [:command :type] :cancel)
                    (assoc-in [:command :args] {:message-id cancel-id}))
      :else (-> cmd
                (assoc-in [:command :type] :schedule)
                (assoc-in [:command :args] {:text text :post-at time})
                (assoc :response (format "Scheduled \"%s\" to be sent at %s" text time))))))

(defn parse-cmd
  [params]
  (let [{:strs [text user_id team_id channel_id response_url]} params
        parsed {:slack-channel-id channel_id
                :slack-user-id    user_id
                :slack-team-id    team_id
                :response-url     response_url
                ;; will be populated if we can find a user api token
                :api-token        nil
                ;; will be populated with a response to send to the response-url
                :response         nil
                ;; :schedule / :help / :list
                :command          nil}
        token (db/get-slack-token-for-user (db/conn) user_id team_id)]
    (if (nil? token)
      parsed
      (let [api-token (cr/decrypt {:data (:encrypted token)
                                   :iv   (:iv token)
                                   :salt (:salt token)})
            {:keys [command response]} (parse-cmd-str text)]
        (assoc parsed
          :api-token api-token
          :command command
          :response response)))))


(defn slack-resp->err-msg
  [resp]
  (when-let [err (u/get-some-> resp :body "error")]
    (format "Yikes, something went wrong: %s" err)))


(defn fmt-msg [msg]
  (let [{:strs [id post_at text]} msg]
    (format "%s [%s]: %s" id (u/unix-seconds->dt post_at) text)))


(defn handle-list-cmd
  [parsed]
  (let [{:keys [command api-token slack-channel-id]} parsed
        {:keys [type]} command]
    (assert (= type :list) (format "expected :list, got %s" type))
    (d/chain
      (slack/list-messages api-token {:channel slack-channel-id})
      (fn [resp]
        (let [scheduled (u/get-some-> resp :body "scheduled_messages")]
          (->> (map fmt-msg scheduled)
               (string/join "\n")
               (str "Scheduled:\n")
               (assoc parsed :response)))))))

(defn handle-cancel-cmd
  [parsed]
  (let [cancel-id (u/get-some-> parsed :command :args :message-id)]
    (if cancel-id
      (assoc parsed :response (format "Cancelled %s!" cancel-id))
      (assoc parsed :response (format "Cancel what?")))))

(defn handle-schedule-cmd
  [parsed]
  (let [{:keys [command api-token slack-channel-id]} parsed
        {:keys [type args]} command
        {:keys [text post-at]} args]
    (assert (= type :schedule) (format "expected :schedule, got %s" type))
    (d/chain
      (slack/schedule-message {:channel slack-channel-id
                               :token   api-token
                               :text    text
                               :post-at post-at})
      slack-resp->err-msg
      (fn [err-msg]
        (if err-msg
          (assoc parsed
            :response err-msg)
          parsed)))))


(defn unhandled-command
  [parsed]
  (t/error (format "unhandled command %s" (parsed :command)))
  (assoc parsed :response (format "Whoops. I didn't understand that command")))


(defn handle-parsed-cmd
  [parsed]
  (let [{:keys [command]} parsed
        {:keys [type]} command]
    (case type
      :help (assoc parsed :response help)
      :list (handle-list-cmd parsed)
      :cancel (handle-cancel-cmd parsed)
      :schedule (handle-schedule-cmd parsed)
      (unhandled-command parsed))))


(defn handle-cmd
  [params]
  (->
    (d/chain (future (parse-cmd params))
             (fn [parsed]
               (if (nil? (:api-token parsed))
                 (assoc parsed
                   :response (format "Whoops, looks like you need to login at %s" login-url))
                 (handle-parsed-cmd parsed)))
             (fn [{:keys [response-url response]}]
               (slack/slash-respond response-url response)))
    (d/catch
      Exception
      (fn [^Exception e]
        (t/error (format "error in command processor: %s" e) {:user-id (params :user_id)})))))

(defn slack-command
  "slash cmd webhook"
  [req]
  (let [req (u/parse-form-body req)
        body (:body req)]
    (time/in
      ;; spawn the cmd processor in the background, waiting
      ;; a short while to let the webhook respond before we
      ;; send a processed result.
      ;; Time in ms
      100
      (fn []
        (handle-cmd body)))
    (->resp)))
