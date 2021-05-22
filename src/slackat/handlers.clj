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


(defn index [_]
  (->redirect "/login"))

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

(defn -upsert-user [resp]
  {})

(defn login-slack
  "redirected here from slack's authorization"
  [req]
  (let [{:strs [code state]} (req :params)]
    (if (not (valid-state-token? state))
      (->text "Login redirect token expired. Please trying logging in again")
      (d/chain
        (slack/exchange-access code)
        (fn [resp] (clojure.pprint/pprint resp))
        (fn [resp] -upsert-user)
        (fn [_] (->json {:ok :ok}))))))


(defn parse-time [s]
  (u/utc-now))


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
