(ns slackat.handlers
  (:import [java.io File]
           [io.netty.buffer PooledSlicedByteBuf])
  (:require [taoensso.timbre :as t]
            [manifold.deferred :as d]
            [manifold.stream :as s]
            [manifold.time :as dtime]
            [byte-streams :as bs]
            [aleph.http :as http]
            [clojure.java.jdbc :as j]
            [clojure.java.io :as io]
            [clojure.core.async :as a]
            [cheshire.core :as json]
            [slackat.execution :as ex]
            [slackat.database.core :as db]
            [slackat.utils :as u :refer [->resp ->text ->json ->redirect]]
            [slackat.slack :as slack]
            [slackat.config :as config]))


(defn index [_]
  (->text "hello"))


(defn login
  "redirect to slack to login"
  [req]
  (-> (slack/build-redirect "token")
      ->redirect))


(defn login-slack
  "redirected here from slack's authorization"
  [req]
  (let [{:strs [code state]} (req :params)]
    (d/chain
      (slack/exchange-access code)
      (fn [resp] (t/info "access details" resp))
      (fn [_] (->json {:ok :ok})))))


(defn exec-cmd [command text]
  {:result (format "you said, \"%s\"" text)
   :error  nil})


(defn slack-command
  "slash cmd webhook"
  [req]
  (let [req (u/parse-form-body req)
        body (:body req)
        {:strs [command text user_id channel_id response_url]} body]
    (t/info (json/encode {:pretty true}) {:body body})
    (t/info "slash command"
            {:command      command :text text
             :user-id      user_id :channel-id channel_id
             :response-url response_url})
    (a/go
      (let [_ (a/<! (a/timeout 100))
            {res :result} (exec-cmd command text)]
        @(d/chain (http/post
                    response_url
                    {:headers {"content-type" "application/json; charset=utf-8"}
                     :body    (json/encode
                                {:replace_original true
                                 :delete_original  true
                                 :text             res
                                 :response_type    "ephemeral"})})
                  u/parse-form-body
                  clojure.pprint/pprint)))
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
