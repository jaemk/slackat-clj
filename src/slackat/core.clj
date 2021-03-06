(ns slackat.core
  (:require [taoensso.timbre :as t]
            [nrepl.server]
            [manifold.deferred :as d]
            [aleph.http :as http]
            [ring.middleware.params :as params]
            [ring.util.request :as ring-request]
            [compojure.response :refer [Renderable]]
            [slackat.router :as router]
            [slackat.utils :as u :refer [->resp]]
            [slackat.config :as config]
            [slackat.commands.core :as cmd])
  (:gen-class)
  (:import (java.net InetSocketAddress)
           (java.io Closeable)))


(set! *warn-on-reflection* true)


;; Make sure compojure passes through all
;; deferred objects to play nice with aleph
(extend-protocol Renderable
  manifold.deferred.IDeferred
  (render [d _] d))


(defn- wrap-deferred-request
  "Wrap deferred request with logging and error handling"
  [handler]
  (fn [request]
    (let [method (:request-method request)
          uri (:uri request)
          status (atom nil)
          start (:aleph/request-arrived request)]
      (-> request
          (d/chain
            handler
            (fn [resp]
              (reset! status (:status resp))
              resp))
          (d/catch
            Exception
            (fn [^Exception e]
              (let [info (ex-data e)]
                (if (nil? info)
                  (do
                    (reset! status 500)
                    (t/error "UNKNOWN ERROR"
                             {:type     nil
                              :exc-info e})
                    (->resp {:status 500 :body "Something went wrong"}))
                  (do
                    (reset! status (-> info :resp :status))
                    (t/error "ERROR"
                             {:type    (:type info)
                              :ex-data info})
                    (:resp info))))))
          (d/finally
            (fn []
              (let [elap-ms (-> (System/nanoTime)
                                (- start)
                                (/ 1000000.))]
                (t/info "completed request"
                        {:method          method
                         :uri             uri
                         :status          @status
                         :request-time-ms elap-ms}))))))))


(defn- wrap-query-params
  [handler]
  (fn [request]
    (let [enc (or (ring-request/character-encoding request)
                  "UTF-8")
          request (params/assoc-query-params request enc)]
      (handler request))))


(defn- init-server
  "Initialize server with middleware"
  [opts]
  (let [app (-> (router/load-routes)
                wrap-query-params
                wrap-deferred-request)]
    (http/start-server app opts)))


;; reloadable servers
(defonce ^:dynamic *http-server* (atom nil))
(defonce ^:dynamic *repl-server* (atom nil))


(defn start-server!
  "Start the server!"
  [& {:keys [port
             public]
      :or   {port   (config/v :app-port)
             public (config/v :app-public)}}]
  (let [host (if public "0.0.0.0" "127.0.0.1")
        addr (InetSocketAddress. ^String host ^Integer port)]
    (t/info "starting http server" {:addr addr})
    (reset! *http-server*
            (let [s (init-server {:socket-address addr
                                  :raw-stream?    true})]
              (t/info "started http server" {:addr addr})
              s))))


(defn stop-server!
  "Stop the server!"
  []
  (swap!
    *http-server*
    (fn [svr]
      (when (not (nil? svr))
        (.close ^Closeable svr)
        (t/info "server closed!"))
      svr)))


(defn restart-server!
  []
  (stop-server!)
  (start-server!))


(defn start-repl!
  [& {:keys [port
             public]
      :or   {port   (config/v :repl-port)
             public (config/v :repl-public)}}]
  (let [host (if public "0.0.0.0" "127.0.0.1")]
    (t/info "starting nrepl server"
            {:host host
             :port port})
    (reset! *repl-server*
            (nrepl.server/start-server :bind host :port port))))


(defn stop-repl!
  []
  (swap!
    *repl-server*
    (fn [svr]
      (when (not (nil? svr))
        (nrepl.server/stop-server svr)
        (t/info "nrepl-server closed!")))))


(defn reload
  "Reload a specific namespace"
  [ns']
  (use ns' :reload))


(defn -main
  [& _args]
  (t/info "checking config"
          {:db-max-connections (config/v :db-max-connections)
           :num-threads (config/v :num-threads)})
  (cmd/migrate!)
  (start-repl!)
  (start-server!))
