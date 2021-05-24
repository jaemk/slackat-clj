(ns slackat.config
  (:require [taoensso.timbre :as t]
            [cheshire.core :as json]
            [cheshire.generate :refer [add-encoder encode-str remove-encoder]]
            [slackat.utils :as u])
  (:import (java.time Instant)
           (java.time ZoneId)
           (java.time.format DateTimeFormatter)
           (java.io StringWriter)
           (java.io PrintWriter)))


(defmacro app-version []
  (-> "project.clj" slurp read-string (nth 2)))


(defn env [k & {:keys [default parse]
                :or {default nil
                     parse identity}}]
  (if-let [value (System/getenv k)]
    (parse value)
    default))


(def num-cpus (.availableProcessors (Runtime/getRuntime)))

(defn not-nil [msg x]
  (if (nil? x)
    (u/ex-error! msg)
    x))

(def values
  (delay
    {
     :db-host     (env "DATABASE_HOST")
     :db-port     (env "DATABASE_PORT")
     :db-name     (env "DATABASE_NAME")
     :db-user     (env "DATABASE_USER")
     :db-password (env "DATABASE_PASSWORD")
     :app-port    (env "PORT" :default 3003 :parse u/parse-int)
     :app-public  (env "PUBLIC" :default false :parse u/parse-bool)
     :repl-port   (env "REPL_PORT" :default 3999 :parse u/parse-int)
     :repl-public (env "REPL_PUBLIC" :default false :parse u/parse-bool)
     :pretty-logs (env "PRETTY_LOGS" :default true :parse u/parse-bool)
     :max-client-connections
                  (env "MAX_CLIENT_CONNECTIONS" :default 2000 :parse u/parse-int)
     :max-client-connections-per-host
                  (env "MAX_CLIENT_CONNECTIONS_PER_HOST" :default 500 :parse u/parse-int)
     :keep-alive-client-connections
                  (env "KEEP_ALIVE_CLIENT_CONNECTIONS" :default true :parse u/parse-bool)
     :keep-alive-client-timeout-ms
                  (env "KEEP_ALIVE_CLIENT_TIMEOUT_MS" :default 5000 :parse u/parse-int)

     :domain      (env "SLACKAT_DOMAIN" :parse (partial not-nil "SLACKAT_DOMAIN is a required env var"))
     :host        (env "SLACKAT_HOST" :parse (partial not-nil "SLACKAT_HOST is a required env var"))

     :slack-client-id
                  (env "SLACK_CLIENT_ID")
     :slack-client-secret
                  (env "SLACK_CLIENT_SECRET")

     :encryption-key
                  (env "ENCRYPTION_KEY" :parse (partial not-nil "ENCRYPTION_KEY is a required env var"))
     :signing-key (env "SIGNING_KEY" :parse (partial not-nil "SIGNING_KEY is a required env var"))

     :slack-user-token
                  (env "SLACK_USER_TOKEN")
     :slack-user-id
                  (env "SLACK_USER_ID")

     :app-version (app-version)
     :num-cpus    num-cpus
     :num-threads (* num-cpus
                     (env "THREAD_MULTIPLIER" :default 8 :parse u/parse-int))}))


(defn v
  [k & {:keys [default]
        :or {default nil}}]
  (if-let [value (get @values k)]
    value
    default))


(def utc-zone (ZoneId/of "UTC"))
(def ny-zone (ZoneId/of "America/New_York"))


;; -- Structured logging
(defonce ^:dynamic *pretty-console-logs* (atom (v :pretty-logs)))

(defn fmt-exc [e]
  (let [sw (StringWriter.)
        pw (PrintWriter. sw)]
    (.printStackTrace e pw)
    (.toString sw)))

(defn fmt-exc-info [data]
  (if-let [e (:exc-info data)]
    (-> (dissoc data :exc-info)
        (assoc :_/exc-info (fmt-exc e)))
    data))

(defn log-args->map [log-args]
  (let [n-args (count log-args)
        -first (first log-args)
        -second (second log-args)
        data {:event nil}]
    (cond
      (= n-args 0)            data
      (and (= n-args 1)
           (map? -first))     (merge data -first)
      (and (= n-args 1)
           (string? -first))  {:event -first}
      (and (= n-args 2)
           (string? -first)
           (map? -second))    (merge {:event -first} -second)
      :else (merge data {:args log-args}))))

(defn build-log-map [data]
  (let [{:keys [level instant config vargs ?ns-str ?line ?msg-fmt]} data
        real-instant (.toInstant instant)
        utc-time (.atZone real-instant utc-zone)
        local-time (.atZone real-instant ny-zone)
        timestamp-utc (.format utc-time DateTimeFormatter/ISO_OFFSET_DATE_TIME)
        timestamp-local (.format local-time DateTimeFormatter/ISO_OFFSET_DATE_TIME)
        log-data (if-not (nil? ?msg-fmt)
                   {:event (apply format ?msg-fmt vargs)}
                   (log-args->map vargs))]
    (-> {:_/level level
         :_/timestamp timestamp-utc
         :_/timestamp-local timestamp-local
         :_/source-namespace ?ns-str
         :_/source-namespace-line ?line}
        (merge log-data)
        fmt-exc-info)))

(defn fmt-log-data [data]
  (->> (seq data)
       (remove (fn [[k v]]
                 (-> (namespace k)
                     (= "_"))))
       (map #(clojure.string/join "=" %))
       (clojure.string/join " ")))

(defn fmt-log [data]
  (if-not @*pretty-console-logs*
    (json/encode data)
    (let [{:keys [_/timestamp
                  _/level
                  _/source-namespace
                  _/source-namespace-line
                  _/exc-info
                  event]} data]
      (format "%-25s [%-7s] %-20s [%s:%d] %s%s"
              timestamp
              (name level)
              event
              source-namespace
              source-namespace-line
              (fmt-log-data data)
              (if exc-info
                (str " " exc-info)
                "")))))


;; override the log formatter
(t/merge-config!
  {:output-fn (fn [d]
                (->> d
                     build-log-map
                     (into (sorted-map))
                     fmt-log))})


;; -- json encoder additions
(add-encoder java.net.InetSocketAddress
             (fn [d jsonGenerator]
               (.writeString jsonGenerator (str d))))
