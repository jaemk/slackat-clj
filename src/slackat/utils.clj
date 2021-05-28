(ns slackat.utils
  (:require [taoensso.timbre :as t]
            [cheshire.core :as json]
            [byte-streams :as bs]
            [java-time :as jt]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [ring.util.codec :as ring-codec]
            [buddy.core.codecs :as codecs])
  (:import [java.util UUID]
           [java.nio ByteBuffer]
           (org.ocpsoft.prettytime.nlp PrettyTimeParser)))


;; ---- response builders
(defn- extract-header-opts [opts default-headers]
  (let [headers (or (get opts :headers) {})
        opts (dissoc opts :headers)
        headers (merge default-headers headers)]
    [opts headers]))

(defn ->resp
  "Construct a response map
  Any options provided are merged into a default 200 response"
  ([] ->resp {})
  ([opts]
   (let [ct (or (:ct opts) "text/plain")
         opts (dissoc opts :ct)

         [opts headers] (extract-header-opts opts {"content-type" ct})
         default {:status  200
                  :headers headers
                  :body    ""}]
     (merge default opts))))

(defn ->text
  ([s] (->text s {}))
  ([s opts]
   (let [s (if (instance? String s) s (str s))
         [opts headers] (extract-header-opts opts {"content-type" "text/plain"})]
     (merge
       {:status  200
        :headers headers
        :body    s}
       opts))))

(defn ->json
  ([mapping] (->json mapping {}))
  ([mapping opts]
   (let [[opts headers] (extract-header-opts opts {"content-type" "application/json"})]
     (merge
       {:status  200
        :headers headers
        :body    (json/encode mapping)}
       opts))))

(defn ->redirect
  ([to] (->redirect to {}))
  ([to opts]
   (let [[opts headers] (extract-header-opts opts {"location" to})]
     (merge
       {:status  307
        :headers headers}
       opts))))


;; ---- http (server & client) utils
(defn parse-json-body [r]
  (->> r
       :body
       bs/to-string
       json/decode
       (assoc r :body)))

(defn parse-form-body [r]
  (->> r
       :body
       bs/to-string
       ring-codec/form-decode
       (assoc r :body)))


;; ---- error builders
(defn ex-invalid-request!
  [& {:keys [e-msg resp-msg] :or {e-msg    "invalid request"
                                  resp-msg "invalid request"}}]
  (throw
    (ex-info e-msg
             {:type :invalid-request
              :msg  e-msg
              :resp (->resp {:status 400 :body resp-msg})})))

(defn ex-unauthorized!
  [& {:keys [e-msg resp-msg] :or {e-msg    "unauthorized"
                                  resp-msg "unauthorized"}}]
  (throw
    (ex-info e-msg
             {:type :invalid-request
              :msg  e-msg
              :resp (->resp {:status 401 :body resp-msg})})))

(defn ex-not-found!
  [& {:keys [e-msg resp-msg] :or {e-msg    "item not found"
                                  resp-msg "item not found"}}]
  (throw
    (ex-info e-msg
             {:type :invalid-request
              :msg  e-msg
              :resp (->resp {:status 404 :body resp-msg})})))

(defn ex-does-not-exist! [record-type]
  (let [msg (format "%s does not exist" record-type)]
    (throw
      (ex-info
        msg
        {:type  :does-not-exist
         :cause record-type
         :msg   msg
         :resp  (->resp {:status 404 :body "item not found"})}))))

(defn ex-error!
  [e-msg & {:keys [resp-msg cause] :or {resp-msg "something went wrong"
                                        cause    nil}}]
  (throw
    (ex-info
      e-msg
      {:type  :internal-error
       :cause cause
       :msg   e-msg
       :resp  (->resp {:status 500 :body resp-msg})})))


;; ---- macros
(defmacro get-some->
  "Build a 'get' chain:
      (get-some-> {\\\"a\\\" {\\\"b\\\" {:c 1}}}
                  \"a\"
                  \"b\"
                  :c)
  Becomes:
      (some-> {\"a\" {\"b\" {:c 1}}}
              (get \"a\")
              (get \"b\")
              (get :c)
  "
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (get ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))


;; ---- time
(defn utc-now []
  (jt/zoned-date-time (jt/instant) "UTC"))


(defn dt->unix-seconds [dt]
  (-> dt
      jt/to-millis-from-epoch
      (/ 1000)
      int))


(defn parse-time
  "Parse a human string to a utc datetime.
  Ex. 'in 2 minutes'  '7pm tomorrow'"
  [s]
  (some-> (new PrettyTimeParser)
          (.parse s)
          first
          (jt/zoned-date-time "UTC")))


;; ---- general
(defn spy
  ([arg] (spy arg nil))
  ([arg desc]
   (let [desc (if (nil? desc) "" (str ": " desc))]
     (println (format "=============== [SPY%s] ===============" desc))
     (pprint/pprint arg)
     (println "=====================================")
     arg)))


(defn trim-to-nil [s]
  (some-> s
          string/trim
          (#(if (empty? %) nil %))))


;; -- base64 map serialization
(defn b64-str->bytes
  "convert a base64 string to decoded bytes"
  [^String s]
  (buddy.core.codecs/b64u->bytes (.getBytes s)))

(defn map->b64-str
  "encode a json-able map to a url-safe base64 string"
  [data]
  (-> (json/encode data)
      codecs/str->bytes
      codecs/bytes->b64u
      codecs/bytes->str))

(defn b64-str->map
  "decode a url-safe base64 string to a map"
  [^String s]
  (-> (b64-str->bytes s)
      codecs/bytes->str
      json/decode))


;; -- uuid
(defn uuid []
  (UUID/randomUUID))

(defn format-uuid [^UUID uuid]
  (-> uuid .toString (.replace "-" "")))

(defn parse-uuid [^String uuid-str]
  (when (some? uuid-str)
    (try
      (-> (codecs/hex->bytes uuid-str)
          ((fn [^"[B" buf]
             (if (not (= 16 (alength buf)))
               (throw (Exception. "invalid uuid"))
               buf)))
          (ByteBuffer/wrap)
          ((fn [^ByteBuffer buf]
             (UUID. (.getLong buf) (.getLong buf)))))
      (catch Exception e
        (t/error {:exc-info e})
        (throw (Exception. "Invalid uuid"))))))


(defn parse-int [s]
  (Integer/parseInt s))


(defn parse-bool [s]
  (Boolean/parseBoolean s))


(defn kebab->under [s]
  (string/replace s "-" "_"))


(defn under->kebab [s]
  (string/replace s "_" "-"))


(defn pad-vec
  ([coll size] (pad-vec coll size nil))
  ([coll size pad]
   (as-> coll v
         (concat v (repeat pad))
         (take size v)
         (vec v))))
