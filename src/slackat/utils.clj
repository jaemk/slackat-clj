(ns slackat.utils
  (:require [taoensso.timbre :as t]
            [cheshire.core :as json]
            [byte-streams :as bs]
            [java-time :as jt]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.util UUID]
           [java.nio ByteBuffer]
           [org.apache.commons.codec.binary Hex]))


;; ---- response builders
(defn ->resp
  "Construct a response map
  Any kwargs provided are merged into a default 200 response"
  [& kwargs]
  (let [kwargs (apply hash-map kwargs)
        ct (or (:ct kwargs) "text/plain")
        kwargs (dissoc kwargs :ct)
        headers (or (get kwargs :headers) {})
        kwargs (dissoc kwargs :headers)
        default-headers {"content-type" ct}
        headers (merge default-headers headers)
        default {:status  200
                 :headers headers
                 :body    ""}]
    (merge default kwargs)))


(defn ->text [s & kwargs]
  (let [kwargs (apply hash-map kwargs)
        s (if (instance? String s) s (str s))
        headers (or (get kwargs :headers) {})
        kwargs (dissoc kwargs :headers)
        default-headers {"content-type" "text/plain"}
        headers (merge default-headers headers)]
    (merge
      {:status  200
       :headers headers
       :body    s}
      kwargs)))


(defn ->json [mapping & kwargs]
  (let [kwargs (apply hash-map kwargs)
        headers (or (get kwargs :headers) {})
        kwargs (dissoc kwargs :headers)
        default-headers {"content-type" "application/json"}
        headers (merge default-headers headers)]
    (merge
      {:status  200
       :headers headers
       :body    (json/encode mapping)}
      kwargs)))


(defn ->redirect [to]
  {:status  307
   :headers {"location" to}})


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
       ring.util.codec/form-decode
       (assoc r :body)))


;; ---- error builders
(defn ex-invalid-request!
  [& {:keys [e-msg resp-msg] :or {e-msg    "invalid request"
                                  resp-msg "invalid request"}}]
  (throw
    (ex-info e-msg
             {:type :invalid-request
              :msg  e-msg
              :resp (->resp :status 400 :body resp-msg)})))

(defn ex-unauthorized!
  [& {:keys [e-msg resp-msg] :or {e-msg    "unauthorized"
                                  resp-msg "unauthorized"}}]
  (throw
    (ex-info e-msg
             {:type :invalid-request
              :msg  e-msg
              :resp (->resp :status 401 :body resp-msg)})))

(defn ex-not-found!
  [& {:keys [e-msg resp-msg] :or {e-msg    "item not found"
                                  resp-msg "item not found"}}]
  (throw
    (ex-info e-msg
             {:type :invalid-request
              :msg  e-msg
              :resp (->resp :status 404 :body resp-msg)})))

(defn ex-does-not-exist! [record-type]
  (let [msg (format "%s does not exist" record-type)]
    (throw
      (ex-info
        msg
        {:type  :does-not-exist
         :cause record-type
         :msg   msg
         :resp  (->resp :status 404 :body "item not found")}))))

(defn ex-error!
  [e-msg & {:keys [resp-msg cause] :or {resp-msg "something went wrong"
                                        cause    nil}}]
  (throw
    (ex-info
      e-msg
      {:type  :internal-error
       :cause cause
       :msg   e-msg
       :resp  (->resp :status 500 :body resp-msg)})))


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


;; ---- general
(defn spy
  ([arg] (spy arg nil))
  ([arg desc]
   (let [desc (if (nil? desc) "" (str ": " desc))]
     (println (format "=============== [SPY%s] ===============" desc))
     (clojure.pprint/pprint arg)
     (println "=====================================")
     arg)))


(defn uuid []
  (UUID/randomUUID))


(defn format-uuid [^UUID uuid]
  (-> uuid .toString (.replace "-" "")))


(defn parse-uuid [^String uuid-str]
  (when (some? uuid-str)
    (try
      (-> (Hex/decodeHex uuid-str)
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
