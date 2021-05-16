(ns slackat.crypto
    (:require [buddy.core.codecs :as codecs]
              [buddy.core.nonce :as nonce]
              [buddy.core.crypto :as crypto]
              [buddy.core.kdf :as kdf]))


(set! *warn-on-reflection* true)


(defn rand-bytes [n-bytes] (nonce/random-bytes n-bytes))

(defn new-salt [] (rand-bytes 16))

(defn hash-password [raw-password salt take-bytes]
  (kdf/get-bytes
    (kdf/engine {:key raw-password
                 :salt salt
                 :alg :pbkdf2
                 :digest :sha512
                 :iterations 1e5})
    take-bytes))


(defn encrypt
  [clear-text password salt]
  (let [initialization-vector (rand-bytes 16)]
    {:data (codecs/bytes->hex
             (crypto/encrypt
               (codecs/to-bytes clear-text)
               (hash-password password salt 64)
               initialization-vector
               {:algorithm :aes256-cbc-hmac-sha512}))
     :iv (codecs/bytes->hex initialization-vector)}))


(defn decrypt
  [{:keys [data iv]} password salt]
  (codecs/bytes->str
    (crypto/decrypt
      (codecs/hex->bytes data)
      (hash-password password salt 64)
      (codecs/hex->bytes iv)
      {:algorithm :aes256-cbc-hmac-sha512})))