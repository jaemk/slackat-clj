(ns slackat.crypto
    (:require [slackat.config :as config]
              [buddy.core.codecs :as codecs]
              [buddy.core.nonce :as nonce]
              [buddy.core.mac :as mac]
              [buddy.core.crypto :as crypto]
              [buddy.core.kdf :as kdf]))


(set! *warn-on-reflection* true)


(defn sign
  ([text]
   (sign text (config/v :signing-key)))
  ([text key]
   (-> (mac/hash text {:key key :alg :hmac+sha256})
       codecs/bytes->hex)))


(defn verify-sig
  ([text signature-hex]
   (verify-sig text signature-hex (config/v :signing-key)))
  ([text signature-hex key]
   (mac/verify text
               (codecs/hex->bytes signature-hex)
               {:key key :alg :hmac+sha256})))


(defn rand-bytes [n-bytes]
  (nonce/random-bytes n-bytes))


(defn new-salt []
  (rand-bytes 16))


(defn hash-password [raw-password salt take-bytes]
  (kdf/get-bytes
    (kdf/engine {:key raw-password
                 :salt salt
                 :alg :pbkdf2
                 :digest :sha512
                 :iterations 1e5})
    take-bytes))


(defn encrypt
  ([s]
   (let [salt (new-salt)]
     (encrypt s (config/v :encryption-key) salt)))
  ([clear-text password salt]
   (let [initialization-vector (rand-bytes 16)]
     {:data (codecs/bytes->hex
              (crypto/encrypt
                (codecs/to-bytes clear-text)
                (hash-password password salt 64)
                initialization-vector
                {:algorithm :aes256-cbc-hmac-sha512}))
      :salt salt
      :iv   (codecs/bytes->hex initialization-vector)})))


(defn decrypt
  ([data]
   (decrypt data (config/v :encryption-key)))
  ([{:keys [data iv salt]} password]
   (codecs/bytes->str
     (crypto/decrypt
       (codecs/hex->bytes data)
       (hash-password password salt 64)
       (codecs/hex->bytes iv)
       {:algorithm :aes256-cbc-hmac-sha512}))))
