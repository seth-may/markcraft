(ns app.core
  (:require [clojure.core.async :as async :refer [go go-loop chan <! >! <!! >!!]]
            [clojure.spec.alpha :as s]))

;; --- Spec Validation ---
(s/def ::name (s/and string? #(> (count %) 0)))
(s/def ::age (s/and int? #(> % 0) #(< % 150)))
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/def ::user (s/keys :req-un [::name ::age ::email]))

;; --- Transducer Pipeline ---
(def xf-pipeline
  (comp
    (filter #(> (:score %) 0))
    (map #(update % :score * 1.1))
    (partition-by :category)
    (mapcat #(take 5 (sort-by :score > %)))))

;; --- Actor System with core.async ---
(defn create-actor [initial-state handler]
  (let [mailbox (chan 1024)
        state (atom initial-state)]
    (go-loop []
      (when-let [msg (<! mailbox)]
        (let [[new-state reply] (handler @state msg)]
          (reset! state new-state)
          (when reply (>! (:reply-to msg) reply)))
        (recur)))
    {:send (fn [msg] (>!! mailbox msg))
     :state (fn [] @state)}))

;; --- Memoized Recursive with Protocol ---
(defprotocol ICache
  (lookup [this key])
  (store [this key value]))

(defrecord LRUCache [capacity items order]
  ICache
  (lookup [this key]
    (when-let [v (get items key)]
      [v (-> this
             (update :order #(cons key (remove #{key} %))))]))
  (store [this key value]
    (if (>= (count items) capacity)
      (let [evict (last order)]
        (-> this
            (update :items dissoc evict)
            (update :items assoc key value)
            (update :order #(cons key (butlast (remove #{evict} %))))))
      (-> this
          (update :items assoc key value)
          (update :order #(cons key %))))))

;; --- Ring Middleware ---
(defn wrap-logging [handler]
  (fn [request]
    (let [start (System/nanoTime)
          response (handler request)
          elapsed (/ (- (System/nanoTime) start) 1e6)]
      (println (format "%s %s %.2fms" (:method request) (:uri request) elapsed))
      response)))
