(ns crossplay-words.worker
  (:require [trie.core :as t]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def dict (atom (t/trie [])))

(defn exact-match [search-text]
  (get @dict search-text))

(def a-to-z (map char (range 97 123)))

(defn add-all-az [rack]
  (map #(str rack %) a-to-z))

(defn append-all [racks appends-left]
  (if (zero? appends-left)
    racks
    (append-all (mapcat add-all-az racks) (dec appends-left))))

(defn get-racks [search-text]
  (let [non-blank-text (apply str (remove #(= \? %) search-text))
        num-appends (abs (- (count search-text) (count non-blank-text)))]
  (append-all [non-blank-text] num-appends)))

(defn rack-permutations [search-text]
  (let [racks (get-racks search-text)]
    (map #(apply str %) (mapcat #(combo/permutations %) racks))))

(defn rack-search [search-text]
  (let [racks (rack-permutations search-text)
        results (map exact-match racks)]
    (filter #(not (nil? %)) results)))

(defn get-matches [type query]
  (if (= type "rack") (rack-search query) [(exact-match query)]))

(defn handle-message [e]
  (let [msg (.-data e)
        query (.-query msg)
        type (.-type msg)
        result (get-matches type query)]
        (js/postMessage (clj->js {:type "result" :query query :result result}))))

(defn init []
  (-> (js/fetch "/words.txt")
      (.then #(.text %))
      (.then (fn [word-data]
               (reset! dict (t/trie (str/split word-data #"\n")))
               (js/postMessage (clj->js {:type "ready" :count (count @dict)})))))
  (js/self.addEventListener "message" handle-message))
