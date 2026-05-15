(ns crossplay-words.core
  (:require [trie.core :as t]
            [cljs.pprint]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def request (atom ""))

(defn set-nope []
  (-> js/document
    (.getElementById "results")
    (.-innerHTML)
    (set! "No match found.")))

(defn set-loading []
  (-> js/document
      (.getElementById "results")
      (.-innerHTML)
      (set! "Loading...")))

(defn set-success-results [results search-text]
  (let [results-ps (map #(apply str "<p>" % "</p>") results)]
  (-> js/document
    (.getElementById "results")
    (.-innerHTML)
    (set! (str/join "" results-ps))
    )))

(defn set-results [results search-text]
  (if (or (empty? results) (= [nil] results)) (set-nope) (set-success-results results search-text)))

(defn set-error-more-than-two-blanks []
  (-> js/document
    (.getElementById "results")
    (.-innerHTML)
    (set! "Max of 2 blanks allowed.")
    ))

(defn rack-search [worker search-text] (.. worker (postMessage (clj->js {:query search-text :type "rack"}))))
(defn exact-match [worker search-text] (.. worker (postMessage (clj->js {:query search-text :type "exact"})))) 

(defn handle-rack-search [worker search-text]
  (if (> (count (filter #(= \? %) search-text)) 2)
    (set-error-more-than-two-blanks)
    (do
      (reset! request search-text)
      (rack-search worker search-text))))

(defn handle-exact-search [worker search-text]
  (reset! request search-text)
  (exact-match worker search-text))

(defn handle-search [worker _]
  (let [search-text (str/lower-case (-> js/document (.getElementById "searchbox") (.-value)))]
  (if (= "" search-text)
    (set-results nil [])
    (let [cheat-mode? (-> js/document
      (.getElementById "mode-toggle")
      (.-checked))]
    (set-loading)
    (if cheat-mode? (handle-rack-search worker search-text) (handle-exact-search worker search-text))))))

(defn set-dict-count [num-items]
  (-> js/document
    (.getElementById "dict-count")
    (.-innerHTML)
    (set! (str "Loaded " num-items " words into dictionary"))))

(defn show-search-box []
  (-> js/document
    (.getElementById "search-area")
    (.setAttribute "style" "display: block")))

(defn handle-result [search-text results]
  (if (= search-text @request) (set-results results search-text)))

(defn handle-worker-message [e]
  (let [msg (.-data e)
        type (.-type msg)]
    (if (= type "ready") (do
                           (set-dict-count (.-count msg))
                           (show-search-box))
        (handle-result (.-query msg) (js->clj (.-result msg))))))

(defn handle-with-worker [worker]
  (fn [e] (handle-search worker e)))

(defn on-load []
  (let [worker (js/Worker. "/js/worker.js")]
   (.. worker (addEventListener "message" handle-worker-message))
    
  (-> js/document
    (.getElementById "searchbox")
    (.addEventListener "input" (handle-with-worker worker)))

  (-> js/document
      (.getElementById "mode-toggle")
      (.addEventListener "input" (handle-with-worker worker)))))

(defn ^:export init []
  (js/document.addEventListener "DOMContentLoaded" on-load))
