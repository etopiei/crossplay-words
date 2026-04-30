(ns crossplay-words.core
  (:require [trie.core :as t]
            [cljs.pprint]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def dict (atom (t/trie [])))

(defn bold-blanks [search-text result]
  ;; This is actually a little tricky with duplicate chars, maybe need a loop recur that 'uses up' chrs.
  ;; Maybe come back to this later, for now we don't have to bold blanks.
  result)

(defn set-nope []
  (-> js/document
    (.getElementById "results")
    (.-innerHTML)
    (set! "No match found.")))

(defn set-success-results [results search-text]
  (let [results-ps (map #(apply str "<p>" % "</p>") (map #(bold-blanks search-text %) results))]
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

(defn handle-rack-search [search-text]
  (if (> (count (filter #(= \? %) search-text)) 2)
    (set-error-more-than-two-blanks)
    (set-results (rack-search search-text) search-text)))

(defn handle-exact-search [search-text]
  (set-results [(exact-match search-text)] search-text))

(defn handle-search [_]
  (let [search-text (str/lower-case (-> js/document (.getElementById "searchbox") (.-value)))]
  (if (= "" search-text)
    (set-results nil [])
    (let [cheat-mode? (-> js/document
      (.getElementById "mode-toggle")
      (.-checked))]
    (if cheat-mode? (handle-rack-search search-text) (handle-exact-search search-text))))))

(defn set-dict-count []
  (-> js/document
    (.getElementById "dict-count")
    (.-innerHTML)
    (set! (str "Loaded " (count @dict) " words into dictionary"))))

(defn show-search-box []
  (-> js/document
    (.getElementById "search-area")
    (.setAttribute "style" "display: block")))

(defn on-load []
  (-> (js/fetch "/words.txt")
    (.then #(.text %))
    (.then (fn [word-data]
      (reset! dict (t/trie (str/split word-data #"\n")))
      (set-dict-count)
      (show-search-box))))

  (-> js/document
    (.getElementById "searchbox")
    (.addEventListener "input" handle-search))

  (-> js/document
      (.getElementById "mode-toggle")
      (.addEventListener "input" handle-search)))

(defn ^:export init []
  (js/document.addEventListener "DOMContentLoaded" on-load))
