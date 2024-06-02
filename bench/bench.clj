(ns bench
  (:require [criterium.core :as c]
            [subzero.core :as sz]
            [subzero.plugins.html :as zh]
            [subzero.plugins.component-registry :as reg]
            [hiccup2.core :as h]
            [dev.onionpancakes.chassis.core :as ch]))

(set! *warn-on-reflection* true)

(defonce !db
  (doto (sz/create-db)
    (reg/install!)
    (zh/install!)))

(def normal-page
  [:html
   [:head
    [:title "Benchmarks"]
    [:link {:rel "stylesheet"
            :href "/css/styles.css"}]
    [:script {:src "/js/app.js"}]]
   [:body
    [:div.sidebar
     [:ul (for [li-no (range 15)]
            [:li [:a {:href (str "/detail/" li-no)}]])]]
    [:div.body
     (for [col ["a" "b" "c"]]
       [:div {:class col}
        (for [item (range 15)]
          [:p (str "Lorem ipsum " col item)])])]]])

(comment
  (do (require '[clj-async-profiler.core :as prof])
      (prof/profile (dotimes [_ 6000]
                      (zh/html !db normal-page)))
      (prof/serve-ui 8888)))

(defn bench-all!
  []
  (c/quick-bench (zh/html !db normal-page))
  (c/quick-bench (str (h/html normal-page)))
  (c/quick-bench (ch/html normal-page)))

(comment
  (bench-all!))

; Subzero
; --------------------------------------------------------------------------------
; eval (current-form): (c/quick-bench (zh/html !db normal-page))
; (out) Evaluation count : 2274 in 6 samples of 379 calls.
; (out)              Execution time mean : 297.104987 µs
; (out)     Execution time std-deviation : 31.541489 µs
; (out)    Execution time lower quantile : 272.399544 µs ( 2.5%)
; (out)    Execution time upper quantile : 336.541232 µs (97.5%)
; (out)                    Overhead used : 5.789356 ns

; Hiccup
; --------------------------------------------------------------------------------
; eval (current-form): (c/quick-bench (str (h/html normal-page)))
; (out) Evaluation count : 10590 in 6 samples of 1765 calls.
; (out)              Execution time mean : 60.134650 µs
; (out)     Execution time std-deviation : 6.510072 µs
; (out)    Execution time lower quantile : 56.203014 µs ( 2.5%)
; (out)    Execution time upper quantile : 71.287679 µs (97.5%)
; (out)                    Overhead used : 5.838632 ns
; (out) 
; (out) Found 1 outliers in 6 samples (16.6667 %)
; (out) 	low-severe	 1 (16.6667 %)
; (out)  Variance from outliers : 30.8644 % Variance is moderately inflated by outliers

; Chassis
; --------------------------------------------------------------------------------
; eval (current-form): (c/quick-bench (ch/html normal-page))
; (out) Evaluation count : 33588 in 6 samples of 5598 calls.
; (out)              Execution time mean : 18.856152 µs
; (out)     Execution time std-deviation : 1.391612 µs
; (out)    Execution time lower quantile : 17.919110 µs ( 2.5%)
; (out)    Execution time upper quantile : 21.242457 µs (97.5%)
; (out)                    Overhead used : 5.838632 ns
; (out) 
; (out) Found 1 outliers in 6 samples (16.6667 %)
; (out) 	low-severe	 1 (16.6667 %)
; (out)  Variance from outliers : 15.3386 % Variance is moderately inflated by outliers
