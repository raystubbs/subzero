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
  [& _]
  (c/quick-bench (zh/html !db normal-page))
  (c/quick-bench (str (h/html normal-page)))
  (c/quick-bench (ch/html normal-page)))

(comment
  (bench-all!))
