(ns subzero.examples.click-counter.main
  (:require
    [subzero.core :as sz]
    [subzero.plugins.component-registry :as reg]
    [subzero.plugins.web-components :as wc]))

(def !db
 (doto (sz/create-db)
    (reg/install!)
    (wc/install! js/document js/customElements)))

(defn on-click
  [event]
  (let [increment-button (.-host (.-currentTarget event))
        clicks           (js/parseInt (.-clicks increment-button))]
    (set! (.-clicks increment-button) (inc clicks))))

(defn button-view
  [{:keys [clicks]}]
  [:root> {:#on {:click on-click}}
   [:button (str "Clicked " clicks " times")]])

(reg/reg-component !db :example/click-counter
  :view button-view
  :props #{:clicks})
