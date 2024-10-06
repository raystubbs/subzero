(ns subzero.examples.animations.main
  (:require
   [subzero.core :as sz]
   [subzero.plugins.component-registry :as reg]
   [subzero.plugins.web-components :as wc]))

(defonce !db
  (doto (sz/create-db)
    (reg/install!)
    (wc/install! js/document js/customElements)))

(defonce !state (atom {}))

(defn demo-1-view
  [{:keys [state]}]
  (let [showing? (get-in state [::demo-1 :showing?])]
    [:root>
     :#style {:display :flex :margin "1rem"}
     [:button
      :#on {:click #(swap! !state assoc-in [::demo-1 :showing?] (not showing?))}
      (if showing? "Hide" "Show")]
     [:div
      :#style {:overflow :auto :flex 1}
      (when showing?
        [:div
         :#initial {:style {:transform "translateX(-100%)"}}
         :#final {:style {:transform "translateX(-100%)"}}
         :#style {:transform "translateX(0%)"
                  :transition "transform 300ms"
                  :display :inline-block}


         "Hello, World!"])]]))

(defn demo-2-view
  [{:keys [state]}]
  (let [showing? (get-in state [::demo-2 :showing?])]
    [:root>
     :#style {:display :flex :margin "1rem"}
     :#css "
@keyframes jitter-out {
  0% {
    transform: translateX(0%);
    opacity: 1.0;
  }

  10% {
    transform: translateX(-20%);
    opacity: 1.0;
  }

  30% {
    transform: translateX(20%);
    opacity: 0.5;
  }

  50% {
    transform: translateX(0%);
    opacity: 1.0;
  }
  
  100% {
    transform: translateX(0%);
    opacity: 0.0;
  }
}

@keyframes slide-in {
  from { transform: translateX(-100%); opacity: 1; }
  to { transform: translateX(0%); opacity: 1; }
}
"
     [:button
      :#on {:click #(swap! !state assoc-in [::demo-2 :showing?] (not showing?))}
      (if showing? "Hide" "Show")]
     [:div
      :#style {:overflow :auto :flex 1}
      (when showing?
        [:div
         :#initial {:style {:opacity 0 :animation :none}}
         :#final {:style {:animation "linear 300ms jitter-out" :opacity 0}}
         :#style {:display :inline-block :animation "linear 300ms slide-in"}


         "Hello, World!"])]]))

(reg/reg-component !db :example/demo-1
  :view demo-1-view
  :props {:state !state})

(reg/reg-component !db :example/demo-2
  :view demo-2-view
  :props {:state !state})
