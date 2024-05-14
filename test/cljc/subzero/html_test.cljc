(ns subzero.html-test
  (:require
   [clojure.string :as str]
   [subzero.plugins.component-registry :refer [reg-component] :as component-registry]
   [subzero.plugins.html :as html]
   [subzero.core :as subzero]
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :refer-macros [deftest is]])))

(defn create-db []
  (doto (subzero/create-db)
    component-registry/install!
    (html/install!
      :render-listener
      (fn [id k v]
        [:x/listen :id id :event k :action v])
      
      :render-binding
      (fn [id k v]
        [:x/bind :id id :prop k :ref v]))))

(deftest simple-rendering
  (let [!db (create-db)]
    (is
      (=
        (html/html !db [:div :foo "bar" "BAZ"])
        "<div foo=\"bar\">BAZ</div>"))
    (is
      (=
        (html/html !db [:div :foo "bar" "BAZ"])
        (html/html !db [:div {:foo "bar"} "BAZ"])))))

(deftest quote-escaping
  (let [!db (create-db)]
    (is
      (=
        (html/html !db [:div :foo "\"bar\""])
        "<div foo=\"&quot;bar&quot;\"></div>"))
    (is
      (str/includes?
        (html/html !db
          [:div
           :#class ["something" "\"other\""]])
        "&quot;other&quot;"))
    (is
      (str/includes?
        (html/html !db
          [:div
           :#style {:font-family "\"Something\""}])
        "&quot;Something&quot;"))))

(deftest doctype
  (let [!db (create-db)]
    (is
      (str/starts-with?
        (html/html !db {:doctype "html"}
          [:div "foo"])
        "<!DOCTYPE html>"))))

(deftest declarative-shadow
  (let [!db (create-db)]
    (reg-component !db :x/something
      {::html/render? true
       :props #{:foo}
       :view (fn [{:keys [foo]}] [:div foo])})
    (is
      (=
        (html/html !db
          [:x/something :foo "BAR"])
        "<x-something foo=\"BAR\"><template shadowrootmode=\"open\"><div>BAR</div></template></x-something>"))))