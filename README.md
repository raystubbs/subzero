# SubZero
Build web components in Clojure/Script (the easy way).

SubZero is the web component and SSR portion of Zero,
extracted from that lib to provide a minimal 'web components'
library; without all the state management and other extras
provided by Zero.

## Example: Custom Element
```clojure
(ns increment-counter
  (:require
   [subzero.core :as sz]
   [subzero.plugins.component-registry :as reg]
   [subzero.plugins.web-components :as wc]))

(defn on-click
  [event]
  (let [increment-button (.-host (.-currentTarget event))
        clicks           (js/parseInt (.-clicks increment-button))]
    (set! (.-clicks increment-button) (inc clicks))))

(defn button-view
  [{:keys [clicks]}]
  [:root> {:#on {:click on-click}}
   [:button (str "Clicked " clicks " times")]])


(defonce !db
 (doto (sz/create-db)
  (reg/install!)
  (wc/install! js/document js/customElements)))

(reg/reg-component !db :incrementing-button
  :view button-view
  :props #{:clicks})
```
```html
<incrementing-button clicks="0"></incrementing-button>
```

## Example: SSR (With Declarative Shadow DOM)
```clojure
(ns identity-card
  (:require
   [subzero.core :as sz]
   [subzero.plugins.component-registry :as reg]
   [subzero.plugins.html :as html]))

(defn identity-card-view
  [{:keys [name age sex]}]
  [:root>
   [:table
    [:tbody
     [:tr
      [:th "Name"]
      [:td name]]
     [:tr
      [:th "Age"]
      [:td age]]
     [:tr
      [:th "Sex"]
      [:td sex]]]]])

(defonce !db
 (doto (sz/create-db)
  (reg/install!)
  (html/install!)))

(reg/reg-component !db :identity-card
  :view identity-card-view
  :props #{:name :age :sex})

(print
 (html/html !db {:doctype "html"}
  [:identity-card
   :name "John Doe"
   :age 46
   :sex "Male"]))
```
```html
<!DOCTYPE html>
<html>

<body>
    <identity-card name="John Doe" age="46" sex="Male">
        <template shadowrootmode="open">
            <table>
                <tbody>
                    <tr>
                        <th>Name</th>
                        <td>John Doe</td>
                    </tr>
                    <tr>
                        <th>Age</th>
                        <td>46</td>
                    </tr>
                    <tr>
                        <th>Sex</th>
                        <td>Male</td>
                    </tr>
                </tbody>
            </table>
        </template>
    </identity-card>
</body>

</html>
```

Docs on the way.
