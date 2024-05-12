(ns subzero.component-btest
  (:require
   [subzero.core :as core]
   [subzero.plugins.component-registry
    :refer [reg-component]
    :as component-registry]
   [subzero.plugins.web-components :as web-components]))

(defn do-steps [zero-element & step-fns]
  (if (empty? step-fns)
    nil
    (js/Promise.
      (fn [resolve reject] 
            (js/setTimeout
              (fn []
                (js/requestAnimationFrame
                  (fn []
                    (try
                      (-> ((first step-fns) zero-element)
                        js/Promise.resolve
                        (.then
                          (fn []
                            (resolve (apply do-steps zero-element (rest step-fns)))))
                        (.catch reject))
                      (catch :default ex
                        (reject ex)))))))))))

(defn step-test [create-element-fn & step-fns]
  (fn []
    (let [el (create-element-fn)]
      (js/document.body.append el)
      (.then (apply do-steps el step-fns)
        (fn [] (.remove el))))))

(defn create-element [name]
  (js/document.createElement (core/element-name name)))

(def !!db (atom nil))

(defn init-engine!
  []
  (reset! !!db (core/create-db))
  (component-registry/install! @!!db)
  (web-components/install! @!!db
    js/document
    js/customElements))

(js/describe "Component"
  (fn []
    (js/beforeEach
      (fn []
        (init-engine!)
        (reg-component @!!db ::echo
          :props #{:vdom}
          :view (fn [{:keys [vdom]}]
                  vdom))))
    
    (js/it "renders" 
      (step-test 
        #(create-element ::echo)
        (fn [^js/HTMLElement echo]
          (set! (.-vdom echo)
            [:div "FOO"]))
        (fn [^js/HTMLElement el]
          (assert (= 1 (-> el .-shadowRoot .-childNodes .-length)))
          
          (let [first-el (-> el .-shadowRoot .-firstChild)]
            (assert (= "DIV" (.-nodeName first-el)))
            (assert (= "FOO" (.-innerText first-el)))))))
    
    (js/it "reacts to property"
      (step-test
        #(create-element ::echo)
        (fn [^js/HTMLElement el]
          (assert (= "" (-> el .-shadowRoot .-firstChild .-nodeValue)))
          (set! (.-vdom el) "BAR"))
        (fn [^js/HTMLElement el]
          (assert (= "BAR" (-> el .-shadowRoot .-firstChild .-nodeValue))))))
    
    (js/it "reacts to attribute"
      (step-test
        #(create-element ::echo)
        (fn [^js/HTMLElement el]
          (assert (= "" (-> el .-shadowRoot .-firstChild .-nodeValue)))
          (.setAttribute el "vdom" "BAR"))
        (fn [^js/HTMLElement el]
          (assert (= "BAR" (-> el .-shadowRoot .-firstChild .-nodeValue))))))
    
    (js/it "reacts to binding"
      (let [!atom (atom "BAR")]
        (step-test
          #(create-element ::echo)
          (fn [^js/HTMLElement el]
            (assert (= "" (-> el .-shadowRoot .-firstChild .-nodeValue)))
            (set! (.-vdom el)
              [:input :#bind {:value !atom}]))
          (fn [^js/HTMLElement el]
            (assert (= "BAR" (-> el .-shadowRoot .-firstChild .-value)))
            (reset! !atom "BAZ"))
          (fn [^js/HTMLElement el]
            (assert (= "BAZ" (-> el .-shadowRoot .-firstChild .-value)))))))
    
    (js/it "listens to events"
      (let [!atom (atom nil)]
        (step-test
          #(create-element ::echo)
          (fn [^js/HTMLElement el]
            (assert (= "" (-> el .-shadowRoot .-firstChild .-nodeValue)))
            (set! (.-vdom el)
              [:input :#on {:input #(reset! !atom (-> % .-target .-value))}]))
          (fn [^js/HTMLElement el]
            (let [input-el (-> el .-shadowRoot .-firstChild)]
              (assert (= "" (.-value input-el)))
              (set! (.-value input-el) "BAR")
              (.dispatchEvent input-el (js/InputEvent. "input"))))
          (fn [^js/HTMLElement el]
            (assert (= "BAR" @!atom))
            (set! (.-vdom el) [:input]))
          (fn [^js/HTMLElement el]
            (let [input-el (-> el .-shadowRoot .-firstChild)]
              (set! (.-value input-el) "BAZ")
              (.dispatchEvent input-el (js/InputEvent. "input"))))
          (fn [^js/HTMLElement _el]
            (assert (= @!atom "BAR"))))))))