(ns subzero.plugins.web-components "
Implements web components on top of the component registry.

State lives at `::state` in the db, and has:
  ::class->fields-index
  ;; Mapping of `all prop name variations -> writable JS property name`
  ;; for element classes.

  ::css-link-elements
  ;; A set of all CSS `<link>` elements managed by this plugin.  Used
  ;; for quick access when we need to override the hrefs during hot
  ;; reload.  Only populated when hot reload is enabled.

  ::css-stylesheet-objects
  ;; A map of `normalized URL -> constructed style sheet`, so we can
  ;; reuse them.  Entries never expire.

  ::href-overrides
  ;; When shadow-cljs (or figwheel I presume) detects an FS change of
  ;; some stylesheet linked at the top level of the document, it'll force
  ;; the browser to reload it be appending some arbitrary 'version'
  ;; to the href's query string.  The plugin watches for those changes, and:
  ;;   1. Keeps track of the new HREF, so it can be used in newly created links
  ;;      and constructed stylesheets.
  ;;   2. Replaces the HREF in all matching `::css-link-elements`.
  ;;   3. Reloads all matching `::css-stylehseet-objects` with the new URL.
  ;;
  ;; Only populated when hot reload is enabled.
"
    (:require
     [subzero.rstore :as rstore]
     [subzero.core :as core]
     [subzero.impl.util :as util]
     [subzero.impl.markup :as markup]
     [subzero.logger :as log]
     [subzero.plugins.component-registry :as component-registry]
     [goog :refer [DEBUG]]
     [goog.object :as obj]
     [clojure.string :as str]
     [clojure.set :as set]
     [check.core :refer [check when-check]]))

(defn- compile-css
  [!db s]
  (let [CSSStyleSheet (.-CSSStyleSheet ^js (get-in @!db [::state ::window]))]
    (doto (CSSStyleSheet.) (.replaceSync s))))

(defonce ^:private private-state-sym (js/Symbol "subzeroWebComponentsPrivate"))
(defonce ^:private render-order-sym (js/Symbol "subzeroWebComponentsRenderOrder"))
(defonce ^:private html-ns "http://www.w3.org/1999/xhtml")
(defonce ^:private svg-ns "http://www.w3.org/2000/svg")
(defonce ^:private js-undefined (js* "undefined"))

(declare invalidate! schedule-animation-frame!)

(defn- get-private-state [^js/Object obj]
  (or (obj/get obj private-state-sym)
    (let [!private-state (atom {})]
      (obj/set obj private-state-sym !private-state)
      !private-state)))

(defn- is-js-property-writable?
  [^js/Object obj property-name]
  (if (nil? obj)
    false
    (if-let [prop-def (js/Object.getOwnPropertyDescriptor obj property-name)]
      (or (.-writable prop-def) (some? (.-set prop-def)))
      (is-js-property-writable? (js/Object.getPrototypeOf obj) property-name))))

(defn- get-fields-index-for-class
  [!db ^js/Object js-class]
  (or (get-in @!db [::state ::class->fields-index js-class])
    (let [parent-class (js/Object.getPrototypeOf js-class)
          proto (.-prototype js-class)]
      (when proto
        (let [index (->> proto
                      js/Object.getOwnPropertyNames
                      (filter #(is-js-property-writable? proto %))
                      (mapcat
                        (fn [prop-name]
                          [[(keyword prop-name) prop-name]
                           [(keyword (util/snake-case prop-name)) prop-name]
                           [(keyword (util/cammel-case prop-name)) prop-name]]))
                      (into {})
                      (merge (some->> parent-class (get-fields-index-for-class !db))))]
          (rstore/patch! !db
            {:path [::state ::class->fields-index js-class]
             :change [:value index]})
          index)))))

(defn- get-fields-index-for-object
  [!db ^js/Object js-obj]
  (->> js-obj .-constructor (get-fields-index-for-class !db)))

(defn- purge-fields-index-for-class
  [!db class]
  (rstore/patch! !db
    {:path [::state ::class->fields-index]
     :change [:clear class]})
  nil)

(defn- hot-reload-enabled?
  [!db]
  (boolean (and DEBUG (get-in @!db [::state ::hot-reload?]))))

(defn- doc-origin
  [!db]
  (-> @!db ::state ^js/HTMLDocument (::document) .-location .-origin))

(defn- absolute-url
  [!db url]
  (js/URL. (str url) (doc-origin !db)))

(defn- local-url?
  [!db ^js/URL url]
  (= (doc-origin !db) (.-origin url)))

(defn- ensure-top-level-css-link "
When shadow-cljs (or figwheel) detect file system changes to a
CSS file linked in the top level document, they'll force the
browser to reload said asset by adding (or bumping) a 'version'
in the query string.  These tools _don't_ do this for stylesheets
linked in a ShadowRoot, so this plugin watches for those changes
made by the hot reload tooling to determine when it should reload
stylesheets.

Problem is, if there's no link in the top level for a stylesheet
used within a ShadowRoot, then there's nothing for the hot reload
tooling to update; and no way for us to know when the asset has
changed on disk.

To get around this, when we see a stylesheet URL that isn't linked
at the top level, we install our own link, with a media query that
will never be satisfied.  So it won't apply any styling to the top
level; but tooling will still be able to see it and update it
when the asset changes on disk.
" [!db css-url]
  (when (hot-reload-enabled? !db)
    (let [full-css-url (absolute-url !db css-url)
          document ^js/HTMLDocument (get-in @!db [::state ::document])]
      (when (local-url? !db full-css-url)
        (let [top-level-css-link-doms
              (array-seq (.querySelectorAll document "link[rel=\"stylesheet\"]"))

              existing-link-dom
              (some
                (fn [^js/HTMLLinkElement link]
                  (let [link-url (absolute-url !db (.-href link))]
                    (when
                     (and
                       (= (.-origin full-css-url) (.-origin link-url))
                       (= (.-pathname full-css-url) (.-pathname link-url)))
                      link)))
                top-level-css-link-doms)]
          (or existing-link-dom
            (let [new-link-dom (.createElement document "link")]
              ;; impossible media query, so link will never apply
              (set! (.-media new-link-dom) "screen and print")
              (set! (.-href new-link-dom) (.-pathname full-css-url))
              (set! (.-rel new-link-dom) "stylesheet")
              (.append (.-head document) new-link-dom)
              new-link-dom))))))
  nil)

(defn- load-stylesheet-contents! [stylesheet-object url]
  (let [!private-state (get-private-state stylesheet-object)
        aborter (js/AbortController.)]
    (when-let [old-aborter (::aborter @!private-state)]
      (.abort old-aborter ::overridden))

    (swap! !private-state assoc ::href (str url) ::aborter aborter)

    (-> (js/fetch url #js{:signal (.-signal aborter)})
      (.then #(.text %))
      (.then (fn [css-text]
               (when (= (::href @!private-state) (.toString url))
                 (.replace stylesheet-object css-text))))
      (.catch (fn [cause]
                (when-not (= ::overridden cause)
                  (throw cause)))))
    nil))

(defn- get-stylesheet-object
  [!db x]
  (cond
    (or (instance? js/URL x) (and (string? x) (or (str/starts-with? x "/") (str/starts-with? x "http"))))
    (let [absolute-url (absolute-url !db x)
          absolute-url-str (.toString absolute-url)]
      (or
        (get-in @!db [::state ::css-stylesheet-objects absolute-url-str])

        (let [new-css-obj (compile-css !db "* { display: none; }")]
          (ensure-top-level-css-link !db absolute-url-str)
          (load-stylesheet-contents! new-css-obj
            (or
              (when (local-url? !db absolute-url)
                (some-> (get-in @!db [::state ::href-overrides (.-pathname absolute-url)]) str))
              absolute-url-str))
          (rstore/patch! !db
            {:path [::state ::css-stylesheet-objects absolute-url-str]
             :change [:value new-css-obj]})
          new-css-obj)))

    (string? x)
    (compile-css !db x)

    (instance? (.-CSSStyleSheet ^js (get-in @!db [::state ::window])) x)
    x))

(defn- adjusted-prop-value
  [!db prop-name value]
  (if-not (and (hot-reload-enabled? !db) (= :href prop-name))
    value
    (let [url (cond
                (instance? js/URL value)
                value

                (and (string? value) (str/starts-with? value "http"))
                (try
                  (absolute-url !db value)
                  (catch :default _
                    nil)))]
      (or
        (when (and url (local-url? !db url))
          (get-in @!db [::state ::href-overrides (.-pathname url)]))
        value))))

(defn- set-prop!
  [!db ^js/Object obj prop-name value]
  {:pre [(keyword? prop-name)]}
  (when-let [field-name (-> (get-fields-index-for-object !db obj) (get prop-name))]
    (obj/set obj field-name value)
    true))

(defn- get-prop
  [!db ^js/Object obj prop-name]
  {:pre [(keyword? prop-name)]}
  (when-let [field-name (-> (get-fields-index-for-object !db obj) (get prop-name))]
    (obj/get obj field-name)))

(defn- set-element-prop!
  [!db ^js/HTMLElement element prop-name value]
  (when-not (set-prop! !db element prop-name value)
    (let [adjusted-value (adjusted-prop-value !db prop-name value)
          component-name (or (.-componentName element) (-> element .-nodeName str/lower-case keyword))
          attr-name (name prop-name)
          attr-value ((component-registry/get-attribute-writer !db component-name)
                      adjusted-value attr-name component-name)]
      (cond
        (true? attr-value)
        (.setAttribute element attr-name "")

        (not attr-value)
        (.removeAttribute element attr-name)

        :else
        (.setAttribute element attr-name (str attr-value)))))
  nil)

(defn- get-element-prop
  [!db ^js/HTMLElement element prop-name]
  (let [x (get-prop !db element prop-name)]
    (if (= x js-undefined)
      (.getAttribute element (name prop-name))
      x)))


(defn- default-ns
  [tag]
  (case tag
    :svg svg-ns
    nil))

(defn- diff-shallow
  [map-a map-b ks]
  (reduce
    (fn [diff key]
      (let [val-a (get map-a key)
            val-b (get map-b key)]
        (if (= val-a val-b)
          diff
          (assoc diff key [val-a val-b]))))
    {} ks))

(defn- diff-props
  [old-props new-props]
  (let [all-keys (merge old-props new-props)
        deep-keys [:#style :#on :#on-host :#internals :#bind]]
    (as-> all-keys $
      (apply dissoc $ deep-keys)
      (keys $)
      (diff-shallow old-props new-props $)
      (reduce
        (fn [diff key]
          (if-not (contains? all-keys key)
            diff
            (let [old-inner-map (get old-props key)
                  new-inner-map (get new-props key)
                  inner-diff (diff-shallow old-inner-map new-inner-map (keys (merge old-inner-map new-inner-map)))]
              (if (empty? inner-diff)
                diff
                (assoc diff key inner-diff)))))
        $ deep-keys))))

(defn- coll->validity-flags-obj "
Creates a `flags` object for
[ElementInternals#setValidity](https://developer.mozilla.org/en-US/docs/Web/API/ElementInternals/setValidity)
from a set/coll of keywords.
" [coll]
  (let [obj #js{}]
    (doseq [field-name (->> coll (filter util/named?) (map (comp util/cammel-case name)))]
      (obj/set obj field-name true))
    obj))

(defprotocol IListenKey
  (listen [lk !db ^js/EventTarget target listener-fun])
  (unlisten [lk !db ^js/EventTarget target]))

(defprotocol IListenValue
  (get-listener-fun [lv !db]))

(extend-protocol IListenKey
  Keyword
  (listen
    [k _!db target listener-fun]
    (let [!private-state (get-private-state target)
          aborter (js/AbortController.)]
      (when-let [old-listener-aborter ^js/AbortController (get-in @!private-state [::listener-aborters k])]
        (.abort old-listener-aborter))
      (swap! !private-state assoc-in [::listener-aborters k] aborter)
      (.addEventListener target (name k) listener-fun #js{:signal (.-signal aborter)}))
    nil)
  (unlisten
    [k _!db target]
    (let [!private-state (get-private-state target)]
      (when-let [old-listener-aborter ^js/AbortController (get-in @!private-state [::listener-aborters k])]
        (.abort old-listener-aborter))
      (swap! !private-state util/dissoc-in [::listener-aborters k]))
    nil))

(extend-protocol IListenValue
  function
  (get-listener-fun
    [f _!db]
    f))

(defprotocol IBindKey
  (bind [bk !db ^js/HTMLElement element watchable])
  (unbind [bk !db ^js/HTMLElement element]))

(defprotocol IBindValue
  (get-bind-watchable [bv !db]))

(extend-protocol IBindKey
  Keyword
  (bind
    [k !db element watchable]
    (let [!private-state (get-private-state element)]
      (when-let [old-watchable (get-in @!private-state [::bindings k])]
        (remove-watch old-watchable k))
      (swap! !private-state assoc-in [::bindings k] watchable)
      (add-watch watchable [element k]
        (fn [_ _ old-val new-val]
          (when-not (identical? old-val new-val)
            (set-element-prop! !db element k new-val))))
      (when (and
              (util/can-deref? watchable)
              (not (identical? @watchable (get-element-prop !db element k))))
        (set-element-prop! !db element k @watchable))))
  (unbind
    [k !db element]
    (let [!private-state (get-private-state element)]
      (when-let [old-watchable (get-in @!private-state [::bindings k])]
        (remove-watch old-watchable [element k]))
      (set-element-prop! !db element k nil)
      (swap! !private-state util/dissoc-in [::bindings k]))))

(defn- listener-fn
  [!db x]
  (if (and (ifn? x) (not (satisfies? IListenValue x)))
    x
    (get-listener-fun x !db)))

(defn- patch-listeners!
  [!db ^js/EventTarget target listener-diff-map]
  (doseq [[k [old-val new-val]] listener-diff-map]
    (when (some? old-val)
      (unlisten k !db target))
    (when (some? new-val)
      (listen k !db target (listener-fn !db new-val)))))

(defn- binding-watchable
  [!db x]
  (if (and (util/can-watch? x) (not (satisfies? IBindValue x)))
    x
    (get-bind-watchable x !db)))

(defn- patch-bindings!
  [!db ^js/HTMLElement element binds-diff-map]
  (doseq [[k [old-val new-val]] binds-diff-map]
    (when (some? old-val)
      (unbind k !db element))
    (when (some? new-val)
      (bind k !db element (binding-watchable !db new-val)))))

(defrecord HostListenKey [k]
  IListenKey
  (listen
    [_ !db target listener-fun]
    (listen k !db target listener-fun))
  (unlisten
    [_ !db target]
    (unlisten k !db target)))

(defn- patch-root-props!
  [!db ^js/ShadowRoot shadow-root ^js/ElementInternals internals props]
  (let [!private-state (get-private-state shadow-root)
        form-associated? (-> shadow-root .-host .-constructor .-formAssociated)
        diff (diff-props (::props @!private-state) props)
        ^js host-css (or (::host-css @!private-state)
                       (let [x (compile-css !db ":host {}")]
                         (swap! !private-state assoc ::host-css x)
                         x))]
    (when-not (empty? diff)
      (when-let [style-diff (diff :#style)]
        (let [style-obj (-> host-css .-cssRules (.item 0) .-style)]
          (doseq [[k [_ new-val]] style-diff]
            (if-not new-val
              (.removeProperty style-obj (name k))
              (.setProperty style-obj (name k) (markup/clj->css-property new-val))))))
      (when-some [[_ css-prop] (get diff :#css)]
        (set! (.-adoptedStyleSheets shadow-root)
          (->> (conj css-prop host-css)
            (mapv (partial get-stylesheet-object !db))
            to-array)))

      (patch-listeners! !db shadow-root (diff :#on))
      (patch-listeners! !db (.-host shadow-root) (update-keys (diff :#on-host) ->HostListenKey))

      ;; patch internals
      (when internals
        (let [internals-fields-index (get-fields-index-for-class !db
                                       (.-ElementInternals ^js (get-in @!db [::state ::window])))]
          (doseq [[k [_ new-val]] (diff :#internals)]
            (case k
              :#states
              (when-some [states ^js/CustomStateSet (.-states internals)]
                (.clear states)
                (doseq [state-val new-val]
                  (.add states (name state-val))))

              :#value
              (when form-associated?
                (let [[value state] (if (map? new-val)
                                      [(:value new-val) (:state new-val)]
                                      [new-val nil])]
                  (.setFormValue internals (or value "") (or state ""))))

              :#validity
              (when form-associated?
                (.setValidity internals
                  (coll->validity-flags-obj (:flags new-val))
                  (or (:message new-val) js-undefined)
                  (or (:anchor new-val) js-undefined))
                (when (:report? new-val)
                  (.reportValidity internals)))

              (when-some [field-name (get internals-fields-index k)]
                (obj/set internals field-name new-val)))))))
    (swap! !private-state assoc ::props props))
  nil)

(defn- normal-prop-name? [k]
  (and (keyword? k) (not (str/starts-with? (name k) "#"))))

(defn- patch-props!
  [!db ^js/HTMLElement element props]
  (let [!private-state (get-private-state element)
        old-props (::props @!private-state)]
    (cond
      (seq old-props)
      (let [diff (diff-props (dissoc (::props @!private-state) :#initial :#final) props)]
        (when-not (empty? diff)
          ;; normal props
          (doseq [[k [_old-val new-val]] (filter #(normal-prop-name? (key %)) diff)]
            (set-element-prop! !db element k new-val))

          (patch-listeners! !db element (diff :#on))
          (patch-bindings! !db element (diff :#bind))

          ;; patch styles
          (when-let [style-diff (diff :#style)]
            (let [style-obj (.-style element)]
              (doseq [[k [_ new-val]] style-diff]
                (if-not new-val
                  (.removeProperty style-obj (name k))
                  (.setProperty style-obj (name k) (markup/clj->css-property new-val))))))

           ;; patch classes
          (when-let [[_ class] (diff :#class)]
            ;; setting className is faster than .setAttribute, but there
            ;; doesn't seem to be a way to remove the attribute this way,
            ;; so use .removeAttribute to remove it
            (cond
              (nil? class)
              (.removeAttribute element "class")

              (coll? class)
              (set! (.-className element) (str/join " " class))

              :else
              (set! (.-className element) (str class))))

          (swap! !private-state assoc ::props props)))

      :else
      (let [initializing? (::initializing? @!private-state)
            class (or (when initializing? (some-> props :#initial :class)) (:#class props))
            style (merge (:#style props) (when initializing? (some-> props :#initial :style)))]
        ;; normal props
        (doseq [[k v] (filter #(normal-prop-name? (key %)) props)]
          (set-element-prop! !db element k v))

        ;; listeners
        (doseq [[k v] (:#on props)]
          (listen k !db element (listener-fn !db v)))

        ;; bindings
        (doseq [[k v] (:#bind props)]
          (bind k !db element (binding-watchable !db v)))

        ;; patch styles
        (when (seq style)
          (let [style-obj (.-style element)]
            (doseq [[k v] style]
              (.setProperty style-obj (name k) (markup/clj->css-property v)))))

         ;; patch classes
        (when (seq class)
          (cond
            (coll? class)
            (set! (.-className element) (str/join " " class))

            :else
            (set! (.-className element) (str class))))

        (swap! !private-state assoc ::props (assoc props :#style style :#class class)))))
  nil)

(defn- prepare-node-for-removal!
  [!db ^js/Node node]
  (let [!private-state (get-private-state node)
        props (::props @!private-state)
        shadow-root-class (.-ShadowRoot ^hs (get-in @!db [::state ::window]))]
    (cond
      (instance? shadow-root-class node)
      (doseq [[k listener] (:#on-host props)]
        (when (some? listener)
          (unlisten (->HostListenKey k) !db (.-host node))))

      :else
      (doseq [[k watchable] (:#bind props)]
        (when (some? watchable)
          (unbind k !db node))))
    (doseq [[k listener] (:#on props)]
      (when (some? listener)
        (unlisten k !db node)))
    (swap! !private-state update ::props dissoc :#bind :#on)

    ;; SubZero components
    (when (some? (::status @!private-state))
      (let [shadow-root (.-shadowRoot node)
            !static-state (get-private-state (.-constructor node))]
        (rstore/patch! !db
          [{:path [::state ::elements-to-render]
            :change [:clear node]}
           {:path [::state ::elements-to-patch]
            :change [:clear node]}
           {:path [::state ::elements-to-disconnect]
            :change [:clear node]}])
        (swap! !private-state assoc ::status :disconnected)
        (doseq [cleanup-fn (::cleanup-fns @!private-state)]
          (cleanup-fn))
        (swap! !private-state assoc ::cleanup-fns #{})
        (when (pos? (get-in @!private-state [::lifecycle-event-listener-counts "disconnect"]))
          (.dispatchEvent shadow-root (js/Event. "disconnect")))
        (swap! !static-state update ::instances disj node)
        (prepare-node-for-removal! !db shadow-root))))

  (doseq [child-dom (-> node .-childNodes array-seq)]
    (prepare-node-for-removal! !db child-dom))

  (when (and (hot-reload-enabled? !db) (= (.-nodeName node) "LINK"))
    (rstore/patch! !db
      {:path [::css-link-elements]
       :change [:clear node]}))
  nil)

(defn- insert-child!
  [^js/Node dom ^js/Node reference ^js/Node child]
  (cond
    (nil? reference)
    (if (fn? (.-prepend dom))
      (.prepend dom child)
      (if-let [first-child (.-firstChild dom)]
        (.insertBefore dom child first-child)
        (.appendChild dom child)))

    (fn? (.-after reference))
    (.after reference child)

    :else
    (if-let [next-child (.-nextSibling reference)]
      (.insertBefore dom child next-child)
      (.appendChild dom child))))

(defn- try-pass-focus!
  [!db ^js/Node dom]
  (let [tab-index (.-tabIndex dom)
        focus-fn (.-focus dom)]
    (cond
      (and (some? tab-index) (not (neg? tab-index)) (fn? focus-fn))
      (.call focus-fn dom)

      (instance? js/ShadowRoot dom)
      (some->> dom .-host (try-pass-focus! !db))

      (not (identical? (.-body ^js/HTMLDocument (get-in @!db [::state ::document])) dom))
      (some->> dom .-parentNode (try-pass-focus! !db)))))

(defn- apply-layout-changes!
  [^js/HTMLElement element start-index stop-index child-dom->source-index target-layout]
  (loop [boundary-index (dec start-index)
         next-target-index start-index]
    (if (<= stop-index next-target-index)
      nil
      (let [next-child-dom (get target-layout next-target-index)
            next-source-index (get child-dom->source-index next-child-dom)
            boundary-dom (get target-layout boundary-index)]
        (cond
          ;; new child, just insert it in the right place
          (nil? next-source-index)
          (do
            (insert-child! element boundary-dom next-child-dom)
            (recur next-target-index (inc next-target-index)))

          ;; It's a pivot node if it's only been shifted by 1 in either direction,
          ;; and both its old and new indices are greater than boundary-index.  Pivots
          ;; are 'stable', we don't move them, instead everything else moves
          ;; around them.
          (and
            (< boundary-index next-target-index)
            (< boundary-index next-source-index)
            (<= (dec next-source-index) next-target-index (inc next-source-index)))
          (recur next-target-index (inc next-target-index))

          :else
          (do
            (insert-child! element boundary-dom next-child-dom)
            (recur next-target-index (inc next-target-index))))))))

(defn- parse-duration-str
  [s]
  (if-some [[_ n u] (re-matches #"([0-9.]+)(s|ms)" s)]
    (case u "s" (* (parse-double n) 1000) "ms" (parse-long n))
    0))

(defn- get-element-animation-delay
  [^js element]
  (let [computed-style (js/getComputedStyle element)]
    (reduce
      (fn [current-max [next-duration-str next-delay-str]]
        (max current-max (+ (parse-duration-str next-delay-str))(parse-duration-str next-duration-str)))
      0
      (concat
        (map vector
          (str/split (.getPropertyValue computed-style "transition-duration") #"\s*,\s*")
          (str/split (.getPropertyValue computed-style "transition-delay") #"\s*,\s*"))
        (map vector
          (str/split (.getPropertyValue computed-style "animation-duration") #"\s*,\s*")
          (str/split (.getPropertyValue computed-style "animation-delay") #"\s*,\s*"))))))

(defn set-timeout!
  ([!db f]
   (set-timeout! !db f 0))
  ([!db f to]
   (.setTimeout ^js/Window (get-in @!db [::state ::window]) f to)))

(defn- patch-children!
  [!db ^js/HTMLElement host ^js/Node node children]
  (let [document ^js/HTMLDocument (get-in @!db [::state ::document])
        text-node-type (-> ^js (get-in @!db [::state ::window]) .-Node .-TEXT_NODE)
        host-state @(get-private-state host)
        disable-tags? (get-in @!db [::state ::disable-tags?])
        source-layout (->> node .-childNodes array-seq
                        (keep
                          (fn [^js child-dom]
                            (let [!state (get-private-state child-dom)]
                              (if (not (::finalizing? @!state))
                                child-dom
                                (do
                                  (swap! !state assoc ::finalizing? false)
                                  (.removeChild node child-dom)
                                  (prepare-node-for-removal! !db child-dom)
                                  nil)))))
                        vec)
        preproc-vnode (get-in @!db [::state ::preproc-vnode])
        !child-doms (atom
                      (group-by
                        (fn [child-dom]
                          (if (= text-node-type (-> child-dom .-nodeType))
                            :text
                            (let [props (::props @(get-private-state child-dom))]
                              [(:#sel props) (:#key props)])))
                        source-layout))

        take-el-dom (fn [tag props]
                      (let [matcher [(:#sel props) (:#key props)]
                            match (->> matcher (get @!child-doms) first)]
                        (if match
                          (do
                            (swap! !child-doms update matcher subvec 1)
                            match)
                          (.createElementNS document
                            (or
                              (:xmlns props)
                              (default-ns tag)
                              (.-namespaceURI node)
                              html-ns)
                            (markup/kw->el-name tag)))))

        take-text-dom (fn []
                        (if-let [existing (first (get @!child-doms :text))]
                          (do
                            (swap! !child-doms update :text subvec 1)
                            existing)
                          (.createTextNode document "")))

        target-layout (->> children
                        (mapcat
                          (fn process-children [vnode]
                            (cond
                              (vector? vnode)
                              (let [[tag props body] (preproc-vnode vnode)
                                    child-element (take-el-dom tag props)
                                    !child-element-state (get-private-state child-element)
                                    old-props (::props @!child-element-state)]
                                (when (or
                                        disable-tags?
                                        (nil? (:#tag props))
                                        (::ignore-tags-on-next-patch? host-state)
                                        (not= (:#tag props) (:#tag old-props))
                                        (not (true? (::initializing? @!child-element-state))))
                                  (when (nil? (::initializing? @!child-element-state))
                                    (swap! !child-element-state assoc ::initializing? (boolean (:#initial props))))

                                  (patch-props! !db child-element props)
                                  (when-not (:#opaque? props)
                                    (when (::ignore-tags-on-next-patch? host-state)
                                      (swap! !child-element-state assoc ::ignore-tags-on-next-patch? true))
                                    (patch-children! !db host child-element body))

                                  (when (::initializing? @!child-element-state)
                                    (set-timeout! !db
                                      (fn []
                                        (swap! !child-element-state assoc ::initializing? false)
                                        (rstore/patch! !db
                                          {:path [::state ::elements-to-patch]
                                           :change [:conj host]})
                                        (schedule-animation-frame! !db)))))
                                [child-element])

                              (fn? vnode)
                              (let [vdom (-> host-state ::prop-vals vnode)]
                                (if (seq? vdom)
                                  (map process-children vdom)
                                  (process-children vdom)))

                              :else
                              (let [child-dom (take-text-dom)
                                    text-value (str vnode)]
                                (when-not (identical? (.-nodeValue child-dom) text-value)
                                  (set! (.-nodeValue child-dom) text-value))
                                [child-dom]))))
                        vec)

        focused-doms (::doms-on-focus-path host-state)

        index-of-focused-child-in-target
        (when (seq focused-doms)
          (util/index-of (partial contains? focused-doms) target-layout))

        child-dom->source-index (set/map-invert source-layout)
        preserved-child-doms (set target-layout)]

    ;; keep track of <link> elements, so we can make them react to hot reloads
    (when (hot-reload-enabled? !db)
      (doseq [child-node target-layout
              :when (and
                      (= (.-nodeName child-node) "LINK")
                      (= "stylesheet" (.-rel child-node)))]
        (ensure-top-level-css-link !db (.-href child-node))
        (rstore/patch! !db
          {:path [::css-link-elements]
           :change [:conj child-node]})))

    ;; apply layout changes
    (cond
      (nil? index-of-focused-child-in-target)
      (apply-layout-changes!
        node
        0
        (count target-layout)
        child-dom->source-index
        target-layout)

      :else
      (do
        (apply-layout-changes!
          node
          0
          index-of-focused-child-in-target
          child-dom->source-index
          target-layout)
        (apply-layout-changes!
          node
          (inc index-of-focused-child-in-target)
          (count target-layout)
          child-dom->source-index
          target-layout)))

    ;; remove expired children
    (doseq [child-node source-layout
            :when (not (contains? preserved-child-doms child-node))
            :let [!child-state (get-private-state child-node)
                  child-props  (::props @!child-state)]]
      ;; if we need to remove a focused child, then first try to move focus to a focusable
      ;; parent; otherwise the browser will default to focusing the <body>... which can case
      ;; problems in many cases
      (when (contains? focused-doms child-node)
        (try-pass-focus! !db (.-parentNode child-node)))

      (cond
        (:#final child-props)
        (let [final-props (:#final child-props)
              final-style (:style final-props)
              final-class (:class final-props)]
          (swap! !child-state assoc ::finalizing? true)

          (when (seq final-style)
            (let [style-obj (.-style child-node)]
              (doseq [[k v] final-style]
                (.setProperty style-obj (name k) (markup/clj->css-property v)))))

          (when (seq final-class)
            (cond
              (coll? final-class)
              (set! (.-className child-node) (str/join " " final-class))

              :else
              (set! (.-className child-node) (str final-class))))

          (let [total-animation-delay (- (get-element-animation-delay child-node) 50)]
            (if-not (pos? total-animation-delay)
              (do
                (.removeChild node child-node)
                (prepare-node-for-removal! !db child-node))
              (set-timeout! !db
                (fn []
                  (when (::finalizing? @!child-state)
                    (.removeChild node child-node)
                    (prepare-node-for-removal! !db child-node)))
                total-animation-delay))))

        :else
        (do
          (.removeChild node child-node)
          (prepare-node-for-removal! !db child-node))))))


(defn- patch-root!
  [!db ^js/HTMLElement host vnode]
  (let [shadow-root ^js/ShadowRoot (.-shadowRoot host)
        !host-state (get-private-state host)
        !shadow-state (get-private-state shadow-root)
        !static-state (-> host .-constructor get-private-state)
        default-css (::default-css @!static-state)
        old-props (::props @!shadow-state)
        preproc-vnode (get-in @!db [::state ::preproc-vnode])
        [props body] (cond
                       (and (vector? vnode) (= (first vnode) :root>))
                       (rest (preproc-vnode vnode))

                       (seq? vnode)
                       [{} vnode]

                       :else
                       [{} (list vnode)])]
    (when (or
            (get-in @!db [::state ::disable-tags?])
            (nil? (:#tag props))
            (::ignore-tags-on-next-patch? @!host-state)
            (not= (:#tag props) (:#tag old-props)))
      (patch-root-props! !db shadow-root (::internals @!host-state)
        (update props :#css
          (fn [css]
            (cond
              (coll? css) (into default-css css)
              (some? css) (conj default-css css)
              :else default-css))))
      (when-not (:#opaque? props)
        (patch-children! !db host shadow-root body)))))

(defn- expire-elements!
  [!db]
  (doseq [element (get-in @!db [::state ::elements-to-disconnect])
          :when (not (.-isConnected element))]
    (prepare-node-for-removal! !db element))
  (rstore/patch! !db
    {:path [::state ::elements-to-disconnect]
     :change [:value #{}]})
  nil)

(defn- render-vdom!
  [^js/HTMLElement element]
  (let [!element-state (get-private-state element)
        !static-state (get-private-state (.-constructor element))]
    (swap! !element-state assoc ::rendered-vdom
      (try
        ((::view @!static-state) (::prop-vals @!element-state))
        (catch :default e
          (log/error "Error in component view function"
            :data {:component (::name @!static-state)}
            :ex e)
          nil))))
  nil)

(defn- render-vdoms-for-dirty-elements!
  [!db]
  (doseq [element (get-in @!db [::state ::elements-to-render])]
    (render-vdom! element))
  (rstore/patch! !db
    {:path [::state ::elements-to-render]
     :change [:value #{}]})
  nil)

(defn- update-dirty-elements!
  [!db]
  (when-let [before-render (get-in @!db [::state ::before-render])]
    (try
      (before-render !db)
      (catch :default ex
        (log/error "Error in before-render callback" :ex ex))))
  (rstore/patch! !db
    {:path [::state ::pending-render-id]
     :change [:value nil]})

  (while (seq (get-in @!db [::state ::elements-to-patch]))
    (let [elements-to-patch (get-in @!db [::state ::elements-to-patch])
          elements-to-render (get-in @!db [::state ::elements-to-render])]
      (rstore/patch! !db
        [{:path [::state ::elements-to-patch]
          :change [:value #{}]}
         {:path [::state ::elements-to-render]
          :change [:value #{}]}])
      (doseq [^js/HTMLElement element (sort-by #(obj/get % render-order-sym) elements-to-patch)
              :let [!element-state (get-private-state element)]
              :when (not= (::status @!element-state) :disconnected)]
        (let [!static-state (-> element .-constructor get-private-state)
              ^js/ShadowDom shadow (::shadow @!element-state)
              vdom-props (::props @!element-state)
              vdom (do
                     (when (contains? elements-to-render element)
                       (render-vdom! element))
                     (::rendered-vdom @!element-state))]

          ;; if it needs to be focusable, but explicit tabIndex wasn't set
          (when (and
                  (= (::focus @!static-state) :self)
                  (not (or (contains? vdom-props :tab-index) (contains? vdom-props :tabindex)))
                  (< (.-tabIndex element) 0))
            (set! (.-tabIndex element) 0))


          (try
            ;; render the thing to DOM
            (patch-root! !db element vdom)
            (swap! !element-state assoc ::ignore-tags-on-next-patch? false)


            ;; dispatch lifecycle events
            (let [event-type
                  (if (= :connected (::status @!element-state))
                    "update"
                    (do
                      (swap! !element-state assoc ::status :connected)
                      "connect"))

                  observed-events
                  (->> (get @!element-state ::lifecycle-event-listener-counts)
                    (keep #(when (pos? (val %)) (key %)))
                    set)]
              (when (seq observed-events)
                (set-timeout! !db
                  (fn []
                    (when (contains? observed-events event-type)
                      (.dispatchEvent shadow (js/Event. event-type)))
                    (when (contains? observed-events "render")
                      (.dispatchEvent shadow (js/Event. "render")))))))

            (catch :default ex
              (log/error "Error rendering component"
                :data {:component (::name @!static-state)}
                :ex ex)))))))
  (set-timeout! !db
    (fn []
      (expire-elements! !db)
      (when-let [after-render (get-in @!db [::state ::after-render])]
        (try
          (after-render !db)
          (catch :default ex
            (log/error "Error in after-render callback" :ex ex))))))
  nil)

(defn- schedule-animation-frame!
  [!db]
  (when-not (get-in @!db [::state ::pending-render-id])
    (rstore/patch! !db
      {:path [::state ::pending-render-id]
       :change [:value (.requestAnimationFrame
                         ^js/Window (get-in @!db [::state ::window])
                         (partial update-dirty-elements! !db))]})))

(defn- invalidate!
  [!db ^js/HTMLElement element ignore-tags?]
  (let [!element-state (get-private-state element)]
    (when (not= :disconnected (::status @!element-state))
      (rstore/patch! !db
        [{:path [::state ::elements-to-patch]
          :change [:conj element]}
         {:path [::state ::elements-to-render]
          :change [:conj element]}])
      (when ignore-tags?
        (swap! !element-state assoc ::ignore-tags-on-next-patch? true))
      (schedule-animation-frame! !db))))


(defn- update-element-class
  [!db class component-name
   {:keys [props view focus inherit-doc-css? form-associated? extra-properties]
    :as component-spec}
   old-component-spec]
  (let [^js proto (.-prototype class)
        !static-state (get-private-state class)
        document (get-in @!db [::state ::document])
        attr->prop-spec (->> props vals
                          (keep
                            (fn [prop-spec]
                              (when (and (:attr prop-spec) (nil? (:state-factory prop-spec)))
                                [(:attr prop-spec) prop-spec])))
                          (into {}))
        init-props! (fn [^js/Node instance]
                      (let [!instance-state (get-private-state instance)
                            attr-reader (component-registry/get-attribute-reader !db component-name)]
                        (doseq [prop-spec (vals props)
                                :when (not (contains? (::prop-vals @!instance-state) (:prop prop-spec)))]
                          (cond
                            (:state-factory prop-spec)
                            (try
                              (let [state ((:state-factory prop-spec) instance)
                                    watch-key [::state-prop (:prop prop-spec) instance]
                                    cleanup-fn (fn []
                                                 (swap! !instance-state update ::prop-vals dissoc (:prop prop-spec))
                                                 (remove-watch state watch-key)
                                                 (when-let [state-cleanup (:state-cleanup prop-spec)]
                                                   (state-cleanup state instance)))]
                                (when-not (util/can-watch? state)
                                  (throw (ex-info "State factory produced something not watchable"
                                           {:state state
                                            :component component-name})))
                                (add-watch state watch-key
                                  (fn [_ _ _ new-val]
                                    (swap! !instance-state assoc-in [::prop-vals (:prop prop-spec)] new-val)
                                    (invalidate! !db instance false)))
                                (when (satisfies? IDeref state)
                                  (swap! !instance-state update ::prop-vals assoc (:prop prop-spec) @state))
                                (swap! !instance-state update ::cleanup-fns (fnil conj #{}) cleanup-fn))
                              (catch :default e
                                (log/error "Error initializing state prop" :ex e)))

                            (and (:attr prop-spec) (-> @!instance-state ::prop-vals (contains? (:prop prop-spec)) not))
                            (swap! !instance-state assoc-in [::prop-vals (:prop prop-spec)]
                              (some-> (.getAttribute instance (:attr prop-spec))
                                (attr-reader (:attr prop-spec) component-name)))))))
        default-css (cond-> [(get-in @!db [::state ::default-css])]
                      inherit-doc-css?
                      (into (->> (.querySelectorAll document "link[rel=\"stylesheet\"]")
                              .values es6-iterator-seq
                              (map (fn [^js/HTMLElement link] (.-href link)))
                              (remove str/blank?)
                              (mapv (partial get-stylesheet-object !db)))))]
    (swap! !static-state merge
      {::view view
       ::focus focus
       ::name component-name
       ::default-css default-css})

    ;; If this component's fields have been indexed,
    ;; remove it from the index since its fields may have changed
    (purge-fields-index-for-class !db class)

    ;; Delete properties on the prototype.
    (doseq [property-name (::removable-properties @!static-state)]
      (js-delete proto property-name))
    (swap! !static-state assoc ::removable-properties #{})

    (js/Object.defineProperties
      class
      #js{:observedAttributes
          #js{:value (to-array (keys attr->prop-spec))
              :configurable true}
          :formAssociated
          #js{:value (boolean form-associated?)
              :configurable true}})

    (js/Object.defineProperties
      proto
      #js{:connectedCallback
          #js{:value
              (fn []
                (let [^js/Node this (js* "this")]
                  (when (= :disconnected (::status @(get-private-state this)))
                    (swap! !static-state update ::instances conj this)
                    (swap! (get-private-state this) assoc ::status :connecting)
                    (init-props! this)
                    (invalidate! !db this false))))
              :configurable true}
          :disconnectedCallback
          #js{:value
              (fn []
                (rstore/patch! !db
                  {:path [::state ::elements-to-disconnect]
                   :change [:conj (js* "this")]}))
              :configurable true}

          :attributeChangedCallback
          #js{:value
              (fn [attr-name _old-val new-val]
                (let [^js/Node instance (js* "this")
                      !instance-state (get-private-state instance)
                      attr-reader (component-registry/get-attribute-reader !db component-name)]
                  (when-let [prop-spec (get attr->prop-spec attr-name)]
                    (swap! !instance-state assoc-in [::prop-vals (:prop prop-spec)]
                      (some-> new-val
                        (attr-reader attr-name component-name)))
                    (invalidate! !db instance false))))
              :configurable true}

          :elementName
          #js{:value (markup/kw->el-name component-name)
              :writable false
              :configurable true}

          :componentName
          #js{:value component-name
              :writable false
              :configurable true}})

    (doseq [prop-spec (filter :field (vals props))
            :let [prop-name (:prop prop-spec)]]
      (swap! !static-state update ::removable-properties conj (:field prop-spec))

      (js/Object.defineProperty
        proto
        (:field prop-spec)
        #js{:get
            (fn []
              (-> (js* "this") get-private-state deref ::prop-vals (get prop-name)))
            :set
            (if (:state-factory prop-spec)
              (js* "undefined")
              (fn [x]
                (let [instance (js* "this")
                      !instance-state (-> instance get-private-state)]
                  (when-not (identical? x (get-in @!instance-state [::prop-vals prop-name]))
                    (swap! !instance-state assoc-in [::prop-vals prop-name] x)
                    (invalidate! !db instance false)))))
            :configurable true}))

    (doseq [[property-name {:keys [get set value writable?]}] extra-properties]
      (swap! !static-state update ::removable-properties conj (name property-name))

      (js/Object.defineProperty
        proto
        (name property-name)
        #js{:get get :set set :value value :writable writable?}))

    (doseq [instance (::instances @!static-state)
            :let [!instance-state (get-private-state instance)]]
      (swap! !instance-state update ::prop-vals
        (fn [prop-vals]
          (let [new-spec-props (:props component-spec)
                old-spec-props (:props old-component-spec)]
            (reduce-kv
              (fn [m k _v]
                (cond
                  (or
                    (not (contains? new-spec-props k))
                    (and (:state-factory new-spec-props) (nil? (:state-factory old-spec-props))))
                  (dissoc m k)

                  (or
                    (not (contains? old-spec-props k))
                    (and (nil? (:state-factory new-spec-props)) (:state-factory old-spec-props)))
                  (assoc m k (get (::props @!instance-state) k))

                  :else
                  m))
              prop-vals
              prop-vals))))
      (init-props! instance)
      (invalidate! !db instance true))))


(defonce ^:private !render-order-seq (atom 0))

(defn- update-component
  [!db component-name
   {focus :focus :as component-spec}
   old-component-spec]
  (let [el-name (markup/kw->el-name component-name)
        registry ^js/CustomElementRegistry (get-in @!db [::state ::registry])]
    (if-let [existing (.get registry el-name)]
      (update-element-class !db existing component-name component-spec old-component-spec)
      (let [html-element-class (.-HTMLElement ^js (get-in @!db [::state ::window]))
            new-class (js* "(class extends ~{} {
                                constructor() {
                                    super();
                                    this['init']()
                                }
                            })" html-element-class)
            !static-state (get-private-state new-class)]

        (swap! !static-state assoc ::instances #{})

        (js/Object.defineProperty (.-prototype new-class) "init"
          #js{:value
              (fn []
                (let [instance ^js/HTMLElement (js* "this")
                      !instance-state (get-private-state instance)

                      shadow ^js/ShadowRoot
                      (or (.-shadowRoot instance)
                        (.attachShadow instance
                          #js{:mode "open"
                              :delegatesFocus (= focus :delegate)}))]

                  (reset! !instance-state
                    {::shadow shadow
                     ::internals (when (fn? (.-attachInternals instance)) (.attachInternals instance))
                     ::prop-vals {}
                     ::lifecycle-event-listener-counts {}
                     ::doms-on-focus-path #{}
                     ::status :disconnected
                     ::ignore-tags-on-next-patch? false
                     ::rendered-vdom nil})

                  (obj/set instance render-order-sym (swap! !render-order-seq inc))

                  (.addEventListener shadow "focusin"
                    (fn [^js/FocusEvent event]
                      (let [event-path (.composedPath event)
                            shadow-index (.indexOf event-path (::shadow @!instance-state))]
                        (swap! !instance-state assoc ::doms-on-focus-path (set (.slice event-path 0 shadow-index)))))
                    true)
                  (.addEventListener shadow "focusout"
                    (fn [^js/FocusEvent _event]
                      (when (nil? (.-activeElement shadow))
                        (swap! !instance-state assoc ::doms-on-focus-path #{})))
                    true)

                  (let [orig-add-event-listener (.bind (.-addEventListener shadow) shadow)
                        orig-remove-event-listener (.bind (.-removeEventListener shadow) shadow)]
                    (js/Object.defineProperties shadow
                      #js{:addEventListener
                          #js{:value
                              (fn add-event-listener-override [type & others]
                                (case type
                                  ("connect" "disconnect" "update" "render" "focus" "blur")
                                  (swap! !instance-state update-in [::lifecycle-event-listener-counts type] (fnil inc 0))
                                  nil)
                                (apply orig-add-event-listener type others))
                              :configurable false
                              :writable false}
                          :removeEventListener
                          #js{:value
                              (fn remove-event-listener-override [type & others]
                                (case type
                                  ("connect" "disconnect" "update" "render" "focus" "blur")
                                  (swap! !instance-state update-in [::lifecycle-event-listener-counts type] (fnil dec 0))
                                  nil)
                                (apply orig-remove-event-listener type others))
                              :configurable false
                              :writable false}}))))
              :configurable false
              :writable false})
        (update-element-class !db new-class component-name component-spec old-component-spec)
        (.define registry el-name new-class))))
  nil)

(defn- start-hot-reload-observer!
  [!db]
  (letfn
   [(update-link [^js/Node original url]
      (let [^js/Node clone (.cloneNode original)
            !clone-state (get-private-state clone)]
        (set! (.-href clone) url)
        (reset! !clone-state @(get-private-state original))
        (.insertAdjacentElement original "beforebegin" clone)
        (.addEventListener clone "load"
          (fn [_]
            (set-timeout! !db #(.remove original) 60))
          #js{:once true})
        (rstore/patch! !db
          [{:path [::css-link-elements]
            :change [:conj clone]}
           {:path [::css-link-elements]
            :change [:clear original]}])))
    (observer-cb [^js/Array records]
      (let [path->css-link-elements
            (delay
              (group-by
                (fn [^js/Node x]
                  (-> x .-href (absolute-url !db) .-pathname))
                (get-in @!db [::state ::css-link-elements])))]
        (doseq [^js record records, ^js/Node node (-> record .-addedNodes array-seq)
                :when (and (= "LINK" (.-nodeName node)) (= "stylesheet" (.-rel node)))
                :let [created-link-url (absolute-url !db (.-href node))]
                :when (local-url? !db created-link-url)]

          (rstore/patch! !db
            {:path [::state ::href-overrides]
             :change [:assoc (.-pathname created-link-url) (str created-link-url)]})

          (doseq [^js/Node matching-link (get @path->css-link-elements (.-pathname created-link-url))]
            (update-link matching-link created-link-url))
          (doseq [[original-url-str stylesheet-object] (get-in @!db [::state ::css-stylesheet-objects])
                  :let [original-url (js/URL. original-url-str)]
                  :when (and
                          (local-url? !db original-url)
                          (= (.-pathname original-url) (.-pathname created-link-url)))]
            (load-stylesheet-contents! stylesheet-object created-link-url)))))]
    (let [observer (js/MutationObserver. observer-cb)
          document (get-in @!db [::state ::document])
          opts #js{:childList true}]

      (rstore/patch! !db
        {:path [::state ::hot-reload-observer]
         :change [:value observer]})

      (.observe observer (.-head document) opts)
      (.observe observer (.-body document) opts))))

(defn- stop-hot-reload-observer!
  [!db]
  (when-let [observer (get-in @!db [::state ::hot-reload-observer])]
    (.disconnect observer))
  nil)

(defn install!
  [!db ^js/Window doc ^js/CustomElementRegistry cer
   & {:keys [hot-reload? disable-tags? preproc-vnode
             after-render before-render] :as opts}]
  {:pre [(some? (::component-registry/state @!db))]}
  (let [window ^js/Window (or (:window opts) js/window)]
    (core/install-plugin! !db ::state
      (fn web-components-plugin
        [!db]
        {::hot-reload? (if (some? hot-reload?) hot-reload? (and DEBUG (= js/globalThis window)))
         ::disable-tags? (if (some? disable-tags?) disable-tags? DEBUG)
         ::document doc
         ::registry cer
         ::window window
         ::class->fields-index {}
         ::css-link-elements #{}
         ::css-stylesheet-objects {}
         ::href-overrides {}
         ::elements-to-patch #{}
         ::elements-to-render #{}
         ::elements-to-disconnect #{}
         ::pending-render-id nil
         ::vdom-render-interval-id nil
         ::render-order-seq 0
         ::preproc-vnode (if preproc-vnode
                           (comp preproc-vnode markup/preproc-vnode)
                           markup/preproc-vnode)
         ::after-render after-render
         ::before-render before-render

         ::core/init
         (fn web-components-plugin-init
           []
           (when (hot-reload-enabled? !db)
             (if (= (.-readyState doc) "complete")
               (start-hot-reload-observer! !db)
               (.addEventListener (.-defaultView doc) "load"
                 (fn []
                   (start-hot-reload-observer! !db))
                 #js{:once true})))

           ;; preemptively index some classes
           (get-fields-index-for-class !db ^js (.-HTMLElement window))
           (some->> window ^js (.-ElementInternals) (get-fields-index-for-class !db))

           (let [components-path [::component-registry/state ::component-registry/components]]
           ;; watch for new component registrations
             (rstore/watch !db ::components components-path
               (fn [old-val new-val _changed-paths]
               ;; TODO: use changed-paths to optimize
                 (doseq [[component-name component-spec] new-val
                         :let [old-component-spec (get old-val component-name)]
                         :when (not (identical? old-component-spec component-spec))]
                   (update-component !db component-name component-spec old-component-spec))))

           ;; handle existing component registrations
             (doseq [[component-name component-spec] (get-in @!db components-path)]
               (update-component !db component-name component-spec nil)))

         ;; setup vdom render interval
           (rstore/patch! !db
             [{:path [::state ::vdom-render-interval-id]
               :change [:value (js/setInterval render-vdoms-for-dirty-elements! 7 !db)]}
              {:path [::state ::default-css]
               :change [:value (compile-css !db ":host { display: contents; }")]}]))

         ::core/finl
         (fn web-components-plugin-finl
           [state]
           (log/warn "Requested removal of web components plugin, but custom element registrations can't be removed cleanly.")
           (when (hot-reload-enabled? !db)
             (stop-hot-reload-observer! !db))
           (when-let [render-id (::pending-render-id state)]
             (js/cancelAnimationFrame render-id))
           (js/clearInterval (::vdom-render-interval-id state))
         ;; TODO: see if we can clean up the classes enough that they can be reused
           )})
      (dissoc opts :hot-reload? :disable-tags? :preproc-vnode :after-render :before-render))))

(defn remove!
  [!db]
  (core/remove-plugin! !db ::state))


(when-check
  (defn do-steps [!db zero-element & step-fns]
    (if (empty? step-fns)
      (js/Promise. ;; this gives async stuff time to finish up
        (fn [resolve]
          (set-timeout! !db resolve 120)))
      (js/Promise.
        (fn [resolve reject]
          (set-timeout! !db
            (fn []
              (.requestAnimationFrame
                ^js/Window (get-in @!db [::state ::window])
                (fn []
                  (try
                    (-> ((first step-fns) zero-element)
                      js/Promise.resolve
                      (.then
                        (fn []
                          (resolve (apply do-steps !db zero-element (rest step-fns)))))
                      (.catch reject))
                    (catch :default ex
                      (reject ex)))))))))))

  (defn steps [!db create-element-fn & step-fns]
    (let [el (create-element-fn)]
      (.append ^js/Document (get-in @!db [::state ::document]) el)
      (.then (apply do-steps !db el step-fns)
        (fn [] (.remove el)))))

  (defn with-test-db [f]
    (let [win ((resolve 'dev/create-window))
          !db (core/create-db)]
      (component-registry/install! !db)
      (install! !db (.-document win) (.-customElements win) :window win :hot-reload? false)
      (component-registry/reg-component !db ::echo
        :props #{:vdom}
        :view (fn [{:keys [vdom]}] vdom))

      (.finally (js/Promise.resolve (f !db))
        (fn []
          (.then ^js/Promise ((resolve 'dev/dispose-window) win)
            (fn []
              (remove! !db)))))))


  (defn create-element [!db name]
    (.createElement ^js/Document (get-in @!db [::state ::document]) (core/element-name name))))

(check ::component-renders
  (with-test-db
    (fn [!db]
      (steps !db #(create-element !db ::echo)
        (fn [^js/HTMLElement echo]
          (set! (.-vdom echo) [:div "FOO"]))
        (fn [^js/HTMLElement echo]
          (assert (= 1 (-> echo .-shadowRoot .-childNodes .-length)))
          (let [first-child (-> echo .-shadowRoot .-firstChild)]
            (assert (= "DIV" (.-nodeName first-child)))
            (assert (= "FOO" (.-innerText first-child)))))))))

(check ::component-reacts-to-property
  (with-test-db
    (fn [!db]
      (steps !db #(create-element !db ::echo)
        (fn [^js/HTMLElement el]
          (assert (= "" (-> el .-shadowRoot .-firstChild .-nodeValue)))
          (set! (.-vdom el) "BAR"))
        (fn [^js/HTMLElement el]
          (assert (= "BAR" (-> el .-shadowRoot .-firstChild .-nodeValue))))))))

(check ::component-reacts-to-attribute
  (with-test-db
    (fn [!db]
      (steps !db #(create-element !db ::echo)
        (fn [^js/HTMLElement el]
          (assert (= "" (-> el .-shadowRoot .-firstChild .-nodeValue)))
          (.setAttribute el "vdom" "BAR"))
        (fn [^js/HTMLElement el]
          (assert (= "BAR" (-> el .-shadowRoot .-firstChild .-nodeValue))))))))

(check ::component-reacts-to-binding
  (with-test-db
    (fn [!db]
      (let [!atom (atom "BAR")]
        (steps !db  #(create-element !db ::echo)
          (fn [^js/HTMLElement el]
            (assert (= "" (-> el .-shadowRoot .-firstChild .-nodeValue)))
            (set! (.-vdom el)
              [:input :#bind {:value !atom}]))
          (fn [^js/HTMLElement el]
            (assert (= "BAR" (-> el .-shadowRoot .-firstChild .-value)))
            (reset! !atom "BAZ"))
          (fn [^js/HTMLElement el]
            (assert (= "BAZ" (-> el .-shadowRoot .-firstChild .-value))))
          (fn [^js/HTMLElement el]
            (set! (.-vdom el) [:input]))
          (fn [_]
            (reset! !atom "FOO"))
          (fn [^js/HTMLElement el]
            (assert (contains? #{"" "null"} (-> el .-shadowRoot .-firstChild .-value)))))))))

(check ::component-listens-to-events
  (with-test-db
    (fn [!db]
      (let [!atom (atom nil)
            InputEvent (.-InputEvent ^js (get-in @!db [::state ::window]))]
        (steps !db #(create-element !db ::echo)
          (fn [^js/HTMLElement el]
            (assert (= "" (-> el .-shadowRoot .-firstChild .-nodeValue)))
            (set! (.-vdom el)
              [:input :#on {:input #(reset! !atom (-> % .-target .-value))}]))
          (fn [^js/HTMLElement el]
            (let [input-el (-> el .-shadowRoot .-firstChild)]
              (assert (= "" (.-value input-el)))
              (set! (.-value input-el) "BAR")
              (.dispatchEvent input-el (InputEvent. "input"))))
          (fn [^js/HTMLElement el]
            (assert (= "BAR" @!atom))
            (set! (.-vdom el) [:input]))
          (fn [^js/HTMLElement el]
            (let [input-el (-> el .-shadowRoot .-firstChild)]
              (set! (.-value input-el) "BAZ")
              (.dispatchEvent input-el (InputEvent. "input"))))
          (fn [^js/HTMLElement _el]
            (assert (= @!atom "BAZ"))))))))

(check ::component-disconnects
  (with-test-db
    (fn [!db]
      (let [!disconnected? (atom false)]
        (steps !db #(create-element !db ::echo)
          (fn [^js/HTMLElement el]
            (set! (.-vdom el)
              [::echo
               :vdom
               [:root>
                :#on {:disconnect #(reset! !disconnected? true)}]]))
          (fn [^js/HTMLElement el]
            (set! (.-vdom el) nil))
          (fn [_]
            (js/Promise. (fn [resolve] (set-timeout! !db resolve 10))))
          (fn [_]
            (assert (true? @!disconnected?))))))))
