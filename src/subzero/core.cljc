(ns subzero.core
  (:require
   [subzero.rstore :as rstore]
   [subzero.impl.markup :as markup]))

(defn create-db
  []
  (rstore/rstore {}))

(defn dispose-db
  [!db]
  (doseq [plugin-state (vals @!db)
          :let [finl-fn (::finl plugin-state)]
          :when (ifn? finl-fn)]
    (finl-fn))
  (rstore/patch! !db {:path [] :change [:value {}]}))

(defn install-plugin!
  [!db k plugin-fn]
  (let [plugin-state (plugin-fn !db)]
    (rstore/patch! !db {:path [k] :change [:value plugin-state]})
    (when (fn? (::init plugin-state))
      ((::init plugin-state))))
  nil)

(defn remove-plugin!
  [!db k]
  (when-let [plugin-state (get @!db k)]
    (when (fn? (::finl plugin-state))
      ((::finl plugin-state)))
    (rstore/patch! !db {:path [] :change [:clear k]})))

(defn element-name
  [kw]
  (markup/kw->el-name kw))

#?(:cljs
   (do
     (defn js-proxy [x & {:keys [get]}]
       (js* "new Proxy(~{}, ~{})" x #js{:get get}))
     
     (def ^:private iequiv-prop "cljs$core$IEquiv$_equiv$arity$2")
     (defonce ^:private key-eq-sym (js/Symbol "equivKey"))
     
     (defn with-ident-eq "
     Returns a version of `x` with IEquiv overridden with
     an identity check.  Useful to avoid comparison overhead
     when passing large collections as web component props.
     "
       [x]
       (js-proxy x
         :get
         (fn [target prop receiver]
           (if (= prop iequiv-prop)
             identical?
             (js/Reflect.get target prop receiver)))))
     
     (defn with-const-eq "
     Returns a version of `x` with IEquiv overridden to compare
     against a given constant.  Meant to help optimize prop
     comparisons for large collections.
     " [k x]
       (letfn [(equiv-fn [_ other]
                 (let [other-k (js* "~{}[~{}]" other key-eq-sym)]
                   (and
                     (not= (js* "undefined") other-k)
                     (= k other-k))))]
         (js-proxy x
           :get
           (fn [target prop receiver]
             (cond
               (= key-eq-sym prop)
               k
               
               (= iequiv-prop prop)
               equiv-fn
               
               :else
               (js/Reflect.get target prop receiver))))))
     nil))

