(ns subzero.core
  (:require
   [subzero.rstore :as rstore]
   [subzero.impl.markup :as markup]
   [check.core :refer [check]]))

(defn create-db
  []
  (rstore/rstore {}))

(defn dispose-db!
  [!db]
  (doseq [plugin-state (vals @!db)
          :let [finl-fn (::finl plugin-state)]
          :when (ifn? finl-fn)]
    (finl-fn plugin-state))
  (rstore/patch! !db {:path [] :change [:value {}]}))

(defn install-plugin!
  [!db k plugin-fn & {:keys [ignore-if-already-installed?]}]
  (cond
    (rstore/patch! !db {:path [k] :change [:value ::pending]} :when #(= ::none (get % k ::none)))
    (let [plugin-state
          (locking !db
            (let [plugin-state (plugin-fn !db)]
              (rstore/patch! !db {:path [k] :change [:value plugin-state]})
              plugin-state))]
      (when (fn? (::init plugin-state))
        ((::init plugin-state))))

    (not ignore-if-already-installed?)
    (throw (ex-info "Plugin with given key already installed" {:key k})))
  nil)

(defn remove-plugin!
  [!db k]
  (let [[old-db _] (rstore/patch! !db {:path [] :change [:clear k]})]
    (when-let [plugin-state (get old-db k)]
      (when (fn? (::finl plugin-state))
        ((::finl plugin-state) plugin-state)))))

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

#?(:cljs
   (do
     (check ::with-ident-eq
       (let [m1 (with-ident-eq (hash-map :foo "BAR"))
             m2 (with-ident-eq (hash-map :foo "BAR"))]
         (assert (not= m1 m2))
         (assert (= m1 m1))
         (assert (= m2 m2))
         (assert (map? m1))
         (assert (map? m2))))
     (check ::with-const-eq
       (let [m1 (with-const-eq :foo {:foo "BAR"})
             m2 (with-const-eq :foo {:foo "BAR"})
             m3 (with-const-eq :bar {:foo "BAR"})
             m4 {:foo "BAR"}]
         (assert (= m1 m2))
         (assert (not= m1 m3))
         (assert (not= m1 m4))
         (assert (map? m1))
         (assert (map? m2))
         (assert (map? m3))))))
