(ns subzero.core
  (:require
   [subzero.rstore :as rstore]))

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
    (when (fn? (::init plugin-state))
      ((::init plugin-state)))
    (rstore/patch! !db {:path [k] :change [:value plugin-state]}))
  nil)

(defn remove-plugin!
  [!db k]
  (when-let [plugin-state (get @!db k)]
    (when (fn? (::finl plugin-state))
      ((::finl plugin-state)))
    (rstore/patch! !db {:path [] :change [:clear k]})))

