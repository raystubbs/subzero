(ns test
  (:require
   [check.core :as check]
   [subzero.core]
   [subzero.rstore]
   [subzero.plugins.html]))

(defn handle-status
  [status]
  (when-not (some #(= ::check/pending %) (vals status))
    (when (some #(= ::check/failure %) (vals status))
      (System/exit 1))))

(defn- run
  [& _args]
  (add-watch check/!status ::status (fn [_ _ _ status] (handle-status status)))
  (handle-status @check/!status))
