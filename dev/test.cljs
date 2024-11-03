(ns test
  (:require
   [check.core :as check]
   [subzero.core]
   [subzero.rstore]
   [subzero.plugins.html]
   [subzero.plugins.web-components]))

(defn handle-status
  [status]
  (when-not (some #(= ::check/pending %) (vals status))
    (when (some #(= ::check/failure %) (vals status))
      (js/process.exit 1))))

(add-watch check/!status ::status (fn [_ _ _ status] (handle-status status)))
(handle-status @check/!status)
