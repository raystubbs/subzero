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
    (if (some #(= ::check/failure %) (vals status))
      (do
        (print "The following checks have failed:")
        (doseq [[k v] status :when (not= :check.core/success v)]
          (print "  " k))
        (js/process.exit 1))
      (print "All" (count status) "checks passed.")))
  nil)

(add-watch check/!status ::status (fn [_ _ _ status] (handle-status status)))
(handle-status @check/!status)
