(ns test
  (:require
   [clj-arsenal.check :as check]
   [subzero.core]
   [subzero.rstore]
   [subzero.plugins.html]))

(defn handle-status
  [status]
  (when-not (some #(= ::check/pending %) (vals status))
    (if (some #(= ::check/failure %) (vals status))
      (do
        (print "The following checks have failed:")
        (doseq [[k v] status :when (not= ::check/success v)]
          (print "  " k))
        (System/exit 1))
      (print "All" (count status) "checks passed.")))
  nil)

(defn- run
  [& _args]
  (add-watch check/!status ::status (fn [_ _ _ status] (handle-status status)))
  (handle-status @check/!status))
