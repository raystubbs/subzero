(ns zro.plugins.component-registry
  (:require 
   [zro.rstore :refer [IRStore] :as rstore]))

(defn get-attribute-writer
  [!db component-name]
  ;; TODO
  )

(defn get-attribute-reader
  [!db component-name]
  ;; TODO
  )

(defn- normalize-prop-spec
  [prop-name prop-spec]
  (case prop-spec
    :attr {:attr (-> prop-name name util/snake-case)
           :prop prop-name}
    :field {:field (-> prop-name name util/cammel-case)
            :prop prop-name}
    :default {:field (-> prop-name name util/cammel-case)
              :attr (-> prop-name name util/snake-case)
              :prop prop-name}
    (cond
      (satisfies? IWatchable prop-spec)
      {:state-factory (constantly prop-spec) :prop prop-name}

      (fn? prop-spec)
      {:state-factory prop-spec :prop prop-name}

      (map? prop-spec)
      (cond-> prop-spec
        :always
        (assoc :prop prop-name)

        (:state prop-spec)
        (-> (assoc :state-factory (constantly (:state prop-spec))) (dissoc :state-cleanup))

        (not (or (:field prop-spec) (:state prop-spec) (:state-factory prop-spec)))
        (assoc :field (-> prop-name name util/cammel-case))))))
