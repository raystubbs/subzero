(ns subzero.plugins.component-registry
  (:require
   [subzero.impl.util :as util]
   [subzero.rstore :as rstore]
   [subzero.core :as core]
   [clojure.string :as str]))

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
      (util/can-watch? prop-spec)
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

(defn reg-component
  [!db component-name
   & {:keys [view props] :as spec}]
  {:pre [(ifn? view)
         (or (nil? props) (map? props) (set? props))
         (keyword? component-name)
         (= (str component-name) (str/lower-case (str component-name)))]}
  (let [normalized-props
        (->>
          (if (map? props)
            (map normalize-prop-spec (keys props) (vals props))
            (map #(normalize-prop-spec % :default) props))
          (map (juxt :prop identity))
          (into {}))]
    (rstore/patch! !db
      {:path [::state ::components component-name]
       :change [:value (assoc spec :props normalized-props)]}))
  nil)

(defn reg-attribute-writers
  [!db & {:as attribute-writers}]
  (rstore/patch! !db
    [{:path [::state ::attribute-writers]
      :change [:into attribute-writers]}
     {:path [::state ::attribute-writers-cache]
      :change [:value (atom {})]}])
  nil)

(defn reg-attribute-readers
  [!db & {:as attribute-readers}]
  (rstore/patch! !db
    [{:path [::state ::attribute-readers]
      :change [:into attribute-readers]}
     {:path [::state ::attribute-readers-cache]
      :change [:value (atom {})]}])
  nil)

(defn- get-hierarchically
  [m !cache k]
  (when (map? m)
    (let [cached (when !cache (get @!cache k ::not-found))]
      (if (not= ::not-found cached)
        cached
        (let [found (or
                      (get m k)
                      (when-let [ns (when (keyword? k) (namespace k))]
                        (let [ns-parts (vec (str/split ns #"\."))]
                          (reduce
                            (fn [answer i]
                              (if-let [v (get m (keyword (str/join "." (subvec ns-parts 0 i)) "*"))]
                                (reduced v)
                                answer))
                            nil
                            (range (count ns-parts) -1 -1)))))]
          (when !cache
            (swap! !cache assoc k found))
          found)))))

(defn- default-attribute-writer
  [v _ _]
  v)

(defn get-attribute-writer
  [!db component-name]
  (let [{writers ::attribute-writers
         !cache ::attribute-writers-cache} (get @!db ::state)]
    (or (get-hierarchically writers !cache component-name)
      (:default writers)
      default-attribute-writer)))

(defn- default-attribute-reader
  [v _ _]
  v)

(defn get-attribute-reader
  [!db component-name]
  (let [{readers ::attribute-readers
         !cache ::attribute-readers-cache} (get @!db ::state)]
    (or (get-hierarchically readers !cache component-name)
      (:default readers)
      default-attribute-reader)))

(defn install!
  [!db & {:as opts}]
  (core/install-plugin! !db ::state
    (fn component-registry-plugin
      [!db]
      {::attribute-readers {}
       ::attribute-readers-cache (atom {})
       ::attribute-writers {}
       ::attribute-writers-cache (atom {})
       ::components {}})
    opts))

(defn remove!
  [!db]
  (core/remove-plugin! !db ::state))