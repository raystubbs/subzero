(ns subzero.plugins.html "
HTML rendering.
"
  (:require
   [clojure.string :as str]
   [subzero.impl.markup :refer [clj->css-property kw->el-name flatten-body]] 
   [subzero.impl.util :refer [str-writer str-writer->str write] :as base]
   [subzero.plugins.component-registry :as component-registry]
   [subzero.core :as core]
   [subzero.impl.markup :as markup])
  #?(:clj
     (:import
      [java.net URI URL])))

(declare ^:private write-vnode ^:private write-html)

(defn- maybe-write-shadow-dom
  [!db w tag markup-props opts]
  (when-let [spec (get-in @!db [::component-registry/state ::component-registry/components tag])]
    (let [spec-props (if (set? (:props spec))
                       (into {} (map (juxt identity (constantly :default)) (:props spec)))
                       (:props spec))
          preproc-vnode (get-in @!db [::state ::preproc-vnode])
          view-props (update-vals spec-props
                       (fn [prop-spec]
                         (get markup-props (-> prop-spec :prop name str/lower-case))))
          view-result ((:view spec) view-props)
          [shadow-root-props
           shadow-root-body] (cond
                               (and
                                 (vector? view-result)
                                 (= (first view-result) :root>))
                               (-> view-result preproc-vnode rest)

                               (seq? view-result)
                               [{} view-result]

                               :else
                               [{} [view-result]])]
      (write-vnode !db w
        (into [:template :shadowrootmode "open"]
          (concat shadow-root-body
            (keep
              (fn [css-val]
                (if
                  (or (and (string? css-val) (str/starts-with? css-val "http"))
                    #?(:cljs (instance? js/URL css-val)
                       :clj (or (instance? URI css-val) (instance? URL css-val))))
                  [:link :rel "stylesheet" :href (str css-val)]
                  [:style css-val]))
              (cond
                (coll? (:#css shadow-root-props))
                (:#css shadow-root-props)
                
                (some? (:#css shadow-root-props))
                [(:#css shadow-root-props)]))))
        opts))))

(defn- normalize-prop-names
  [props]
  (persistent!
    (reduce-kv
      (fn [m k v]
        (cond
          (or (not (keyword? k)) (namespace k))
          m
          
          (not (str/starts-with? (name k) "#"))
          (assoc! m (-> k name str/lower-case) v)
          
          :else
          (assoc! m k v)))
      (transient {})
      props)))

(defn- escape-quotes
  [s]
  (str/replace s #"\"" "&quot;"))

(defn- write-vnode
  [!db w vnode opts]
  (cond
    (vector? vnode)
    (let [preproc-vnode (get-in @!db [::state ::preproc-vnode])
          [tag props body] (preproc-vnode vnode)
          write-attribute (component-registry/get-attribute-writer !db tag)
          props (normalize-prop-names props)
          needs-id? (some some? (concat (vals (:#bind props)) (vals (:#on props))))
          has-id? (contains? props "id")
          props (cond-> props (and needs-id? (not has-id?)) (assoc "id" (name (gensym)))) 
          html-tag (kw->el-name tag)]
      (write w \< html-tag)
      (when (seq props)
        (doseq [[k v] props]
          (cond
            (string? k)
            (let [attr-name (name k)
                  attr-val (-> v (write-attribute attr-name tag) str)]
              (write w \space attr-name \= \" (escape-quotes attr-val) \"))
            
            :else
            (case k
              :#class
              (let [classes (flatten (:#class props))]
                (when (seq classes)
                  (write w " class=\"" (-> classes first name escape-quotes))
                  (doseq [class (rest classes)]
                    (write w \space (-> class name escape-quotes)))
                  (write w \")))
              
              :#style
              (let [style (:#style props)]
                (when (seq style)
                  (write w " style=\"")
                  (doseq [[k v] (:#style props) :when (some? v)]
                    (write w (name k) \: (-> v clj->css-property escape-quotes) \;))
                  (write w \")))
              
              nil))))
      (write w \>)
      (maybe-write-shadow-dom !db w tag props opts)
      (cond
        (= "script" html-tag)
        (doseq [x body]
          (write w x))
        
        :else
        (doseq [x body]
          (write-vnode !db w x opts)))
      (write w \< \/ html-tag \>)
      
      (let [node-id (get props "id")
            render-listener (get-in @!db [::state ::render-listener])
            render-binding (get-in @!db [::state ::render-binding])]
        (cond
          (and (seq (:#on props)) (nil? render-binding))
          (throw (ex-info "Listeners prop (i.e `#on`) given, yet no renderer provided" {}))
          
          :else
          (doseq [[k v] (:#on props)]
            (write-html !db w (render-listener node-id k v))))
        
        (cond
          (and (seq (:#on props)) (nil? render-binding))
          (throw (ex-info "Bindings prop (i.e `#bind`) given, yet no renderer provided" {}))
          
          :else
          (doseq [[k v] (:#bind props)]
            (write-html !db w (render-binding node-id k v))))))

    :else
    (write w (-> vnode str (str/replace #"[<>]" #(case % "<" "&gt;" ">" "&lt;"))))))

(defn write-html "
Write markup to a writer as HTML.
"
  {:arglists
   '[[!db w & markup]
     [!db w {:keys [doctype]} & markup]]}
  [!db w & args]
  (let [[opts markup] (if (map? (first args)) [(first args) (rest args)] [{} args])]
    (when-let [doctype (:doctype opts)]
      (write w "<!DOCTYPE " doctype ">"))
    (doseq [vnode (flatten-body markup)]
      (write-vnode !db w vnode opts))))

(defn html "
Format markup as an HTML string.
"
  {:arglists
   '[[!db & markup]
     [!db {:keys [doctype]} & markup]]}
  [!db & args]
  (let [w (str-writer)]
    (apply write-html !db w args)
    (str-writer->str w)))

(defn install!
  [!db
   & {:keys [render-listener render-binding preproc-vnode] :as opts}]
  (core/install-plugin! !db ::state
    (fn html-plugin [_]
      {::render-listener render-listener
       ::render-binding render-binding
       ::preproc-vnode (if preproc-vnode
                         (comp preproc-vnode markup/preproc-vnode)
                         markup/preproc-vnode)})
    (dissoc opts :render-listener :render-binding :preproc-vnode)))

(defn remove!
  [!db]
  (core/remove-plugin! !db ::state))