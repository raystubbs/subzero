# SubZero
Build [web components](https://www.webcomponents.org/introduction)
in Clojure/Script (the easy way).

## Highlights
- Depends only on the Clojure/Script runtime and browser APIs
- Small, simple, easy
- Hot reload friendly
- Focus-aware virtual DOM reconciliation
- 'Good enough' performance with progressive optimization tools

## Project Status
Experimental.  Major breaking changes unlikely, but possible.

## Example: Custom Element
```clojure
(ns increment-counter
  (:require
   [subzero.core :as sz]
   [subzero.plugins.component-registry :as reg]
   [subzero.plugins.web-components :as wc]))

(defn on-click
  [event]
  (let [increment-button (.-host (.-currentTarget event))
        clicks           (js/parseInt (.-clicks increment-button))]
    (set! (.-clicks increment-button) (inc clicks))))

(defn button-view
  [{:keys [clicks]}]
  [:root> {:#on {:click on-click}}
   [:button (str "Clicked " clicks " times")]])


(defonce !db
 (doto (sz/create-db)
  (reg/install!)
  (wc/install! js/document js/customElements)))

(reg/reg-component !db :incrementing-button
  :view button-view
  :props #{:clicks})
```
```html
<incrementing-button clicks="0"></incrementing-button>
```

## Example: SSR (With Declarative Shadow DOM)
```clojure
(ns identity-card
  (:require
   [subzero.core :as sz]
   [subzero.plugins.component-registry :as reg]
   [subzero.plugins.html :as html]))

(defn identity-card-view
  [{:keys [name age sex]}]
  [:root>
   [:table
    [:tbody
     [:tr
      [:th "Name"]
      [:td name]]
     [:tr
      [:th "Age"]
      [:td age]]
     [:tr
      [:th "Sex"]
      [:td sex]]]]])

(defonce !db
 (doto (sz/create-db)
  (reg/install!)
  (html/install!)))

(reg/reg-component !db :identity-card
  :view identity-card-view
  :props #{:name :age :sex})

(print
 (html/html !db {:doctype "html"}
  [:identity-card
   :name "John Doe"
   :age 46
   :sex "Male"]))
```
```html
<!DOCTYPE html>
<html>

<body>
    <identity-card name="John Doe" age="46" sex="Male">
        <template shadowrootmode="open">
            <table>
                <tbody>
                    <tr>
                        <th>Name</th>
                        <td>John Doe</td>
                    </tr>
                    <tr>
                        <th>Age</th>
                        <td>46</td>
                    </tr>
                    <tr>
                        <th>Sex</th>
                        <td>Male</td>
                    </tr>
                </tbody>
            </table>
        </template>
    </identity-card>
</body>

</html>
```

## Component Registration Options

### `:view`
A function which takes a map of prop values, and yields the markup to be used in
rendering the component.

### `:props`
Either a set or a map specifying the props to be provided to the `:view`
function, and how the library should source them.

If given as a set of keywords, these are taken as the prop names; with a value
of `:default` being implied for each.  For simple components, this is the most
concise, and often the correct, way to specify the component's props.  See below
for the behavior of `:default` props.

If given as a map, the key for each entry is taken as the prop name.  This is the
key that'll be used in the map of prop values passed to the `:view` function.
The value of this map determines where the prop value comes from, and can be one
of the following:

- `:field` - SubZero will generate a matching JavaScript property for this prop.
  The name of this property will be a cammelCase version of the prop name.  The
  value of the property reflects that of the prop, updating the property updates
  the prop.
- `:attr` - The prop value will be sourced from element attributes with the same
  name as the prop.  Attribute writers and readers can be registered to
  customize how the library serializes values as attributes, or parses attribute
  strings back into useful values.
- `:default` - A combination of `:field` and `:attr`.  The current prop value
  will be the last updated of either the JavaScript property, or the element
  attribute matching the prop name.
- A function - Equivalent to `{:state-factory the-function}`.  See below.
- An `IWatchable` - Equivalent to `{:state-factory (constantly
<the-watchable>)}`.  See below.
- `{:state-factory factory-fn :state-cleanup ?cleanup-fn :field ?field-name}` -
  The `factory-fn` will be called to produce an `IWatchable`, which SubZero will
  watch for new prop values.  If the returned value also satisfies `IDeref`,
  it'll be deref'd for an initial prop value.  An optional `cleanup-fn` can be
  provided, which will be called to perform any cleanup when a component instances
  is disconnected from the DOM.  The optional `:field` option can provide a name
  for a read-only JavaScript property whose value will reflect that of this
  prop.
- `{:attr ?attr-name :field ?field-name}` - Similar to `:field`, `:attr`, and
  `:default`, except the field and attribute names are given explicitly.

### `:focus`
This determines how the component should handle focus.  It can be specified as
either `:self` or `:delegate`.  If not specified, the component will not be
focusable.

The `:self` option indicates that the component itself serves as some kind of
control, and should thus be focusable.  SubZero will implicitly set `tabIndex =
0` for these, if the tab index isn't otherwise given.

The `:delegate` option indicates that the component wraps some kind of control.
This causes the component's first focusable child to be focused in place of the
component itself.  See
[`delegatesFocus`](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot/delegatesFocus)
for details.  **Warning**: if changed in a hot reload, the new value won't apply
to component instances that existing prior to the change.

### `:inherit-doc-css?`
If truthy, SubZero will check the top-level document for stylesheet `<link>`
elements, and import linked stylesheets into this component.  Note that this
wraps the stylesheets with `CSSStyleSheet`, which ignores imports.

## Markup
SubZero uses a markup notation similar to that of
[Hiccup](https://github.com/weavejester/hiccup).  This is the notation that
should be produced by component `:view` functions, or passed into the HTML
rendering functions.

In brief, most values are stringified and treated as text.  The following are
the exceptions.

Vectors represent elements.  They should have a keyword (representing the
element tag) as the first value. Following that, either a prop map; or a
keyword-value sequence or props can be given.  Anything that follows makes up
the element body.

```clojure
[:div]
;; -> <div></div>

[:div :id "my-div" :class "foo" "The " [:b "body"]]
;; -> <div id="my-div" class="foo">The <b>body</b></div>

[:div {:id "my-div" :class "foo"} "The " [:b "body"]]
;; -> <div id="my-div" class="foo">The <b>body</b></div>
```

Sequences are flattened and expanded inline.

```clojure
[:ul (map (fn [x] [:li x]) ["fee" "fi" "fo" "fum"])]
;; -> <ul><li>fee</li><li>fi</li><li>fo</li><li>fum</li></ul>
```

This means `nil` isn't rendered at all.

Functions are called (passed the prop map) and their returned markup rendered as
normal.  When combined with tags (see below), this is a powerful tool for
optimization.

### Special Props
SubZero recognizes some special keys that can be given in an element's prop map,
which have special behavior.  The following special props apply to all elements.

- `:#style` - Sugar for the regular `:style` prop.  Renders a map of style
  properties (e.g `{:display :none :color :red}`).
- `:#class` - Sugar for the regular `:class` prop.  Accepts a string, keyword,
  or symbol; or a collection of the same.  Flattens, stringifies, and joins the
  values together into a class list.
- `:#on` - Registers a map of event listeners (e.g `{:click my-click-fn :focus
my-focus-fn}`).  Multiple listeners for the same event can be specified by
  namespacing the keywords (e.g `:0/click first-click-fn :1/click
second-click-fn`).
- `:#bind` - Creates reactive bindings between regular props (i.e no `#`
  prefix) and `IWatchable` things.  When the watchable thing updates, SubZero
  will update the bound prop in response.  If the watchable thing is also
  derefable then it'll be deref'd for an initial value. (e.g `[:input :#bind
{:value !my-atom} :#on {:input #(reset! !my-atom (-> % .-target .-value))}]`).
- `:#key` - Similar to React keys.  Creates a consistent mapping between this
  vdom node and a particular DOM element instance.
- `:#tag` - Like an
  [ETag](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag) for vdom
  nodes.  Used to help optimize rendering.  If a node's tag is the same across
  renders then SubZero won't need reconcile it.  This is a powerful tool for
  progressive optimizations.
- `:#opaque?` - Indicates that the contents (body) of this node are rendered by
  some other means, so SubZero shouldn't touch it.  (e.g `[:div :innerHTML
"<b>foo</b>" :#opaque? true]`).

### Regular Props
Regular props are rendered by either setting a matching JavaScript property
(if one is found on the prototype of the element being rendered), or as
attributes.  SubZero looks for properties that either match the given prop
name exactly, or match the cammelCase'd form of the prop name.  So for example
the `innerHTML` JavaScript property can be set either as `:innerHTML` or
`:inner-html`.

### The `:root>`
A component `:view` function can return a special `[:root> ...]` form as its top
level value.  This form is similar to element nodes, except its only handles
special props; and these apply to the component instance itself rather than any
child elements.

The `:root>` node shares the `:#on`, `#:style`, `:#tag`, and `:#opaque?` props
with regular vnodes, with the following caveats:
- `:#on` - The listeners are applied to the component's ShadowRoot, on which
  SubZero dispatches custom lifecycle events: `connect`, `render`, `update`,
  `disconnect`.
- `:#style` - Applies the given style properties to the component instance as
  _defaults_, which can be overidden externally.

Some additional special props can also be set on this node:
- `:#css` - A string, URL (`js/URL` or `java.net.URL`), `js/CSSStyleSheet`,
  or a collection of the same.  If the string starts with `http` then it's
  treated as a URL.  The contents are fetched and wrapped in a
  `js/CSSStyleSheet`.  Otherwise it's treated as CSS content and wrapped directly.
  After coersion, the stylesheets are adopted by the component's ShadowRoot.
  When rendering to HTML as a declarative shadow DOM, produces `<script>`
  elements instead.
- `:#internals` - A map of fields to set on the component's
  [`ElementInternals`](https://developer.mozilla.org/en-US/docs/Web/API/ElementInternals).
- `:#on-host` - Like `:#on`, but registers the listeners on the element itself,
  rather than its ShadowRoot.

## Performance Optimization
Use `#:tag` in combination with laziness and function substitution to optimize
rendering performance progressively as bottlenecks are found.  A node whose tag
is the same across renders has the following performance advantages:
- Its props don't need to be compared with the previous version, they're assumed
  to be the same.  Children also don't need to be reconciled.
- Since children don't need to be reconciled, any lazy seqs or functions found
  in the body don't need to be realized.

When rendering lists of vnodes with the same tag.  If new items can be added to
the list, or exsiting items re-arranged, then make sure to give each item a
unique `:#key`.

## Attribute Readers/Writers
You can customize how attributes are serialized and parsed (in both HTML and
custom elements) by registering handlers via `reg/reg-attribute-writers` and
`reg/reg-attribute-readers` respectively.  These take keyval seqs, with the key
for each entry being one of: 1) a component name, 2) `:default`, 3) a wildcard
pattern like `:ns-to-match/*`.

```clojure
(defn json-reader
  [attribute-string attribute-name component-name]
  (js/JSON.parse attribute-string))

(defn json-writer
  [attribute-value attribute-name component-name]
  (js/JSON.stringify attribute-value))

(reg/reg-attribute-readers :my-app/* json-reader :other-component json-reader)
(reg/reg-attribute-writers :my-app/* json-writer :other-component json-writer)
```

## Questions, Feedback, etc.
Join the [#zero-lib](https://clojurians.slack.com/archives/C06UFMY5LUW)
Clojurians channel or open an issue.
