(ns bizlogic.tools.templates
  "HTML templating for Clojure and ClojureScript.

  * Combine HTML fragments into complete documents

  * Extract HTML fragments from documents for use in ClojureScript

  * Generate functions which return 'filled-in' HTML when passed a map
    of values

  * Generate functions/data which can be used to dynamically updated
    templates after they have been added to the DOM

  The functions in this namespace are either called from Clojure and
  used from macros to produce functions or data which can be used in
  ClojureScript.
  "
  (:use net.cgrand.enlive-html)
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import java.io.File))

(defn render
  "Given a seq of Enlive nodes, return the corresponding HTML string."
  [t]
  (apply str (emit* t)))

(declare construct-html)

;; We need to use this instead of Enlive's html-snippet, because
;; html-snippet throws away the doctype
(defn html-parse
  "Parse a string into a seq of Enlive nodes."
  [s]
  (html-resource (java.io.StringReader. s)))

(defn- html-body [name]
  (let [nodes (html-resource name)]
    (or
     (:content (first (select nodes [:body])))
     nodes)))

(defn- include-html [h]
  (let [includes (select h [:_include])]
    (loop [h h
           includes (seq includes)]
      (if includes
        (let [file (-> (first includes) :attrs :file)
              include (construct-html (html-body file))]
          (recur (transform h [[:_include (attr= :file file)]] (substitute include))
                 (next includes)))
        h))))

(defn- valid-tags [c] (filter (every-pred map? #(contains? % :tag) ) c))

(defn- replace-html [h c]
  (let [id (-> c :attrs :id)
        tag (:tag c)
        selector (keyword (str (name tag) "#" id))]
    (transform h [selector] (substitute c))))

(defn- wrap-html [h]
  (let [within (seq (select h [:_within]))]
    (if within
      (let [file (-> (first within) :attrs :file)
            outer (construct-html (html-resource file))
            content (valid-tags (:content (first within)))]
        (loop [outer outer
               content (seq content)]
          (if content
            (recur (replace-html outer (first content)) (next content))
            outer)))
      h)))

(defn construct-html
  "Process a seq of Enlive nodes looking for `_include` and `_within` tags.
  Occurrences of `_include` are replaced by the resource to which they
  refer. The contents of `_within` tags are inserted into the resource
  to which they refer. `_within` is always the top-level tag in a file.
  `_include` can appear anywhere. Files with `_include` can reference
  files which themselves contain `_include` or `_within` tags, to an
  arbitrary level of nesting.

  For more information, see '[Design and Templating][dt]' in the project
  wiki.

  Returns a seq of Enlive nodes.

  [dt]: https://github.com/brentonashworth/one/wiki/Design-and-templating"
  [nodes]
  (wrap-html (include-html nodes)))

(defn load-html
  "Accept a file (a path to a resource on the classpath) and return a
  HTML string processed per construct-html."
  [file]
  (render (construct-html (html-resource file))))

(defn html-dependencies
  "Returns a seq of filenames referenced in _within or _include tags
starting with file, to an arbitrary level of nesting."
  [file]
  (letfn [(find-dep-files [nodes filenames]
          (if-let [dep-nodes (seq (concat (select nodes [:_include])
                                          (select nodes [:_within])))]
            (let [fs (remove nil? (map (comp :file :attrs) dep-nodes))]
              (recur (mapcat html-resource fs) (concat filenames fs)))
            filenames))]
    (find-dep-files (html-resource file) nil)))
