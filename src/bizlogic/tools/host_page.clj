(ns bizlogic.tools.host-page
  (:require [bizlogic.tools.templates
             :refer [construct-html render html-parse]]
            ;;[io.pedestal.app-tools.host-page :refer [script]]
            [net.cgrand.enlive-html :as html]
            [io.pedestal.interceptor :as interceptor
             :refer [definterceptorfn]]
            [cljs.compiler :as compiler]
            [clojure.java.io :as io])
  (:import java.io.File))

(def ^:dynamic *goog-base* "goog/base.js")

(def ^:private script-snippet
  (html/html-snippet "<script type='text/javascript'></script>"))

(defn script
  [f]
  (html/transform script-snippet [:script] f))

(defn- application-view
  [config {:keys [app aspect] :as app-aspect} & scripts]
  (let [template (or (get-in config [app :aspects aspect :template])
                     (get-in config [app :default-template]))]
    (html/transform (construct-html (html/html-resource template))
                    [:body]
                    (apply html/append scripts))))

(defn goog-base-required? [aspect]
  (not (#{:simple :advanced} (:optimizations aspect))))

(defn built-ins [config]
  (mapv (fn [[k v]]
          (case k
            :render {:name "Render"
                     :uri "/_tools/render"
                     :order (:order v)}
            nil))
        (:built-in config)))

(defn generate-control-panel [config environment]
  (html/html-snippet
   (str "<div id='pedestal-toolbar'
                  style='opacity:0'
                  onmouseover='this.style.opacity=0.75;'
                  onmouseout='this.style.opacity=0;'>"
        (apply str
               (map (fn [v]
                      (when-let [uri (:uri v)]
                        (let [url (if (:params v) (str uri "?" (:params v)) uri)]
                          (str "<a href='" url "'>" (:name v) "</a>"))))
                    (sort-by :order
                             (concat (vals (:control-panel config))
                                     (built-ins config)
                                     (vals (:aspects config))))))
        "</div>"
        "<div id='pedestal-status-panel' style='opacity:0'></div>")))

(defn- environment [config uri]
  (first (keep (fn [[k v]] (when (= uri (:uri v)) k))
               (:aspects config))))

(defn- append-control-panel [page-html config environment]
  (render (html/transform (html-parse page-html)
                          [:body]
                          (html/append (generate-control-panel config environment)))))

(definterceptorfn add-control-panel
  "Pedestal interceptor which adds a control panel to a page."
  [config]
  (interceptor/after
   ::add-control-panel
   (fn [{{:keys [headers body status] :as response} :response
         {:keys [uri]} :request
         :as context}]
     (if (and (= status 200)
              (or (= uri "/")
                  (and (.startsWith uri "/design") (.endsWith uri ".html"))
                  (or (= uri "/_tools/render") (= uri "/_tools/render/recording"))
                  (contains? (set (keep :uri (vals (:aspects config)))) uri)))
       (let [environment (environment config uri)
             aspect (get-in config [:aspects environment])
             body (if (= (type body) File) (slurp body) body)
             new-body (append-control-panel body config environment)
             new-content-length (str (count (.getBytes new-body)))]
         (-> context
             (update-in [:response :body]
                        (constantly new-body))
             (update-in [:response :headers "Content-Length"]
                        (constantly new-content-length))))
       context))))

(defn- js-path [base file]
  (str "/" base "/" file))

(defn app-js-path [config file]
  (let [project-base (get-in config [:project :name])]
    (str "/" (str (io/file (name project-base) (str file))))))

(defn add-repl [s aspect-cfg port]
  (if (:repl? aspect-cfg)
    (-> s
        (update-in [:requires]
          #(assoc % "goog.require('bizlogic.tools.client');"
                  (count %)))
        (update-in [:start]
          #(str "bizlogic.tools.client.add_repl(" % "," port ")")))
    s))

(defn- add-recording [s aspect-cfg]
  (if (:recording? aspect-cfg)
    (-> s
        (update-in [:requires]
                   #(assoc %
                      "goog.require('io.pedestal.app_tools.tooling');"
                      (count %)))
        (update-in [:start]
         #(str "io.pedestal.app_tools.tooling.add_recording(" % ")")))
    s))

(defn- add-logging [s aspect-cfg]
  (if (:logging? aspect-cfg)
    (-> s
        (update-in [:requires]
         #(assoc % "goog.require('io.pedestal.app_tools.tooling');"
                 (count %)))
        (update-in [:start]
          #(str "io.pedestal.app_tools.tooling.add_logging(" % ")")))
    s))

(defn goog-base-required? [aspect]
  (not (#{:simple :advanced} (:optimizations aspect))))

(defn- get-scripts [config {:keys [app aspect]}]
  (let [aspect-cfg (get-in config [app :aspects aspect])
        project (get-in config [:project :name])]
    (if (:scripts aspect-cfg)
      (:scripts aspect-cfg)
      (let [main (compiler/munge (:main aspect-cfg))]
        (assert main "Config must have :main or :scripts")
        (println (str "project:" project " ;app:" app))
        (let [scripts
              (-> {:requires (if (goog-base-required? aspect)
                               {(str "goog.require('" main "');") 0}
                               {})
                   :start (str main ".main()")}
                  ;;(add-recording aspect-cfg)
                  ;;(add-logging aspect-cfg)
                  (add-repl aspect-cfg
                    (get-in config/config
                      [:ports :cljs-repl project app] 40000)))]
          (println "scripts are: " scripts)
          (conj (vec (map first (sort-by second (:requires scripts))))
                (str (:start scripts) ";")))))))

(defn application-host
  "Given a configuration map and an environment, return HTML (as a
  string) that can host a ClojureScript application. The environment
  must be either `:development` or `:production` - any other value results
  in an exception. The generated HTML is based on the contents of the
  application's html template, which is loaded as an Enlive resource.

  In production mode, the HTML (as a sequence of Enlive nodes) is
  transformed via the `:prod-transform` function from the config map.

  This function is normally called in two situations:

  1. From a Ring application to dynamically generate the application
     HTML.

  2. From the build script to create static deployment artifacts."
  [config {:keys [app aspect] :as app-aspect}]
  (println "In application-host...")
  (let [aspect-cfg (get-in config [app :aspects aspect])
        transform (or (:transform aspect-cfg) identity)
        base (get-in config [:project :generated-javascript])
        scripts (cons (script (html/set-attr
                               :src
                               (app-js-path config (:out-file aspect-cfg))))
                      (mapcat #(script (html/content %))
                              (get-scripts config app-aspect)))
        scripts (if (goog-base-required? aspect-cfg)
                  (cons (script (html/set-attr
                                 :src (js-path base *goog-base*)))
                        scripts)
                  scripts)]
    (render (transform (apply application-view config app-aspect scripts)))))
