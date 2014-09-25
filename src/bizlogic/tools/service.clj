(ns bizlogic.tools.service
  (:import [java.io File])
  (:require [io.pedestal.http :as service]
            [bizlogic.tools.build :as build]
            [bizlogic.tools.util :as util]
            [io.pedestal.log :as log]
            [clojure.java.io :as io]
            [clojure.string :as string]
            ;; the impl dependencies will go away
            ;; these next two will collapse to one
            [io.pedestal.interceptor :as interceptor
             :refer [definterceptorfn defon-response defon-request]]
            [io.pedestal.http.impl.servlet-interceptor :as servlet-interceptor]
            [io.pedestal.http.route.definition :refer [expand-routes]]
            [io.pedestal.http.ring-middlewares :as middlewares]
            [io.pedestal.http.route :as route]
            [bizlogic.tools.rendering-view.routes :as render-routes]
            [bizlogic.tools.middleware :as app-tools-middleware]
            [bizlogic.tools.host-page :as host-page]
            [bizlogic.tools.templates :refer [load-html]]
            [ring.util.response :as ring-response]
            [ring.middleware.file :as rfile]))

;; Define my own specific routes here to merge wiht toolong specific
;; routes

(definterceptorfn maybe-redirect-to-service [config]
  (interceptor/before
   ::maybe-redirect-to-service
   (fn [{:keys [request] :as context}]
     (println "Checking for redirect....")
     (let [config (or (:config request) config)
           {:keys [host port]} (get-in config [:project :api-server])
           project (get-in config [:project :name])
           port (or port
                    (get-in config/config [:ports :dev-api-servers project]))
           {:keys [app]} (:app-aspect request)]
       (println "Config is : " (:config request))
       (println "app-aspect : " (:app-aspect config))
       (println
        (format "Use api server?: %s"
                (get-in config [app :use-api-server?])))
       (println (format "Host: %s, Port: %s, " host port))
       (if (and (get-in config [app :use-api-server?])
                (not (:response context))
                port)
         (assoc context :response
                (ring-response/redirect
                 (str "http://" host ":" port (:uri request))))
         context)))))

(defon-response default-cache-control-or-no-cache
  [response]
  (update-in response [:headers "Cache-Control"] #(or % "no-cache")))

;; define service routes
(defn dev-routes
  [config]
  (expand-routes
   [[["/_tools/render" {:get (render-routes/serve-render-menu config)}
      ["/recording" {:get (render-routes/serve-recording-page config)}]
      ["/recordings/:recording"
       {:get (render-routes/serve-recording config)
        :post (render-routes/save-recording config)}]]]]))


(defn project-public [config]
  (str (io/file (or (get-in config [:project :output-root])
                  (get-in config/config [:output-root]))
         (name (get-in config [:project :name])))))

(defn projects-public [config]
  (or (get-in config [:project :output-root])
    (get-in config/config [:output-root])))

(defn dev-service*
  [routes config port]
  {:env :dev
   ;; You can bring your own non-default interceptors. Make
   ;; sure you include routing and set it up right for
   ;; dev-mode. If you do, many other keys for configuring
   ;; default interceptors will be ignored.
   ::service/interceptors [default-cache-control-or-no-cache
                           service/not-found
                           service/log-request
                           servlet-interceptor/exception-debug
                           ;; middlewares/cookies
                           (host-page/add-control-panel config)
                           app-tools-middleware/js-encoding
                           (middlewares/file-info)
                           (middlewares/params)
                           (build/builder config)
                           (middlewares/file (project-public config))
                           (middlewares/file (projects-public config))
                           (middlewares/file (:tools-public config/config))
                           ;;(route/router routes)
                           (maybe-redirect-to-service config)]
   ::service/routes routes
   ;; Root for resource interceptor that is available by default.
   ;;              ::service/resource-path nil
   ;; Choose from [:jetty :tomcat].
   ::service/type :jetty
   ::service/port port
   ::service/join? false})

(defn dev-service
  [port config]
  (-> {} ;;(dev-routes config)
      (dev-service* config port)
      service/create-server
      (#(assoc % :start-fn (::service/start-fn %)))
      (#(assoc % :stop-fn (::service/stop-fn %)))))

;; html-service ===========================================================

(def ^:dynamic client-public "client/out/public")
(def ^:dynamic client-apps-dir "client/apps")

;;outs
(def ^:dynamic *tools-public* "client/out/tools/public")
(def ^:dynamic *public* "client/out/public")
(def ^:dynamic *tools-dir* "tools/public")

(def ^:dynamic *out-subdirs* {:html ""
                                :css "stylesheets"
                                :scss "stylesheets"
                                :images "images"})
(defn- split-path [s]
  (string/split s (re-pattern (java.util.regex.Pattern/quote File/separator))))

(defn- ensure-ends-with-sep [p]
  (if (.endsWith p File/separator) p (str p File/separator)))

(defn- get-public [k]
  (when k
    (ensure-ends-with-sep
      (case k
        :public *public*
        :tools-public *tools-public*))))

(defn- make-path [public & parts]
  (str (get-public public) (string/join File/separator (vec parts))))

(defn- file-ext [path]
  (let [file-name (last path)
        i (.lastIndexOf file-name ".")]
    (.toLowerCase (subs file-name i))))

(defn- ensure-directory [dir]
  (let [path (remove empty? (split-path (str dir)))]
    (loop [dir (io/file (first path))
           children (next path)]
      (when (not (.exists dir)) (.mkdir dir))
      (when children
        (recur (io/file dir (first children)) (next children))))
    dir))

(defn- filter-files [files]
  (remove #(or (not (.exists %)) (.isDirectory %)
             (.startsWith (.getName %) ".")
             (.startsWith (.getName %) "#")
             (.endsWith (.getName %) "~")
             (.endsWith (.getName %) ".bak"))
    files))

(defmulti do-modified-action (fn [[group-tag groups]] group-tag))

(defmethod do-modified-action :html [[_ htmls]]
  (println "doing html action ...")
  (doseq [html htmls]
    ;; process the html and spit to output-dir
    ;;(.mkdirs (.getParentFile html))
    ;;(.mkdirs (.getParentFile html))
    (println html)
    (when (:modified? html)
      (let [html' (load-html (:file html))]
        (.mkdirs (.getParentFile (:output-to html)))
        (spit (:output-to html) html')))))

(defmethod do-modified-action :css [[_ css]]
  (doseq [c css]
    (when (:modified? c)
      (let []
        (.mkdirs (.getParentFile (:output-to c)))
        (io/copy (:file c) (:output-to c))))))

(defmethod do-modified-action :scss [[_ sccs]]
  )

(defmethod do-modified-action :js [[_ js]]
  (doseq [js' js]
    (when (:modified? js')
      (let []
        (.mkdirs (.getParentFile (:output-to js')))
        (println "copying js.." js)
        (io/copy (:file js') (:output-to js'))))))

(defmethod do-modified-action :img [[_ images]]
  (doseq [img images]
    (when (:modified? img)
      (ensure-directory (.getParentFile (:output-to img)))
      (io/copy (:file img) (:output-to img)))))

(defn any-modified? [[group-tag group]]
  (some #{true} (map :modified? group)))

(defn tag-modified-info [assets]
  (let []
    (map #(assoc % :modified?
                 (> (.lastModified (:file %))
                    (.lastModified (:output-to %))))
         assets)))

(defn tag-assets-dest [project assets]
  (let [public-dir (get-in config/config [:output-root])
        project-public (io/file public-dir project)
        tools-public (get-in config/config [:tools-public])]
    (map #(assoc %
            :output-to
            (let [f (:file %)]
              (case (:tag %)
                :html (io/file tools-public project (.getName f))
                :css  (io/file project-public "stylesheets" (.getName f))
                :js (io/file project-public "javascripts" (.getName f))
                :img (io/file project-public "images" (.getName f)))))
         assets)))

;; [*templates-dir* *assets-dir*]
(defn- tag-assets [assets]
  (map
   (fn [f]
     {:file f
      :tag (let [n (.getName f)]
             (condp #(.endsWith %2 %1) n
               ".html" :html
               ".js" :js
               ".css" :css
               ".scss" :scss
               ".ico" :img
               ".jpg" :img
               ".png" :img))})
   assets))

(defn prepare-assets [project]
  (let [project-dir (io/file
                      (:apps-dir config/config client-apps-dir)
                       project)]
    (println "project-dir is:.."(str project-dir))
    (flatten
      (map #(filter-files
              (file-seq (io/file project-dir %)))
        ["templates" "assets"]))))

(defn process-assets [project]
  (let [grouped-assets (group-by #(:tag %)
                         (tag-modified-info
                           (tag-assets-dest project
                             (tag-assets
                               (prepare-assets project)))))]
    (doseq [group grouped-assets]
      (when (any-modified? group)
        (do-modified-action group)))))

(defon-request process-project
  [{:keys [path-params] :as req}]
  (let [project (:project path-params)
        asset-path (:asset-path path-params)]
    (println (format "project is : %s; asset-path is %s." project asset-path))
    (process-assets project)
    req))

(def html-routes
  (expand-routes
    [[["/"
       ["/:project/*asset-path" {:get process-project}]]]]))

(def html-interceptors
  [default-cache-control-or-no-cache
   service/not-found
   service/log-request
   servlet-interceptor/exception-debug
   (middlewares/file-info)
   ;; process-project
   (route/router html-routes)
   (middlewares/file "client/out/public")
   (middlewares/file "client/out/tools/public")])


;; use chrome developer tools to edit source files directly under vcs
;; use the server to copy changes
;; Also use this server to emulate any dynamic building of html files that
;; will occur in the production server using enlive for example.

(defn html-service
  [port]
  (-> {:env :dev
       ::service/interceptors html-interceptors
       ::service/type :jetty
       ::service/port port
       ::service/join? false}
    service/create-server
    (#(assoc % :start-fn (::service/start-fn %)))
    (#(assoc % :stop-fn (::service/stop-fn %)))))
