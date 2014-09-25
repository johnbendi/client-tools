; Copyright 2013 Relevance, Inc.

; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
; which can be found in the file epl-v10.html at the root of this distribution.
;
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
;
; You must not remove this notice, or any other, from this software.

; Adapted to the Bizlogic Corporation requirements

(ns bizlogic.tools.build
  (:require [config :as config]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [bizlogic.tools.fs :as fs]
            [bizlogic.tools.compile :as compile]
            [bizlogic.tools.host-page :refer [application-host]]
            [bizlogic.tools.templates :refer [load-html]]
            ;;[io.pedestal.app-tools.compile :as compile]
            [bizlogic.tools.scheduler :as scheduler]
            [bizlogic.tools.log :as log]
            [io.pedestal.interceptor :as interceptor :refer [definterceptorfn]])
  (:import [java.io File]))

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

;; propose to dispatch on tag type of file
;; process-files -> analyze -> modified-action ->
;; propose
;; get the list of files to process from the config assets
;; classify the files
;; dispatch to their common operations and get output path from config

(defn- ensure-directory [dir]
  (let [path (remove empty? (split-path (str dir)))]
    (loop [dir (io/file (first path))
           children (next path)]
      (when (not (.exists dir)) (.mkdir dir))
      (when children
        (recur (io/file dir (first children)) (next children))))
    dir))

(defn- filter-files [files]
  (->> files
       (remove #(or (not (.exists %)) (.isDirectory %)
                    (.startsWith (.getName %) ".")
                    (.startsWith (.getName %) "#")
                    (.endsWith (.getName %) "~")
                    (.endsWith (.getName %) ".bak")))))

(defmulti do-modified-action (fn [_ _ [group-tag groups]] group-tag))

(defmethod do-modified-action :html [config aspect [_ htmls]]
  (println "doing html action ...")
  (doseq [html htmls]
    ;; process the html and spit to output-dir
    ;;(.mkdirs (.getParentFile html))
    ;;(.mkdirs (.getParentFile html))
    (println html)
    (when (:modified? html)
      (let [html' (load-html (:file html))]
        (.mkdirs (.getParentFile (:file html)))
        (spit (:output-to html) html')))))

(defmethod do-modified-action :css [config aspect [_ css]]
  (doseq [c css]
    (when (:modified? c)
      (let []
        (.mkdirs (.getParentFile (:output-to c)))
        (io/copy (:file c) (:output-to c))))))

(defmethod do-modified-action :scss [config aspect [_ sccs]]
  )

(defmethod do-modified-action :js [config aspect [_ js]]
  (doseq [js' js]
    (when (:modified? js')
      (let []
        (.mkdirs (.getParentFile (:output-to js')))
        (println "copying js.." js)
        (io/copy (:file js') (:output-to js'))))))

(defmethod do-modified-action :img [opts aspect [_ images]]
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
                :html (io/file tools-public "design" (.getName f))
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
  (let [projects-dir (get-in config/config [:apps-dir])
        project-dir (io/file projects-dir (fs/sanitize project))]
    (println "project-dir is:.."(str project-dir))
    (flatten
     (map #(filter-files
            (file-seq (io/file project-dir %)))
          ["templates" "assets"]))))

(defn process-assets [config {:keys [app aspect]}]
  (let [project (name (get-in config [:project :name]))
        grouped-assets
        (group-by #(:tag %)
                  (tag-modified-info
                   (tag-assets-dest project
                    (tag-assets
                     (prepare-assets project)))))]
    (println "analyzing assets ....")
    (println "grouped assets :" grouped-assets)
    (doseq [group grouped-assets]
      (when (any-modified? group)
        (do-modified-action config aspect group)))))

(defn tag-tools-dest [assets]
  (let [tools-public (get-in config/config [:tools-public])
        tools-dir (get-in config/config [:tools-dir])]
    (map #(assoc % :output-to
                 (io/file tools-public
                          (fs/relative-path tools-dir (:file %))))
         assets)))

(defn prepare-tools-assets []
  (let [tools-src-dir (get-in config/config [:tools-dir])]
   (filter-files
      (file-seq (io/file tools-src-dir)))))

(defn process-tools [config aspect]
  (let [grouped-assets
        (group-by #(:tag %)
                  (tag-modified-info
                   (tag-tools-dest
                    (tag-assets
                     (prepare-tools-assets)))))]
    (doseq [group grouped-assets]
      (when (any-modified? group)
        (do-modified-action config aspect group)))))

(defn- drop-leading-sep [s]
  (if (.startsWith s File/separator)
    (subs s 1)
    s))

(defn get-project-public [dir config]
  (ensure-directory
   (str (io/file dir (name (get-in config [:project :name]))))))

(defn- make-host-page [config output-root {:keys [app aspect] :as app-aspect}]
  (let [project-public (get-project-public output-root config)]
    (when project-public (ensure-directory project-public))
    (let [app-host-page (io/file
                          (or project-public output-root)
                          (drop-leading-sep
                            (get-in config [app :aspects aspect :uri])))]
      (when (or (not (.exists app-host-page))
              (> (.lastModified
                   (io/file (io/resource (:default-template (app config)))))
                (.lastModified app-host-page)))
        (spit (str app-host-page)
              (application-host config app-aspect))))))

(defn build!
  "Builds the current project into the output directory."
  [config {:keys [app aspect] :as app-aspect}]
  (let [output-root (or (get-in config [:project :output-root])
                        (get-in config/config [:output-root]))]
    (ensure-directory output-root)
    (compile/compile!
     (compile/cljs-compilation-options *public* config app-aspect))
    (println "default-template :" (get-in config [app :default-template]))
    (make-host-page config output-root app-aspect)
    (process-assets config app-aspect)
    #_(process-tools config app-aspect)))

(def build-agent (agent nil))

(defn thread-safe-build! [config app-aspect]
  (let [p (promise)]
    (send build-agent
          (fn [_]
            (deliver p (try
                         (println "agentThread :" (Thread/currentThread))
                         (build! config app-aspect)
                         (catch Throwable e
                           (do
                             (log/info :config config :app-aspect app-aspect)
                             (log/error :exception e)
                             {:error (.getMessage e)}))))))
    p))

(def ^:private scheduler (scheduler/scheduler))

(defn- start-watcher [state configs aspect]
  (assoc state
    :task
    (scheduler/periodic scheduler 500
                        (fn [] (doseq [b (map #(thread-safe-build! % aspect) configs)]
                                (deref b))))))

(defn- stop-watcher [state]
  (scheduler/cancel (:task state)))

(defn watcher [configs aspect]
  (let [watcher-state (atom {})]
    {:state watcher-state
     :start-fn #(swap! watcher-state start-watcher configs aspect)
     :stop-fn #(stop-watcher @watcher-state)}))

(defn app-aspect-from-request [config req]
  (if-not (= (:uri req) "/")
    (let [resource (last (string/split (:uri req) #"/"))
          [app aspect] (string/split (first (string/split resource #"\.")) #"-")]
      {:app (keyword app) :aspect (keyword aspect)})
    {:app (:name (:project config)) :aspect :dev}))

(defn- build-app-aspect [config request]
  (if (= (:uri request) "/_tools/render/recording")
    (keep (fn [[k v]] (when (not= (:optimizations v) :advanced) k)) (:aspects config))
    (app-aspect-from-request config request)))

(defn- attempt-build? [request]
  (let [uri (:uri request)]
    (or (= uri "/")
        (= uri "/_tools/render")
        (= uri "/_tools/render/recording")
        (.endsWith uri ".html"))))

;; how do I deal with sub namespaces and sub pages
;; Do  I need to re think how the config map is being used
;; How do I match app sub namespaces with output dirs
;; Who gives me the app
;; How can the app be determined from the request
;; the request uri?

(definterceptorfn builder
  "Interceptor that blocks further processing until all required build
  steps have been completed."
  [config]
  (interceptor/on-request
   ::dynamic-builder
   (fn [request]
     (let [config-file (io/file (:location (:project config)) "config.clj")
           config (if (> (.lastModified config-file)
                        (:read-time (:project config)))
                    (load-file (str config-file))
                    config)
           app-aspect (build-app-aspect config request)]
       (when (attempt-build? request)
         (let [t (System/currentTimeMillis)]
           (println "config is:" config)
           (println "app-aspect: " app-aspect)
           (log/info :msg (str "LOGGING " app-aspect))
           (log/info :msg "Build started" :time t :units :ms)
           @(thread-safe-build! config app-aspect)
           (log/info :msg "Build finished"
             :duration (- (System/currentTimeMillis) t)
             :units :ms)
           (assoc request :config config :app-aspect app-aspect)))
       (assoc request :config config :app-aspect app-aspect)))))
