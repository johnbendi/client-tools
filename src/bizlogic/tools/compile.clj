(ns bizlogic.tools.compile
  (:require [config :as config]
            [bizlogic.tools.fs :as fs]
            [bizlogic.tools.util :as util]
            [cljs.closure :as cljsc
             :refer [build Compilable -compile ]]
            [cljs.js-deps :as jsdeps :refer [dependency-order]]
            [clojure.java.classpath :as classpath]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [bizlogic.tools.log :as log]
            [clojure.tools.namespace.find :as ns-find]
            [clojure.tools.namespace.file :as ns-file]
            [cemerick.pomegranate :as pomegranate]))

(def ^:dynamic *tools-public* "client/out/tools/public")
(def ^:dynamic *public* "client/out/public")
(def ^:dynamic *shared-metadata* :shared)

(defn rename-to-js
  "Rename any Clojure-based file to a JavaScript file."
  [file-str]
  (clojure.string/replace file-str #".clj\w*$" ".js"))

(defn relative-path
  "Given a directory and a file, return the relative path to the file
  from within this directory."
  [dir file]
  (.substring (.getAbsolutePath file)
              (inc (.length (.getAbsolutePath dir)))))

(defn js-file-name
  "Given a directory and file, return the relative path to the
  JavaScript file."
  [dir file]
  (rename-to-js (relative-path dir file)))

(defn sanitize
  "Replace hyphens with underscores."
  [s]
  (string/replace (name s) "-" "_"))

(def ^:dynamic *apps-base* "client/apps")

(defn starts-with-dot-or-hash [f]
  (or (.startsWith (.getName f) ".")
      (.startsWith (.getName f) "#")))

(defn filter-js-files [files]
  (filter (fn [f] (and (not (.isDirectory f))
                       (not (starts-with-dot-or-hash f))
                       (.endsWith (.getName f) ".js")))
          files))


(defn- copy-modified-js [js-sources]
  (println "copying modified js...")
  (println "js-sources: " (type js-sources))
  ;;(ensure-directory output-dir)
  (doseq [src (filter :modified? js-sources)]
    (.mkdirs (.getParentFile (:output-to src)))
    (io/copy (:file src) (:output-to src)))
  js-sources)

#_
(defn copy-to-public [files config]
  (let [apps-base (get-in config/config [:apps-dir])
        app-base (name (:project-name config))
        app-js-path (io/file apps-base app-base "js")
        apps-public (get-in config/config [:output-root])
        output-js-public (:generated-javascript config/config)]
    (->> (map (fn [f]
                (println "f is: " f)
                (assoc f :output-to
                      (io/file apps-public
                               output-js-public
                               (fs/relative-path app-js-path (:file f)))))
              files)
         copy-modified-js
         (map (fn [js'] {:file (:output-to js')})))))

(defn ns-marked-as-shared?
  "Is the namespace of the given file marked as shared?"
  ([jar file-name]
     (when-let [ns-decl (ns-find/read-ns-decl-from-jarfile-entry jar file-name)]
       (*shared-metadata* (meta (second ns-decl)))))
  ([file-name]
     (when-let [ns-decl (ns-file/read-file-ns-decl file-name)]
       (*shared-metadata* (meta (second ns-decl))))))

(defn shared-files-in-jars
  "Return all Clojure files in jars on the classpath which are marked
  as being shared."
  []
  (for [jar (classpath/classpath-jarfiles)
        file-name (ns-find/clojure-sources-in-jar jar)
        :when (ns-marked-as-shared? jar file-name)]
    {:js-file-name (rename-to-js file-name)
     :tag :cljs-shared-lib
     :compile? true
     :source (java.net.URL. (str "jar:file:" (.getName jar) "!/" file-name))}))

;; Overrides
(defn- replace-strings-with-files [watched-files]
  (map (fn [{:keys [source] :as m}]
         (assoc m :source (if (string? source) (io/file source) source)))
       watched-files))

(defn cljs-file?
  "Is the given file a ClojureScript file?"
  [f]
  (and (.isFile f)
       (.endsWith (.getName f) ".cljs")))

(defn clj-file?
  "Is the given file a ClojureScript file?"
  [f]
  (and (.isFile f)
       (.endsWith (.getName f) ".clj")))

(defn client-tools [& [options]]
  (let [tools-dir (io/file (name (:tools-dir config/config)))]
    (for [file (file-seq tools-dir)
          :when (and (not (.isDirectory file))
                     (not (starts-with-dot-or-hash file))
                     (or (ns-marked-as-shared? file) (cljs-file? file)))]
      (if (ns-marked-as-shared? file)
        {:js-file-name (js-file-name tools-dir file)
         :tag :cljs-shared
         :compile? true
         :source file}
        {:js-file-name (js-file-name tools-dir file)
         :tag :cljs
         :compile? true
         :source file}))))

(defn app-cljs-files [& [options]]
  (let [project (sanitize (name (get options :project-name)))
        project-cljs-dir (io/file "client/apps" project "cljs")
        app-cljs-dir (io/file project-cljs-dir project
                              (sanitize (name (:app-name options))))]
    (for [file (file-seq app-cljs-dir)
          ;;_ (println file)
          :when (and (not (.isDirectory file))
                     (not (starts-with-dot-or-hash file))
                     (or (ns-marked-as-shared? file) (cljs-file? file)))]
      (if (ns-marked-as-shared? file)
          {:js-file-name (js-file-name project-cljs-dir file)
           :tag :cljs-shared
           :compile? true
           :source file}
          {:js-file-name (js-file-name project-cljs-dir file)
           :tag :cljs
           :compile? true
           :source file}))))


(defn tag-modified-info [files config]
  (println "Tagging...")
  (let [cnt (count files)]
    (println cnt))
  (map
   (fn [f]
     (println f)
     (assoc f :modified?
            (> (.lastModified (io/file (or (:source f) (:file f))))
               (.lastModified
                (io/file (:output-to config))))))
   files))

(defn app-js-files [{:keys [project-name app-name output-dir] :as options}]
  (let [apps-dir (name (:apps-dir config/config))
        project-js-dir (io/file apps-dir (name project-name) "js")
        app-js-dir (io/file project-js-dir (sanitize app-name))
        public-js-dir (or output-dir (io/file (:output-root config/config)
                                              (:generated-js config/config)))]
    (map (fn [f] {:file f :tag :js
                  :output-to (io/file public-js-dir
                                      (fs/relative-path project-js-dir f))})
         (filter-js-files (file-seq app-js-dir)))))


(defn delete-js-file [options js-file-name]
  (let [js-file (io/file (str (:output-dir options)
                              "/"
                              js-file-name))]
    (when (.exists js-file)
      (.delete js-file))))

(defn force-compilation [options sources]
  (let [{:keys [tags triggers output-dir]} options]
    (when (and tags triggers)
      (let [res (reduce (fn [r t] (into r (get triggers t))) [] tags)
            files (filter (fn [x] (some (fn [re] (re-matches re (:js-file-name x))) res))
                          (filter #(= (:tag %) :cljs) sources))]
        (doseq [f files]
          (log/info :task :forcing :js-path (:js-file-name f))
          (delete-js-file options (:js-file-name f)))))))

(defn build-sources!
  [cljs-sources options]
  (println "building cljs sources...")
  (build
   (reify Compilable
     (-compile [_ options]
       (force-compilation options cljs-sources)
       (let [all-sources
             (flatten
              (map (fn [{:keys [js-file-name source]}]
                     (log/info :task :compiling :js-path js-file-name
                               :source source)
                     (-compile source (assoc options :output-file js-file-name)))
                   (filter :compile? cljs-sources)))]
         (dependency-order all-sources))))
   options))

(defn filter-cljs-sources [cljs-sources options]
  (filter #(> (.lastModified (io/file (:source %)))
              (.lastModified (io/file (:output-to options))))
          cljs-sources))

(defn any-modified? [sources]
  (some :modified?  sources))

(defn forced-compilation-tags [cljs]
  (let [modifieds (filter :modified? cljs)]
    (when (not (empty? modifieds)) (set (distinct (map :tag modifieds))))))

;; for testing purposes
(defn my-sources [options]
  (concat (app-cljs-files options)
          (app-js-files options)
          (client-tools options)))

(defn delete-deps-file [opts]
  (let [deps-file (io/file (:output-to opts))]
    (when (.exists deps-file)
      (.delete deps-file))))

(defn- non-cljs-files [files]
  (map (fn [f] {:source f}) files))

(defn tagged-files-in-dir [dir tag ext]
  (map (fn [f] {:source f :tag tag})
       (filter #(.endsWith % ext)
               (map #(.getAbsolutePath %) (file-seq (io/file dir))))))

(defn html-files-in
  "Return a sequence of file maps for all HTML files in the given
  directory."
  ([dir]
     (html-files-in dir :html))
  ([dir tag]
     (tagged-files-in-dir dir tag ".html")))

(defn clj-files-in
  "Return a sequence of file maps for all Clojure files in the given
  directory."
  [dir tag]
  (tagged-files-in-dir dir tag ".clj"))

(defn watched-sources
  "Return source files where the :js-file-name does not match a
  regular expression in :ignore."
  [options sources]
  (if-let [ignore (:ignore options)]
    (remove (fn [src] (some #(and (:js-file-name src)
                                 (re-matches % (:js-file-name src)))
                           (map #(if (string? %) (re-pattern %) %) ignore)))
            sources)
    sources))

;; TODO: tag modified info separately
(defn compile! [options]
  (println "options is " options)
  (let [{:keys [watch-files]} options
        classpath-sources (shared-files-in-jars)
        cljs-sources (app-cljs-files options)
        js-sources (app-js-files options)
        my-client-tools (client-tools options)
        templates (replace-strings-with-files watch-files)
        my-sources (concat cljs-sources js-sources my-client-tools templates)
        watched-sources (tag-modified-info
                         (watched-sources options my-sources) options)]
    (println ".........................")
    (println "output-to:" (:output-to options))
    (when (any-modified? watched-sources)
      (do
        (println "compiling cljs...")
        (println "------------")
        (println "------------")
        (delete-deps-file options)
        (copy-modified-js (filter #(= :js (:tag %)) watched-sources))
        (build-sources! (filter :compile? (concat watched-sources
                                                  classpath-sources))
                        (assoc options :tags
                               (forced-compilation-tags watched-sources)))
        true))))

(def ^:private build-agent (agent nil))

(defn thread-safe-compile! [options]
  (println "In thread-safe-compile ..")
  (let [p (promise)]
    (send build-agent
          (fn [_] (deliver p (try
                               (println "agent-thread" (Thread/currentThread))
                               (compile! options)
                               (catch Throwable e
                                 (do (log/error :exception e)
                                     {:error (.getMessage e)}))))))
    p))

(defn- jstr
  "Use the :js location provided in opts to construct a path to a
  JavaScript file."
  [public config & paths]
  (str (io/file (or public (get-in config/config [:output-root]))
                (or (get-in config [:project :generated-javascript])
                    (get-in config/config [:generated-javascript]))
                ;;(name (:name (:project config)))
                )))

(defn get-output-file [config {:keys [app aspect]}]
  (let [public-dir (or (:output-root config) (config/config :output-root))
        project-public-dir
        (str (io/file public-dir (name (get-in config [:project :name]))))
        out-file (get-in config [app :aspects aspect :out-file])]
    (str (io/file (fs/ensure-directory project-public-dir)
                  out-file))))

(defn cljs-compilation-options
  ([public config {:keys [app aspect] :as app-aspect}]
     (assert app "cljs-compilation-opts requires an app name!")
     (let [aspect-cfg (get config [app :aspects aspect])
           build-opts (assoc (:build (app config))
                        :project-name (:name (:project config))
                        :app-name app
                        :output-dir (jstr public config)
                        :libs (or (seq (:lib-paths (:project config)))
                                  (:lib-paths config/config))
                        :output-to (get-output-file config app-aspect))]
       (merge (if-let [optimizations (:optimizations aspect-cfg)]
                (assoc build-opts :optimizations optimizations)
                build-opts)
              (:compiler-options aspect-cfg)))))


(defn compile-cljs
  ([app-cfgs aspects]
     (doseq [app-cfg app-cfgs
             env (keep (fn [[k v]]
                         (when (:out-file v) k))
                       (select-keys (:aspects app-cfg)
                                    (seq aspects)))]
       (when (and app-cfg env)
         (println (str "compiling "
                       (get-in app-cfg [:application :app-name])
                       "/" env))
         @(thread-safe-compile!
           (cljs-compilation-options *public*
                                     app-cfg
                                     env))))))

(defn deployment-options [])


(defn compile-project
  [project app-names]
  (println "In compile-project ..")
  (println app-names)
  (doseq [app-name app-names]
    (let [app-cfg (fs/load-config project app-name)]
      (println "app-cfg.." app-cfg)
       (compile-cljs [app-cfg] [:development]))))

(defn project-compiler [configurations]
  (partial compile-project configurations))
