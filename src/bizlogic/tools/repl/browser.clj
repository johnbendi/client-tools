(ns bizlogic.tools.repl.browser
  (:refer-clojure :exclude [loaded-libs])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [cljs.compiler :as comp]
            [cljs.closure :as cljsc]
            [cljs.repl :as repl]
            [cljs.env :as env]
            [bizlogic.tools.repl.server :as server]
            [cljs.repl.browser :as browser]
            [cljs.repl.reflect :as reflect])
  (:import cljs.repl.IJavaScriptEnv
           [java.util.regex Pattern]))

(defonce browser-state (atom {:return-value-fn nil
                              :client-js nil}))

(defonce loaded-libs (atom {}))
(defonce preloaded-libs (atom {}))

(defn- set-return-value-fn
  "Save the return value function which will be called when the next
  return value is received."
  [f {:keys [project app]}]
  (swap! browser-state #(update-in % [project app] assoc :return-value-fn f)))

(defn send-for-eval
  "Given a form and a return value function, send the form to the
  browser for evaluation. The return value function will be called
  when the return value is received."
  ([form return-value-fn {:keys [project app] :as opts}]
     (send-for-eval @(server/connection opts)
       form return-value-fn opts))
  ([conn form return-value-fn {:keys [project app] :as opts}]
     (do (set-return-value-fn return-value-fn opts)
         (server/send-and-close conn 200 form "text/javascript"))))

(defn repl-client-js [{:keys [project app]}]
  (slurp @(:client-js (app (project @browser-state)))))

(defn send-repl-client-page
  [request conn opts]
  (println "sending repl client..")
  (server/send-and-close conn 200
    (str "<html><head><meta charset=\"UTF-8\"></head><body>
          <script type=\"text/javascript\">"
         (repl-client-js opts)
         "</script>"
         "<script type=\"text/javascript\">
          clojure.browser.repl.client.start(\"http://" (-> request :headers :host) "\");
          </script>"
         "</body></html>")
    "text/html" opts))

(defn send-static [{path :path :as request} conn opts]
  (if (and (:static-dir opts)
           (not= "/favicon.ico" path))
    (let [path   (if (= "/" path) "/index.html" path)
          st-dir (:static-dir opts)
          local-path (cond->
                       (seq (for [x (if (string? st-dir) [st-dir] st-dir)
                                  :when (.exists (io/file (str x path)))]
                              (str x path)))
                       (complement nil?) first)
          local-path (if (nil? local-path)
                       (cond
                         (re-find #".jar" path)
                         (io/resource (second (string/split path #".jar!/")))
                         (re-find (Pattern/compile (System/getProperty "user.dir")) path)
                         (io/file (string/replace path (str (System/getProperty "user.dir") "/") ""))
                         :else nil)
                       local-path)]
      (if local-path
        (server/send-and-close conn 200 (slurp local-path)
          (condp #(.endsWith %2 %1) path
            ".html" "text/html"
            ".css" "text/css"
            ".html" "text/html"
            ".jpg" "image/jpeg"
            ".js" "text/javascript"
            ".cljs" "text/x-clojure"
            ".map" "application/json"
            ".png" "image/png"
            "text/plain"))
        (server/send-404 conn path)))
    (server/send-404 conn path)))

(defmulti handle-post (fn [m _ _ ] (:type m)))

(defonce ordering (agent {:expecting nil :fns {}}))

(defmethod handle-post :ready [_ conn {:keys [project app] :as opts}]
  (println "Ready! Setting *print-fn*...")
  (do (swap! loaded-libs #(update-in % [project] assoc
                            app (app (project @preloaded-libs))))
      (send ordering (fn [_] {project {app {:expecting nil :fns {}}}}))
      (send-for-eval conn
                     (cljsc/-compile
                      '[(ns cljs.user)
                        (set! *print-fn* clojure.browser.repl/repl-print)] {})
                     identity opts)))

(defn add-in-order [ordering order f {:keys [project app]}]
  (update-in ordering [project app]
    (fn [{:keys [expecting fns]}]
      {:expecting (or expecting order) :fns (assoc fns order f)})))

(defn run-in-order [ordering {:keys [project app]}]
  (loop [order (:expecting (app (project ordering)))
         fns (:fns (app (project ordering)))]
    (if-let [f (get fns order)]
      (do (f)
          (recur (inc order) (dissoc fns order)))
      {project {app {:expecting order :fns fns}}})))

(defn constrain-order
  "Elements to be printed in the REPL will arrive out of order. Ensure
  that they are printed in the correct order."
  [order f opts]
  (send-off ordering add-in-order order f opts)
  (send-off ordering run-in-order opts))

(defmethod handle-post :print [{:keys [content order]} conn opts]
  (println "Handling REPL print...")
  (do (constrain-order order
        (fn [] (do (print (read-string content))
                   (.flush *out*)))
        opts)
      (server/send-and-close conn 200 "ignore__")))

(defn- return-value
  "Called by the server when a return value is received."
  [val {:keys [project app]}]
  (when-let [f (:return-value-fn (app (project @browser-state)))]
    (swap! browser-state #(update-in % [project app] dissoc :return-value-fn))
    (f val)))

(defmethod handle-post :result [{:keys [content order]} conn opts]
  (println "Handling result...")
  (constrain-order order
    (fn [] (do (return-value content opts)
               (server/set-connection conn opts)))
    opts))

(defn browserapp-eval
  "Given a string of JavaScript, evaluate it in the browser and return a map representing the
   result of the evaluation. The map will contain the keys :type and :value. :type can be
   :success, :exception, or :error. :success means that the JavaScript was evaluated without
   exception and :value will contain the return value of the evaluation. :exception means that
   there was an exception in the browser while evaluating the JavaScript and :value will
   contain the error message. :error means that some other error has occured."
  [form opts]
  (println "Readying for browser eval...")
  (let [return-value (promise)]
    (send-for-eval form
                   (fn [val] (deliver return-value val))
                   opts)
    (let [ret @return-value]
      (try (read-string ret)
           (catch Exception e
             {:status :error
              :value (str "Could not read return value: " ret)})))))

(defn load-javascript
  "Accepts a REPL environment, a list of namespaces, and a URL for a
  JavaScript file which contains the implementation for the list of
  namespaces. Will load the JavaScript file into the REPL environment
  if any of the namespaces have not already been loaded from the
  ClojureScript REPL."
  [{:keys [project app] :as repl-env} ns-list url]
  (let [missing (remove #(contains? (app (project @loaded-libs)) %) ns-list)]
    (when (seq missing)
      (browserapp-eval (slurp url) repl-env)
      (swap! loaded-libs #(update-in % [project app]
                            (partial apply conj) missing)))))

(defn setup-routes [this]
  (server/dispatch-on :get
    (fn [{:keys [path]} _ _] (.startsWith path "/repl"))
    send-repl-client-page this)
  (server/dispatch-on :get
    (fn [{:keys [path]} _ _] (or (= path "/")
                               (.endsWith path ".js")
                               (.endsWith path ".cljs")
                               (.endsWith path ".map")
                               (.endsWith path ".html")))
    send-static this)
  (server/dispatch-on :post (constantly true) handle-post this)
  (server/dispatch-on :get
    (fn [{:keys [path]} _ _] (.startsWith path "/reflect"))
    (fn [{:keys [path] :as req} conn opts]
      (reflect/handle-reflect-query (reflect/parse-param path) req conn opts))
    this))

(defrecord BrowserAppEnv []
  repl/IJavaScriptEnv
  (-setup [this]
    (do (setup-routes this)
        (repl/analyze-source (:src this))
        (comp/with-core-cljs (server/start this))))
  (-evaluate [this _ _ js] (browserapp-eval js this))
  (-load [this ns url]
    (browser/load-javascript this ns url))
  (-tear-down [{:keys [project app] :as this}]
    (do (server/stop this)
        (swap! server/state #(update-in % [project] dissoc app))
        (swap! server/handlers #(update-in % [project] dissoc app))
        (swap! browser-state #(update-in % [project] dissoc app)))))

(defn compile-client-js [opts]
  (cljsc/build '[(ns clojure.browser.repl.client
                   (:require [goog.events :as event]
                             [clojure.browser.repl :as repl]))
                 (defn start [url]
                   (event/listen js/window
                                 "load"
                                 (fn []
                                   (repl/start-evaluator url))))]
               {:optimizations (:optimizations opts)
                :output-dir (:working-dir opts)}))

(defn create-client-js-file [opts file-path]
  (let [file (io/file file-path)]
    (when (not (.exists file))
      (spit file (compile-client-js opts)))
    file))

(defn- provides-and-requires
  "Return a flat list of all provided and required namespaces from a
  sequence of IJavaScripts."
  [deps]
  (flatten (mapcat (juxt :provides :requires) deps)))

(defn- always-preload
  "Return a list of all namespaces which are always loaded into the browser
  when using a browser-connected REPL."
  []
  (let [cljs (provides-and-requires (cljsc/cljs-dependencies {} ["clojure.browser.repl"]))
        goog (provides-and-requires (cljsc/js-dependencies {} cljs))]
    (disj (set (concat cljs goog)) nil)))

(defn repl-env
  "Create a browser-connected REPL environment.

  Options:

  port:           The port on which the REPL server will run. Defaults to 9000.
  working-dir:    The directory where the compiled REPL client JavaScript will
                  be stored. Defaults to \".repl\".
  serve-static:   Should the REPL server attempt to serve static content?
                  Defaults to true.
  static-dir:     List of directories to search for static content. Defaults to
                  [\".\" \"out/\"].
  preloaded-libs: List of namespaces that should not be sent from the REPL server
                  to the browser. This may be required if the browser is already
                  loading code and reloading it would cause a problem.
  optimizations:  The level of optimization to use when compiling the client
                  end of the REPL. Defaults to :simple.
  src:            The source directory containing user-defined cljs files. Used to
                  support reflection. Defaults to \"src/\".
  "
  [& {:keys [project app] :as opts}]
  (let [compiler-env (cljs.env/default-compiler-env opts)
        opts (merge (BrowserAppEnv.)
                    {:port          9000
                     :optimizations :simple
                     :working-dir   ".repl"
                     :serve-static  true
                     :static-dir    ["." "out/"]
                     :preloaded-libs   []
                     :src           "src/"
                     :cljs.env/compiler compiler-env
                     :source-map    true}
                    opts)]
    (cljs.env/with-compiler-env compiler-env
      (swap! preloaded-libs
        #(update-in % [project] assoc
           app (set (concat (always-preload)
                      (map str (:preloaded-libs opts))))))
      (swap! loaded-libs
        #(update-in % [project] assoc app (app (project @preloaded-libs))))
      (swap! browser-state
        #(update-in % [project app] assoc
           :client-js (future (create-client-js-file
                                opts
                                (io/file (:working-dir opts) "client.js")))))
      opts)))
