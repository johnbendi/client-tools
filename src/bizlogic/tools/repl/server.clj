(ns bizlogic.tools.repl.server
  (:refer-clojure :exclude [loaded-libs])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cljs.compiler :as comp]
            [cljs.closure :as cljsc]
            [cljs.repl :as repl])
  (:import java.io.BufferedReader
           java.io.BufferedWriter
           java.io.InputStreamReader
           java.io.OutputStreamWriter
           java.net.Socket
           java.net.ServerSocket
           cljs.repl.IJavaScriptEnv))

(def sample-browser-state {:socket nil
                           :connection nil
                           :promised-conn nil})

(defonce state (atom {}))

(defn connection
  "Promise to return a connection when one is available. If a connection
   is not a available, store the promise in server/state."
  [{:keys [project app] :as opts}]
  (println "Getting connection..")
  (println (format "project is %s app is %s" project app))
  (let [p (promise)
        conn (:connection (app (project @state)))]
    (if (and conn (not (.isClosed conn)))
      (do (deliver p conn)
          p)
      (do (swap! state #(update-in % [project app] assoc :promised-conn p))
          p))))

(defn set-connection [conn {:keys [project app] :as opts}]
  (if-let [promised-conn (:promised-conn (app (project @state)))]
    (do (swap! state
          #(update-in % [project app] dissoc :connection :promised-conn))
        (deliver promised-conn conn))
    (swap! state #(update-in % [project app] assoc :connection conn))))

(defonce handlers (atom {}))

(defn dispatch-on
  "Registers a handler to be dispatched based on a request method and a
  predicate.

  pred should be a function that accepts an options map, a connection,
  and a request map and returns a boolean value based on whether or not
  that request should be dispatched to the related handler."
  ([method pred handler opts]
     (dispatch-on method {:pred pred :handler handler} opts))
  ([method {:as m} {:keys [project app]}]
     (swap! handlers (fn [old]
                       (update-in old [project app method] #(conj (vec %) m))))))

;;; assumes first line already consumed
(defn parse-headers
  "Parse the headers of an HTTP POST request."
  [header-lines]
  (apply hash-map
   (mapcat
    (fn [line]
      (let [[k v] (str/split line #":" 2)]
        [(keyword (str/lower-case k)) (str/triml v)]))
    header-lines)))

(defn read-headers [rdr]
  (loop [next-line (.readLine rdr)
         header-lines []]
    (if (= "" next-line)
      header-lines                      ;we're done reading headers
      (recur (.readLine rdr) (conj header-lines next-line)))))

(defn read-post [line rdr]
  (let [[_ path _] (str/split line #" ")
        headers (parse-headers (read-headers rdr))
        content-length (Integer/parseInt (:content-length headers))
        content (char-array content-length)]
    (io! (.read rdr content 0 content-length)
         {:method :post
          :path path
          :headers headers
          :content (String. content)})))

(defn read-get [line rdr]
  (let [[_ path _] (str/split line #" ")
        headers (parse-headers (read-headers rdr))]
    {:method :get
     :path path
     :headers headers}))

(defn read-request [rdr]
  (let [line (.readLine rdr)]
    (cond (.startsWith line "POST") (read-post line rdr)
          (.startsWith line "GET") (read-get line rdr)
          :else {:method :unknown :content line})))

(defn- status-line [status]
  (case status
    200 "HTTP/1.1 200 OK"
    404 "HTTP/1.1 404 Not Found"
    "HTTP/1.1 500 Error"))

(defn send-and-close
  "Use the passed connection to send a form to the browser. Send a
  proper HTTP response."
  ([conn status form opts]
     (send-and-close conn status form "text/html" opts))
  ([conn status form content-type opts]
     (let [utf-8-form (.getBytes form "UTF-8")
           content-length (count utf-8-form)
           headers (map #(.getBytes (str % "\r\n"))
                        [(status-line status)
                         "Server: ClojureScript REPL"
                         (str "Content-Type: "
                              content-type
                              "; charset=utf-8")
                         (str "Content-Length: " content-length)
                         ""])]
       (with-open [os (.getOutputStream conn)]
         (do (doseq [header headers]
               (.write os header 0 (count header)))
             (.write os utf-8-form 0 content-length)
             (.flush os)
             (.close conn))))))

(defn send-404 [conn path]
  (send-and-close conn 404
                  (str "<html><body>"
                       "<h2>Page not found</h2>"
                       "No page " path " found on this server."
                       "</body></html>")
                  "text/html"))

(defn- dispatch-request [request conn {:keys [project app] :as opts}]
  (println (format "dispatching request for %s and %s..." project app))
  (if-let [handlers ((:method request) (app (project @handlers)))]
    (if-let [handler (some (fn [{:keys [pred handler]}]
                             (when (pred request conn opts)
                               handler))
                           handlers)]
      (if (= :post (:method request))
        (handler (read-string (:content request)) conn opts)
        (handler request conn opts))
      (send-404 conn (:path request)))
    (.close conn)))

(defn- handle-connection
  [opts conn]
  (let [rdr (BufferedReader. (InputStreamReader. (.getInputStream conn)))]
    (if-let [request (read-request rdr)]
      (dispatch-request request conn opts)
      (.close conn))))

(defn server-loop
  [opts server-socket]
  (let [conn (.accept server-socket)]
    (do (.setKeepAlive conn true)
        (future (handle-connection opts conn))
        (recur opts server-socket))))

(defn start
  "Start the server on the specified port."
  [{:keys [project app port] :as opts}]
  (let [ss (ServerSocket. port)]
    (future (server-loop opts ss))
    (swap! state (fn [old] (update-in old [project app] assoc
                             :socket ss :port port)))))

(defn stop
  [{:keys [project app]}]
  (.close (:socket (app (project @state)))))
