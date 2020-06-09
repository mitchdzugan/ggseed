(ns server
  (:require ["express" :as express]
            ["fs" :as fs]
            [allpa.core :as a]
            [cljs.reader :as reader]
            [router :as r]
            [ui.entry]
            [mayu.dom :as dom]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.async :refer [go-loop <!]]))

(def dev?
  (let [debug? (do ^boolean js/goog.DEBUG)]
    (if debug?
      (cond
        (exists? js/window) true
        (exists? js/process) (not= "true" js/process.env.release)
        :else true)
      false)))

(def assets
  (if dev?
    {:css-name "site.css"
     :output-name "client.js"}
    (-> "./dist/assets.edn"
        (fs/readFileSync "utf8")
        reader/read-string
        first)))

(def css-name (:css-name assets))

(def output-name (:output-name assets))

(def page-pre (str"
<!DOCTYPE html>
<html>
   <head>
      <meta charset=\"utf-8\">
      <meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">
      <link href=\"/" css-name "\" rel=\"stylesheet\" type=\"text/css\">
      <title>ggseed</title>
   </head>
   <body>
      <div id=\"app\">
"))

(def page-post (str"
      </div>
      <script src=\"/" output-name "\" type=\"text/javascript\"></script>
   </body>
</html>
"))

(defn stream-route [res route]
  (.type res "html")
  (.write res page-pre)
  (let [{:keys [signal off]}
        (s/build (s/from route e/never))
        markup-channel (dom/render-to-string {::r/s-route signal} ui.entry/root)]
    (go-loop []
      (let [markup (<! markup-channel)]
        (if (nil? markup)
          (do (.write res page-post)
              (.end res))
          (do (.write res markup)
              (recur)))))))

(def app (express))

(defn main! []
  (.get app #".*"
        (fn [req res next]
          (let [url (.-url req)
                route (r/match-by-path url)]
            (if route
              (stream-route res route)
              (next)))))
  (.use app (.static express (if dev? "target" "dist")))
  (let [port (if (some? js/process.env.PORT)
               (js/parseInt js/process.env.PORT)
               3000)]
    (.listen app port #(println (str "App listening on port " port)))))

(defn reload! []
  (println "Code updated."))
