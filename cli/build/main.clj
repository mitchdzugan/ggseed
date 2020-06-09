
(ns build.main
  (:require [shadow.cljs.devtools.api :as shadow]
            [clojure.java.shell :refer [sh]]))

(defn sh! [command]
  (println command)
  (println (:out (sh "bash" "-c" command))))

(defn build []
  (sh! "rm -rf dist/*")
  (shadow/release :browser)
  (shadow/release :server)
  (sh! (str "yarn run node-sass "
            "--output-style compressed "
            "scss/site.scss > dist/site.css"))
  (let [css (slurp "./dist/site.css")
        assets (read-string (slurp "./dist/assets.edn"))
        css-hash (hash css)
        css-name (str "site." css-hash ".css")]
    (spit (str "./dist/" css-name) css)
    (spit "./dist/assets.edn"
          (pr-str (update assets 0 #(merge %1 {:css-name css-name}))))))
