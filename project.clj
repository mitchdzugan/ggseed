#_("
This file is only needed so that heroku will recognize this
as a clojure app and install java which is required for
shadow-cljs to compile the code during build steps
")

(defproject dummy-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies []
  :main ^:skip-aot dummy-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
