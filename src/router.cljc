(ns router
  (:require [allpa.core :as a]
            #?(:clj [reitit.core :as r]
               :cljs [reitit.frontend :as r])))

(def routes
  [["/" {:name ::home}]
   ["/page/:id" {:name ::page}]])

(def router (r/router routes))

(defn url [& args]
  (-> (apply r/match-by-name router args)
      :path))

(defn match-by-path [path]
  (r/match-by-path router path))
