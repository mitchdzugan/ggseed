(ns frontends.browser
  (:require [allpa.core :as a]
            [accountant.core :as accountant]
            [mayu.attach :as attach]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [router :as r]
            [ui.entry]))

(defonce a-s-route (atom {}))
(defonce a-off (atom (fn [])))

(defn mount-root []
  (let [{:keys [off]}
        (attach/attach (js/document.getElementById "app")
                       {::r/s-route @a-s-route}
                       ui.entry/root)]
    (reset! a-off off)))

(defn main! []
  (println "Client init")
  (let [e-route (e/on! (e/Event))
        {s-route :signal} (s/build (s/from nil e-route))]
    (reset! a-s-route s-route)
    (accountant/configure-navigation!
     {:nav-handler #(e/push! e-route (r/match-by-path %1))
      :path-exists? #(boolean (r/match-by-path %1))})
    (accountant/dispatch-current!)
    (mount-root)))

(defn reload! []
  (println "reloading")
  (@a-off)
  (mount-root))
