(ns ui.entry
  (:require [allpa.core :as a :refer [deftagged defprotomethod]]
            [mayu.macros :refer [defui ui]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.dom :as dom]
            [router :as r]
            ["graphql-client" :as graphql-client]
            ["csv-parse" :as csv-parse]))

(def apikey "41b4513569a9b82c39ed9ba60311162b")

(def tournament-query
  (str "query MyTournament($slug: String) {"
       "  currentUser {"
       "    id"
       "  }"
       "  tournament(slug: $slug) {"
       "    admins(roles: [\"admin\", \"manager\", \"creator\", \"reportingManager\", \"bracketManager\"]) {"
       "      id"
       "    }"
       "    id"
       "    name"
       "    slug"
       "    events {"
       "      id"
       "      name"
       "      slug"
       "      phases {"
       "        id"
       "        name"
       "      }"
       "    }"
       "  }"
       "}"))

(def update-mutation
  (str "mutation UpdatePhaseSeeding ($phaseId: ID!, $seedMapping: [UpdatePhaseSeedInfo]!) {"
       "  updatePhaseSeeding (phaseId: $phaseId, seedMapping: $seedMapping) {"
       "    id"
       "  }"
       "}"))

(deftagged SetStep [step])
(deftagged Assoc [assocs])

(def init-state {:completed 0
                 :step 1})

(defprotomethod reduce-state [state ^this action]
  SetStep
  (assoc state :step (min (-> state :completed inc) (:step action)))

  Assoc
  (->> action
       :assocs
       (partition-all 2)
       (reduce (fn [curr [path val]]
                 (assoc-in curr (if (vector? path) path [path]) val))
               state)))

(defui link-tournament [{:keys [input-slug input-key]} _]
  <[div {:class "field"} $=
    <[label {:class "label"} (str "API Key")]
    <[div {:class "control"} $=
      <[input {:class "input"
               :type "text"
               :placeholder "Key"
               :value input-key}
        ] d-key >
      (->> (dom/on-focus-in d-key)
           (e/map #(->Assoc [:done? false]))
           (dom/emit ::state))
      (->> (dom/on-focus-out d-key)
           (e/map #(->Assoc [:done? true]))
           (dom/emit ::state))
      (->> (dom/on-input d-key)
           (e/map #(.. % -target -value))
           (e/map #(->Assoc [:input-key %]))
           (dom/emit ::state))]
    <[p {:class "help"} $=
      <[dom/text "Copy and paste your smash.gg API key obtained from "]
      <[a {:target "_blank"
           :href "https://smash.gg/admin/profile/developer"}
        "here"]
      <[dom/text "."]]]
  <[div {:class "field"} $=
    <[label {:class "label"} "Tournament URL"]
    <[div {:class "control"} $=
      <[input {:class "input"
               :type "text"
               :placeholder "URL"
               :value input-slug}
        ] d-slug >
      (->> (dom/on-input d-slug)
           (e/map #(.. % -target -value))
           (e/map #(->Assoc [:input-slug %]))
           (dom/emit ::state))
      (->> (dom/on-focus-in d-slug)
           (e/map #(->Assoc [:done? false]))
           (dom/emit ::state))
      (->> (dom/on-focus-out d-slug)
           (e/map #(->Assoc [:done? true]))
           (dom/emit ::state))]
    <[p {:class "help"} $=
      <[dom/text "Paste a link to any of your tournament's pages here."]]])

(defui select-phase [{:keys [events event-ind phase-id]} _]
  <[div {:class "select-split"} $=
    <[div $=
      <[div "Select Event"]
      <[div {:style {:border "1px solid black"}} $=
        <[for (map vector (range) (keys events)) $[[ind id]]=
          let [{:keys [name] :as event} (-> events (get id))]
          <[keyed id
            <[div {:class {:item true
                           :selected (= ind (or event-ind 0))}}
              name] d-event >
            (->> (dom/on-click d-event)
                 (e/map #(->Assoc [:event-id (:id event)
                                   :event-ind ind
                                   :event-name name
                                   :phase-id nil
                                   :phase-name nil]))
                 (dom/emit ::state))]]]]
    <[div $=
      let [{:keys [phases]} (->> (or event-ind 0)
                                 (nth (keys events))
                                 (get events))]
      <[div "Select Phase"]
      <[div {:style {:border "1px solid black"}} $=
        <[for phases $[{:keys [id name]}]=
          <[keyed id
            <[div {:class {:item true
                           :selected (= id phase-id)}}
              name
              ] d-phase >
            (->> (dom/on-click d-phase)
                 (e/map #(->Assoc [:phase-id id
                                   :phase-name name]))
                 (dom/emit ::state))]]]]])

(defn index-of [needle haystack]
  (->> haystack
       (keep-indexed #(when (= %2 needle) %1))
       first))

(defn refresh-csv [e-state {:keys [sheet-id num-col id-col]} after keep-loading?]
  (->> (->Assoc [:error nil]) (e/push! e-state))
  (try
    (let [url (js/URL. sheet-id)
          pathname (.-pathname url)
          parts (->> (clojure.string/split pathname #"/")
                     (remove empty?))
          g-sheet-id (apply max-key count parts)]
      (->> (->Assoc [:loading? true :error nil])
           (e/push! e-state))
      (->  (js/fetch (js/Request. (str "/dl-sheet?sheet=" g-sheet-id)))
           (.then #(.text %1))
           (.then #(let [res (js/JSON.parse %1)
                         error (.-error res)
                         raw-csv (.-csv res)]
                     (->> (->Assoc [:loading? false])
                          (e/push! e-state))
                     (if error
                       (->> (->Assoc [:loading? false
                                      :error error
                                      :csv nil
                                      :num-col nil
                                      :id-col nil])
                            (e/push! e-state))
                       (csv-parse raw-csv
                                  (fn [err csv]
                                    (if err
                                      (->> (->Assoc [:loading? false
                                                     :error err
                                                     :csv nil
                                                     :num-col nil
                                                     :id-col nil])
                                           (e/push! e-state))
                                      (->> (->Assoc [:loading? keep-loading?
                                                     :csv csv
                                                     :num-col (or num-col (index-of "Phase Seed"
                                                                                    (nth csv 0 [])))
                                                     :id-col (or id-col (index-of "Seed ID"
                                                                                  (nth csv 0 [])))])
                                           (e/push! e-state)
                                           (after csv))))))))
           (.catch #(->> (->Assoc [:loading? false
                                   :csv nil
                                   :num-col nil
                                   :id-col nil
                                   :error (str "Sheet is not public")])
                         (e/push! e-state)))))
    (catch js/Error e
      (when-not (empty? sheet-id)
        (e/push! e-state (->Assoc [:loading? false :error "Invalid URL"])))
      nil)))

(defui upload-seeding [{:keys [modal? sheet-id csv num-col id-col input-key phase-id] :as state} e-state]
  <[div {:class "seeding-wrap"} $=
    <[div {:style {:margin "0"}
           :class "content"} $=
      <[h3 "Update Seeding Process"]]
    <[p {:style {:margin-bottom "10px"}} $=
      <[dom/text (str "The desired seeds need to be uploaded to Google Sheets and made public. The"
                      " simplest way to do this is to download the phase export for the phase you"
                      " wish to update (")]
      <[a {:href (str "https://smash.gg/api-proxy/phase/"
                      phase-id
                      "/export_results")}
        "Download link"]
      <[dom/text "), update the "]
      <[strong "Phase Seed"]
      <[dom/text (str " column to reflect your desired seeds and then upload to google sheets. Once"
                      " you have done this, you need to use the \"Share\" button at the top right of"
                      " Google Sheets and ensure the document is visible to anyone with a link. Once"
                      " public, copy and paste the link below.")]]
    <[div {:class "field"} $=
      <[label {:class "label"} (str "Google Sheets Link")]
      <[div {:class "control"} $=
        <[input {:class "input"
                 :type "text"
                 :placeholder "Key"
                 :value sheet-id}
          ] d-sheet-id >
        (->> (dom/on-input d-sheet-id)
             (e/map #(.. % -target -value))
             (e/map #(->Assoc [:sheet-id %]))
             (dom/emit ::state))]]
    let [cols (nth csv 0 [])
         no-csv? (empty? csv)]
    <[div {:class {:select-split true :disabled no-csv?}} $=
      <[div $=
        <[div $=
          <[dom/text "Select "]
          <[strong "Phase Seed"]
          <[dom/text " Column"]]
        <[div {:style {:border "1px solid black"}} $=
          <[when no-csv? <[div {:class "item"} "No Sheet Linked"]]
          <[for (map vector (range) cols) $[[ind name]]=
            <[keyed [:a ind name]
              <[div {:class {:item true
                             :selected (= ind num-col)}}
                name
                ] d-phase-seed >
              (->> (dom/on-click d-phase-seed)
                   (e/map #(->Assoc [:num-col ind]))
                   (dom/emit ::state))]]]]
      <[div $=
        <[div $=
          <[dom/text "Select "]
          <[strong "Seed ID"]
          <[dom/text " Column"]]
        <[div {:style {:border "1px solid black"}} $=
          <[when no-csv? <[div {:class "item"} "No Sheet Linked"]]
          <[for (map vector (range) cols) $[[ind name]]=
            <[keyed [:b ind name]
              <[div {:class {:item true
                             :selected (= ind id-col)}}
                name
                ] d-id-seed >
              (->> (dom/on-click d-id-seed)
                   (e/map #(->Assoc [:id-col ind]))
                   (dom/emit ::state))]]]]]
    <[button {:disabled (or (empty? csv) (nil? id-col) (nil? num-col))
              :class "button is-info"}
      "Update smash.gg seeding"] d-update >
    (-> (dom/on-click d-update)
        (dom/consume! #(e/push! e-state (->Assoc [:modal? true]))))
    <[when modal?
      <[div {:style {:position "fixed"
                     :z-index "100"
                     :left 0
                     :height "100%"
                     :width "100%"
                     :transition "transform 0.5s ease-in-out"
                     :transform "translateY(-100%)"
                     :delayed {:transform "translateY(0)"}
                     :remove {:transform "translateY(-100%)"}}} $=
        <[div {:style {:margin "20px"}
               :class "notification is-light is-warning"} $=
          <[dom/text "Are you sure you want to update seeding? This action cannot be undone!"]
          <[div {:class "buttons"
                 :style {:margin-top "20px"
                         :display "flex"
                         :flex-direction "row"}} $=
            <[button {:class "button is-small is-info"} "Update"] d-finalize >
            <[button {:class "button is-small"} "Cancel"] d-cancel >
            (-> (dom/on-click d-cancel)
                (dom/consume! #(e/push! e-state (->Assoc [:modal? false]))))
            (-> (dom/on-click d-finalize)
                (dom/consume! (fn []
                                (e/push! e-state (->Assoc [:loading? true :error nil :modal? false]))
                                (refresh-csv e-state state
                                             (fn [csv]
                                               (let [seed-mapping (->> csv
                                                                       (drop 1)
                                                                       (map #(-> {:seedId (js/parseInt (nth % id-col))
                                                                                  :seedNum (js/parseInt (nth % num-col))}))
                                                                       (sort-by :seedNum)
                                                                       clj->js)
                                                     client
                                                     (graphql-client #js {:url "https://api.smash.gg/gql/alpha"
                                                                          :headers #js {:Authorization
                                                                                        (str "Bearer "
                                                                                             input-key)}})
                                                     ]
                                                 (-> client
                                                     (.query update-mutation #js {:seedMapping seed-mapping
                                                                                  :phaseId phase-id})
                                                     (.then (fn [res]
                                                              (let [errors (or (aget res "errors") #js[])
                                                                    e1 (or (aget errors 0) #js{})
                                                                    message (or (aget e1 "message") nil)]
                                                                (if message
                                                                  (->> (->Assoc [:loading? false
                                                                                 :error message])
                                                                       (e/push! e-state))
                                                                  (->> (->Assoc [:loading? false
                                                                                 :completed 3
                                                                                 :step 4])
                                                                       (e/push! e-state)))))))))
                                             true))))]]]]])

(defui finish [{:keys [event-id slug phase-id]} _]
  let [url (str "https://smash.gg/admin/"
                slug
                "/seeding/"
                event-id
                "/"
                phase-id)]
  <[div {:class "has-text-centered"} $=
    <[div {:class "is-size-3 has-text-weight-bold"}
      "Seeding update complete!"]
    <[div $=
      <[dom/text "Go "]
      <[a {:target "_blank"
           :href url} "here"]
      <[dom/text " to view the updated seeding"]]])

(defn on-link [e-state {:keys [input-key input-slug done? completed]}]
  (try
    (let [url (js/URL. input-slug)
          pathname (.-pathname url)
          parts (->> (clojure.string/split pathname #"/")
                     (remove empty?))
          slug-ind (if (= "admin" (nth parts 0 ""))
                     2
                     1)
          slug (nth parts slug-ind "")]
      (e/push! e-state (->Assoc [:loading? true :error nil]))
      (let [client
            (graphql-client #js {:url "https://api.smash.gg/gql/alpha"
                                 :headers #js {:Authorization (str "Bearer "
                                                                   input-key)}})]
        (-> client
            (.query tournament-query #js {:slug slug})
            (.then (fn [res]
                     (let [data (or (aget res "data") #js {})
                           user (or (aget data "currentUser") #js {})
                           user-id (aget user "id")
                           tournament (aget data "tournament")
                           admins (or (aget tournament "admins") #js [])
                           admin? (-> (->> admins
                                           js->clj
                                           (map #(get % "id"))
                                           (a/index-by #(-> %))
                                           (a/map-values #(-> true)))
                                      (get user-id))
                           tournament (js->clj tournament :keywordize-keys true)
                           errors (aget res "errors")
                           message (aget res "message")
                           events (->> tournament
                                       :events
                                       (a/index-by :id))]
                       (->>
                        (cond
                          (not admin?) [:loading? false
                                        :error "User does not have seeding permissions"]
                          (not (nil? errors)) [:loading? false
                                               :error (aget errors 0 "message")]
                          (not (nil? message)) [:loading? false
                                                :error message]
                          (nil? tournament) [:loading? false
                                             :error (str "Tournament "
                                                         input-slug
                                                         " does not exist")]
                          :else [:done? false
                                 :step (if done? 2 1)
                                 :completed 1
                                 :slug (:slug tournament)
                                 :tournament (:name tournament)
                                 :events events
                                 :loading? false
                                 :csv nil
                                 :event-ind 0
                                 :event-name (-> events
                                                 (get (first (keys events)))
                                                 :name)
                                 :phase-id nil
                                 :phase-name nil
                                 :sheet-id nil
                                 :num-col nil
                                 :id-col nil])
                        ->Assoc
                        (e/push! e-state))))))))
    (catch js/Error e
      (e/push! e-state (->Assoc [:loading? false :error "Invalid URL"]))
      nil)))

(defn on-phase [e-state {:keys [event-ind phase-id events completed]}]
  (let [phase-name (-> events
                       (get (nth (keys events) (or event-ind -1) nil))
                       :phases
                       (#(a/index-by :id %))
                       (get phase-id)
                       :name)]
    (when phase-name
      (js/setTimeout
       (fn []
         (->> (->Assoc [:loading? true])
              (e/push! e-state))
         (js/setTimeout #(->> (->Assoc [:loading? false
                                        :error nil
                                        :step 3
                                        :completed 2
                                        :csv nil
                                        :sheet-id nil
                                        :num-col nil
                                        :id-col nil])
                              (e/push! e-state))
                        500))
       0))))

(defn on-upload [e-state state] (refresh-csv e-state state (fn []) false))

(defn on-finish [e-state {:keys [input-key input-slug]}])

(defui home []
  <[dom/collect-and-reduce ::state reduce-state init-state $=
    let [e-state (e/on! (e/Event))]
    (dom/emit ::state e-state)
    <[div {:style {:height "100%"
                   :display "flex"
                   :flex-direction "column"
                   :align-items "stretch"}} $=
      s-state <- (dom/envs ::state)
      s-input <- (s/map #(select-keys % [:input-key
                                         :input-slug
                                         :done?
                                         :event-ind
                                         :phase-id
                                         :sheet-id])
                        s-state)
      (-> (s/changed s-input)
          (dom/consume! #(let [state (s/inst! s-state)]
                           (case (:step state)
                             1 (on-link e-state state)
                             2 (on-phase e-state state)
                             3 (on-upload e-state state)
                             4 (on-finish e-state state)
                             nil))))
      <[dom/bind s-state $[{:keys [completed
                                   step
                                   loading?
                                   error
                                   tournament
                                   event-name
                                   phase-name]
                            :as state}]=
        <[div {:style {:margin "15px 0 0 0"
                       :display "flex"
                       :flex-direction "row"
                       :justify-content "space-evenly"
                       }} $=
          <[div {:style {:flex "1"}
                 :class "steps"} $=
            <[for (map vector (range) ["Link Tournament"
                                       "Select Phase"
                                       "Upload Seeding"
                                       "Finish"]) $[[i title]]=
              <[keyed i
                let [completed? (< i completed)
                     completable? (<= i completed)
                     active? (= step (inc i))]
                <[div {:class {:step-item true
                               :is-completable completable?
                               :is-active active?
                               :is-success (and (not active?) completed?)
                               :is-completed (and (not active?) completable?)}} $=
                  <[div {:class "step-marker"} (inc i)]
                  <[div {:class "step-details"} $=
                    <[p {:class "step-title"} title]]
                  ] d-step-item >
                (->> (dom/on-click d-step-item)
                     (e/filter #(-> completable?))
                     (e/map #(->SetStep (inc i)))
                     (dom/emit ::state))]]]
          let [completed? (<= step completed)]
          <[button {:class "button is-link"
                    :disabled (not completed?)}
            "Next"] d-next >
          (->> (dom/on-click d-next)
               (e/map #(->SetStep (inc step)))
               (dom/emit ::state))
          <[div {:style {:width "20px" :height "1px"}}]
          ]]
      <[dom/bind s-state $[{:keys [step completed] :as state}]=
        <[div {:class "boxer"
               :style {:flex "1"
                       :position "relative"
                       :overflow "hidden"}} $=
          <[div {:style {:position "absolute"
                         :height "100%"
                         :width "100%"
                         :left "0"
                         :top "0"
                         :transition "transform 0.5s ease-in-out"
                         :transform (str "translateY("
                                         (* -100 (dec step))
                                         "%)")}} $=
            <[when true
              <[div {:class "scroller"
                     :style {:position "absolute"
                             :height "100%"
                             :width "100%"
                             :padding "25px"
                             :overflow "scroll"
                             :left "0"
                             :top "0"}} $=
                <[link-tournament state e-state]]]
            <[when (> completed 0)
              <[div {:class "scroller"
                     :style {:position "absolute"
                             :height "100%"
                             :width "100%"
                             :padding "25px"
                             :overflow "scroll"
                             :left "0"
                             :top "100%"}} $=
                <[select-phase state e-state]]]
            <[when (> completed 1)
              <[div {:class "scroller"
                     :style {:position "absolute"
                             :height "100%"
                             :width "100%"
                             :padding "25px"
                             :overflow "scroll"
                             :left "0"
                             :top "200%"}} $=
                <[upload-seeding state e-state]]]
            <[when (> completed 2)
              <[div {:class "scroller"
                     :style {:position "absolute"
                             :height "100%"
                             :width "100%"
                             :padding "25px"
                             :overflow "scroll"
                             :left "0"
                             :top "300%"}} $=
                <[finish state e-state]]]]]]



      <[dom/bind s-state $[{:keys [completed
                                   step
                                   loading?
                                   error
                                   tournament
                                   event-name
                                   phase-name]
                            :as state}]=
        <[div {:class "columns none is-flex-tablet" :style {:margin "0"}} $=
          <[div {:class "column"} $=
            <[div {:class "content"
                   :style {:margin "0"
                           :height "40px"
                           :display "flex"
                           :flex-direction "row"
                           :align-self "center"
                           :justify-content "center"}} $=
              <[span {:style {:visibility (if (and (not loading?) error)
                                            "visible" "hidden")}
                      :class "has-text-danger"}
                error]
              <[span {:style {:visibility (if loading? "visible" "hidden")}
                      :class "icon"} $=
                <[i {:class "fas fa-spinner fa-spin fa-2x fa-fw"}]]
              <[span {:style {:visibility (if (and (>= completed step)
                                                   (not loading?)
                                                   (not error))
                                            "visible" "hidden")}
                      :class "icon has-text-success"} $=
                <[i {:class "fas fa-check fa-2x fa-fw"}]]]]
          <[div {:class "column"} $=
            <[strong "Tournament: "]
            <[dom/text (or tournament "N/A")]]
          <[div {:class "column"} $=
            <[strong "Event: "]
            <[dom/text (or event-name "N/A")]]
          <[div {:class "column"} $=
            <[strong "Phase: "]
            <[dom/text (or phase-name "N/A")]]]
        <[div {:class "columns none is-flex-mobile" :style {:margin "0"}} $=
          <[div {:class "column"} $=
            <[div {:class "content"
                   :style {:margin "0"
                           :height "40px"
                           :display "flex"
                           :flex-direction "row"
                           :align-self "center"
                           :justify-content "center"}} $=
              <[span {:style {:visibility (if (and (not loading?) error)
                                            "visible" "hidden")}
                      :class "has-text-danger"}
                error]
              <[span {:style {:visibility (if loading? "visible" "hidden")}
                      :class "icon"} $=
                <[i {:class "fas fa-spinner fa-spin fa-2x fa-fw"}]]
              <[span {:style {:visibility (if (and (>= completed step)
                                                   (not loading?)
                                                   (not error))
                                            "visible" "hidden")}
                      :class "icon has-text-success"} $=
                <[i {:class "fas fa-check fa-2x fa-fw"}]]]]]]

      ]])

(defui root []
  s-route <- (dom/envs ::r/s-route)
  <[dom/bind s-route $[route]=
    <[dom/assoc-env ::r/route route $=
      let [name (get-in route [:data :name])]
      <[section {:class "section"
                 :style {:padding-top "0"
                         :padding-bottom "0"
                         :height "100vh"
                         :position "relative"}} $=
        <[div {:class "container"
               :style {:height "100%"}} $=
          <[case name
            <[::r/home <[home]]]]]]])
