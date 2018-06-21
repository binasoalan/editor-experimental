(ns editor-experimental.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [sablono.core :refer-macros [html]]))

(enable-console-print!)

(def init-state {:id 1
                 :name "First form"
                 :questions [{:id 1
                              :text "How do you do?"
                              :answers [{:id 1
                                         :text "OK"}
                                        {:id 2
                                         :text "Not bad"}
                                        {:id 3
                                         :text "Third"}]}
                             {:id 2
                              :text "What ARE you doing?"
                              :answers [{:id 4
                                         :text "Aye"}
                                        {:id 5
                                         :text "Yarr"}]}
                             {:id 3
                              :text "Savvy?"
                              :answers [{:id 6
                                         :text "Hmmm"}
                                        {:id 7
                                         :text "About right"}]}]})


(defmulti read om/dispatch)

(defmethod read :questions
  [{:keys [query state]} key _]
  (let [st @state]
    {:value (om/db->tree query (get st key) st)}))

(defmethod read :default
  [{:keys [state]} key _]
  (let [st @state]
    (if (contains? st key)
      {:value (get st key)}
      {:value :not-found})))


(defmulti mutate om/dispatch)

(defmethod mutate 'question/edit
  [{:keys [state]} _ {:keys [id] :as new-props}]
  {:action #(swap! state
                   update-in [:question/by-id id]
                   merge new-props)})

(defmethod mutate 'answer/edit
  [{:keys [state]} _ {:keys [id] :as new-props}]
  {:action #(swap! state
                   update-in [:answer/by-id id]
                   merge new-props)})

(defmethod mutate 'answer/hodor
  [{:keys [state ref]} _ _]
  {:action #(swap! state
                   update-in ref
                   assoc :text "Hodor")})


(defn hodor-button [parent]
  [:button
   {:on-click
    (fn [e]
      (om/transact! parent '[(answer/hodor) :questions]))}
   "Hodor"])

(defui Answer
  static om/Ident
  (ident [this {:keys [id]}]
    [:answer/by-id id])

  static om/IQuery
  (query [this]
    [:id :text])

  Object
  (render [this]
    (let [{:keys [text]} (om/props this)]
      (html
       [:li text " " (hodor-button this)]))))

(def answer (om/factory Answer {:keyfn :id}))


(defui Question
  static om/Ident
  (ident [this {:keys [id]}]
    [:question/by-id id])

  static om/IQuery
  (query [this]
    [:id :text {:answers (om/get-query Answer)}])

  Object
  (render [this]
    (let [{:keys [text answers]} (om/props this)]
      (html
       [:div
        [:p "Question: " text]
        [:ul (map answer answers)]]))))

(def question (om/factory Question {:keyfn :id}))


;; Editor

(defn answer-editor [c {:keys [id text] :as props}]
  [:li
   [:input
    {:type      "text"
     :value     (str text)
     :on-change (fn [e]
                  (let [text (.. e -target -value)]
                    (om/transact!
                     c `[(answer/edit {:id ~id :text ~text})])))}]])

(defn question-editor [c {:keys [id text answers] :as props}]
  [:div
   [:input
    {:type      "text"
     :value     (str text)
     :on-change (fn [e]
                  (let [text (.. e -target -value)]
                    (om/transact!
                     c `[(question/edit {:id ~id :text ~text})])))}]
   [:ul (map (partial answer-editor c) answers)]])


(defui Editor
  Object
  (render [this]
    (let [questions (om/props this)
          index     (om/get-state this :index)
          question* (nth questions (or index 0))]
      (html
       [:div
        [:p "Viewing question " (inc index) " out of " (count questions)]
        [:p
         [:button
          {:on-click (fn [e]
                       (om/update-state! this update :index dec))}
          "Prev"]
         [:button
          {:on-click (fn [e]
                       (om/update-state! this update :index inc))}
          "Next"]]
        (question-editor this question*)
        [:p "Preview:"]
        (question question*)]))))

(def editor (om/factory Editor))


(defui Form
  static om/IQuery
  (query [this]
    [:id :name {:questions (om/get-query Question)}])

  Object
  (render [this]
    (let [{:keys [name questions]} (om/props this)]
      (html
       [:div
        [:p "Form: " name]
        (editor questions)]))))


(def reconciler
  (om/reconciler
   {:state init-state
    :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler Form (gdom/getElement "app"))


(defn on-js-reload [])
