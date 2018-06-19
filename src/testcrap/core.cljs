(ns testcrap.core
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
  [{:keys [state] :as env} key _]
  (let [st @state]
    (if (contains? st key)
      {:value (get st key)}
      {:value :not-found})))


(defmulti mutate om/dispatch)

(defmethod mutate 'question/edit
  [{:keys [state]} _ {:keys [text index]}]
  (let [ref (nth (:questions @state) index)]
    {:action #(swap! state update-in ref assoc :text text)}))

(defmethod mutate 'answer/hodor
  [{:keys [state ref]} _ _]
  {:action #(swap! state update-in ref assoc :text "Hodor")})


(defn hodor-button [parent]
  [:button
   {:on-click
    (fn [e]
      (om/transact! parent '[(answer/hodor)]))}
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


(defn question-text-input [form index question]
  [:input
   {:type "text"
    :value (str (:text question))
    :on-change
    (fn [e]
      (let [new-text (.. e -target -value)]
        (om/transact!
         form `[(question/edit {:text ~new-text
                                :index ~index})])))}])

(defn next-button [form index]
  [:button
   {:on-click
    (fn [e]
      (om/set-query!
       form {:params {:index (inc index)}}))}
   "Next"])


(defui Editor
  static om/IQueryParams
  (params [this]
    {:index 0})

  Object
  (render [this]
    (let [questions (om/props this)
          {:keys [index]}  (om/get-params this)
          focused-question (nth questions index)]
      (html
       [:div
        (question-text-input this index focused-question)
        (next-button this index)]))))

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
        [:ul (map question questions)]
        (editor questions)]))))


(def reconciler
  (om/reconciler
   {:state init-state
    :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler Form (gdom/getElement "app"))


(defn on-js-reload [])
