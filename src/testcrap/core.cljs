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

(defmethod read :question/edit
  [{:keys [query state]} _ {:keys [index]}]
  (let [st @state]
    {:value (om/db->tree query (nth (:questions st) index) st)}))

(defmethod read :answers
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
  [{:keys [state ref]} _ {:keys [text]}]
  {:action #(swap! state update-in ref assoc :text text)})

(defmethod mutate 'answer/hodor
  [{:keys [state ref]} _ _]
  {:action #(swap! state update-in ref assoc :text "Hodor")})


(defn hodor-button [parent]
  [:button
   {:on-click
    (fn [e]
      (om/transact!
       parent '[(answer/hodor)]))}
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
    (let [{:keys [text
                  answers]} (om/props this)]
      (html
       [:div
        [:p "Question: " text]
        [:ul (map answer answers)]]))))

(def question (om/factory Question {:keyfn :id}))


(defui QuestionEditor
  static om/Ident
  (ident [this {:keys [question/edit]}]
    [:question/by-id (:id edit)])

  static om/IQueryParams
  (params [this]
    {:index 0})

  static om/IQuery
  (query [this]
    [{'(:question/edit {:index ?index}) (om/get-query Question)}])

  Object
  (render [this]
    (let [{:keys [question/edit]} (om/props this)
          {:keys [text answers]} edit]
      (html
       [:div
        [:input
         {:type "text"
          :value (str text)
          :on-change
          (fn [e]
            (let [new-text (.. e -target -value)]
              (om/transact!
               this `[(question/edit {:text ~new-text})])))}]
        [:button
         {:on-click
          (fn [e]
            (om/update-query!
             this update-in [:params :index] inc))}
         "Next"]]))))

(def question-editor (om/factory QuestionEditor))


(defui Form
  static om/IQuery
  (query [this]
    (into
     [:id :name {:questions (om/get-query Question)}]
     (om/get-query QuestionEditor)))

  Object
  (render [this]
    (let [{:keys [name
                  questions
                  question/edit]} (om/props this)]
      (html
       [:div
        [:p "Form: " name]
        [:ul (map question questions)]
        (question-editor {:question/edit edit})]))))


(def reconciler
  (om/reconciler
   {:state init-state
    :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler Form (gdom/getElement "app"))


(defn on-js-reload [])
