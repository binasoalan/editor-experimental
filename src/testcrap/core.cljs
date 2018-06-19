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
                                         :text "About right"}]}]
                 :question/edit {:id 1
                                 :text "How do you do?"
                                 :answers [{:id 1
                                            :text "OK"}
                                           {:id 2
                                            :text "Not bad"}
                                           {:id 3
                                            :text "Third"}]}})

(defn- position [item coll]
  (first
   (keep-indexed
    #(when (= item %2) (inc %1)) coll)))

(defn- next-question [current-id questions]
  (fnext
   (drop-while
    (fn [[_ id]]
      (not= id current-id)) questions)))


(defmulti read om/dispatch)

(defmethod read :questions
  [{:keys [query state]} key _]
  (let [st @state]
    {:value (om/db->tree query (get st key) st)}))

(defmethod read :question/edit
  [{:keys [query state]} _ _]
  (let [{:keys [questions question/edit] :as st} @state]
    {:value (-> (om/db->tree query edit st)
                (assoc
                 :position (position edit questions)
                 :total (count questions)))}))

(defmethod read :default
  [{:keys [state]} key _]
  (let [st @state]
    (if (contains? st key)
      {:value (get st key)}
      {:value :not-found})))


(defmulti mutate om/dispatch)

(defmethod mutate 'question/edit
  [{:keys [state ref]} _ {:keys [text]}]
  {:action #(swap! state
                   update-in ref
                   assoc :text text)})

(defmethod mutate 'question/next
  [{:keys [state ref]} _ _]
  (let [{:keys [questions]} @state
        [_ id] ref]
    {:action #(swap! state
                     assoc :question/edit
                     (if id
                       (next-question id questions)
                       (first questions)))}))

(defmethod mutate 'answer/edit
  [{:keys [state ref]} _ {:keys [text]}]
  {:action #(swap! state
                   update-in ref
                   assoc :text text)})

(defmethod mutate 'answer/hodor
  [{:keys [state ref]} _ _]
  {:action #(swap! state
                   update-in ref
                   assoc :text "Hodor")})


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


(defui AnswerEditor
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
       [:li
        [:input
         {:type "text"
          :value (str text)
          :on-change
          (fn [e]
            (let [new-text (.. e -target -value)]
              (om/transact!
               this `[(answer/edit {:text ~new-text})])))}]]))))

(def answer-editor (om/factory AnswerEditor {:keyfn :id}))


(defui QuestionEditor
  static om/Ident
  (ident [this {:keys [id]}]
    [:question/by-id id])

  static om/IQuery
  (query [this]
    [:id :position :total :text {:answers (om/get-query AnswerEditor)}])

  Object
  (render [this]
    (let [{:keys [position total text answers]} (om/props this)]
      (html
       [:div
        [:p "Viewing question " position " out of " total]
        [:input
         {:type "text"
          :value (str text)
          :on-change
          (fn [e]
            (let [new-text (.. e -target -value)]
              (om/transact!
               this `[(question/edit {:text ~new-text})])))}]
        [:ul (map answer-editor answers)]
        [:button
         {:on-click
          (fn [e]
            (om/transact! this `[(question/next)]))}
         "Next"]]))))

(def question-editor (om/factory QuestionEditor))


(defui Form
  static om/IQueryParams
  (params [this]
    {:index 0})

  static om/IQuery
  (query [this]
    [:id :name {:questions (om/get-query Question)}
     {:question/edit (om/get-query QuestionEditor)}])

  Object
  (render [this]
    (let [{:keys [name
                  questions
                  question/edit]} (om/props this)]
      (html
       [:div
        [:p "Form: " name]
        [:ul (map question questions)]
        (question-editor edit)]))))


(def reconciler
  (om/reconciler
   {:state init-state
    :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler Form (gdom/getElement "app"))


(defn on-js-reload [])
