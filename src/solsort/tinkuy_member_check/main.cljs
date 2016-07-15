(ns solsort.tinkuy_member_check.main
  (:require-macros
   [reagent.ratom :as ratom :refer  [reaction]]
   [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require
   [solsort.misc :refer [<blob-url]]
   [solsort.util
    :refer
    [<blob-text
     <p <ajax <seq<! js-seq normalize-css load-style! put!close!
     parse-json-or-nil log page-ready render dom->clj next-tick]]
   [reagent.core :as reagent :refer []]
   [cljs.reader :refer [read-string]]
   [re-frame.core :as re-frame
    :refer [register-sub subscribe register-handler
            dispatch dispatch-sync]]
   [clojure.string :as string :refer [replace split blank?]]
   [cljs.core.async :as async :refer [>! <! chan put! take! timeout close! pipe]]))

(declare db)
(defn db-raw [& path]
  (if path
    (reaction (get @(apply db (butlast path)) (last path)))
    (subscribe [:db])))
(def db
  "memoised function, that returns a subscription to a given path into the application db"
  (memoize db-raw))
(defn db! "Write a value into the application db" [& path]
  (dispatch (into [:db] path)))

(register-sub :db
              (fn  [db [_ & path]]
                (reaction
                 (if path
                   (get-in @db path)
                   @db))))
(register-handler :db
                  (fn  [db [_ & path]] (let [value (last path) path (butlast path)] (if path (assoc-in db path value) value))))

(defn checkbox [id]
  (let [value @(apply db id)]
    [:img.checkbox
     {:on-click #(apply db! (concat id [(not value)]))
      :src (if value "assets/check.png" "assets/uncheck.png")}]))

(defn select [id options]
  (let [current @(subscribe [:ui id])]
    (into [:select
           {:value (prn-str current)
            :onChange
            #(dispatch [:ui id (read-string (.-value (.-target %1)))])}]
          (for [[k v] options]
            (let [v (prn-str v)]
              [:option {:key v :value v} k])))))

(defn input  [id & {:keys [type size max-length options]
                    :or {type "text"}}]
  (case type
    :select (select id options)
    :checkbox (checkbox id)
    [:input {:type type
             :style {:padding-right 0
                     :padding-left 0
                     :text-align :center
                     :overflow :visible
                     }
             :name (prn-str id)
             :key (prn-str id)
             :size size
             :max-length max-length
             :value @(apply db id)
             :on-change #(apply db! (concat id [(.-value (.-target %1))]))}]))

(go (db! :users (<! (<ajax "http://tinkuy.dk/api/users"))))
(log @(db :users))
(defn handle-file [id file]
  (go
   (log 'handle-file id (<! (<blob-text file)))))

(defn file-input  [id & {:keys [title]
                    :or {title "upload"}}]
  [:div.ui.input
   [:label.ui.button.blue {:for (prn-str id)}
    title]
   [:input {:type :file
            :style {:display :none
                    }
            :name (prn-str id)
            :key (prn-str id)
            :id (prn-str id)
            :value @(apply db id)
            :on-change #(handle-file id (aget (.-files (.-target %1)) 0))
            }]])

(defn main []
  [:div.ui.container
   [:h1 "Tinkuy Member Check"]
   [:div
    "Connect to tinkuy.dk, and upload bank and stripe report, to get a status for paying members"]
   [file-input [:text :users] :title "Upload tinkuy"]
   [file-input [:text :stripe] :title "Upload stripe"]
   [file-input [:text :merkur] :title "Upload merkur"]

   ])

(render [main])
