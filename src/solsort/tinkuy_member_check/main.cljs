(ns solsort.tinkuy_member_check.main
  (:require-macros
   [reagent.ratom :as ratom :refer  [reaction]]
   [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require
   [solsort.misc :refer [<blob-url]]
   [solsort.util :as u
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

(defn select
  "This is a select widget"
  [id options]
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

(defn handle-file [id file]
  (go
    (let [raw (<! (<blob-text file))
         tinkuy (u/parse-json-or-nil raw)
          stripe (if (string/starts-with? raw "id,Description,Created (UTC),Amount,Amount Refunded,Currency,Converted Amount,Converted Amount Refunded,Fee,Tax,Converted Currency,Mode,Status,Statement Descriptor,Customer ID,Customer Description,Customer Email,Captured,Card ID,Card Last4,Card Brand,Card Funding,Card Exp Month,Card Exp Year,Card Name,Card Address Line1,Card Address Line2,Card Address City,Card Address State,Card Address Country,Card Address Zip,Card Issue Country,Card Fingerprint,Card CVC Status,Card AVS Zip Status,Card AVS Line1 Status,Card Tokenization Method,Disputed Amount,Dispute Status,Dispute Reason,Dispute Date (UTC),Dispute Evidence Due (UTC),Invoice ID,Payment Source Type,Destination,Transfer,Interchange Costs,Merchant Service Charge")
                   (map
                    (fn [line]
                      (map #(.slice (first %) 1)
                             (re-seq #"(,[^\",][^,]*|,\"[^\"]*\"|,)" line)))
                    (split raw #"\n")))
          stripe (and stripe
                      (map
                       #(into {} (map vector (first stripe)%))
                       (rest stripe)))
          merkur (map #(split % #";") (split raw #"\n"))
          merkur (and (= 27 (count (first merkur))) merkur)]
      (log 'stripe stripe)
      (cond
    tinkuy (db! :users tinkuy)
    stripe (db! :stripe stripe)
    merkur (db! :merkur merkur)
    :else (js/alert "Input file not in expected format")))))


(log @(db :stripe))

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
    "Connect to tinkuy.dk, and upload bank and stripe report, to get a status for paying members. " [:br]
    "Upload tinkuy member export, bank statement, and stripe statement to compare."
   [:p
    [file-input [:text :users] :title "Upload"]]
   [:hr]
   [:p
    {:style {:line-wrap :none}}
    "Tinkuy: " (str (count @(db :users))) " entries" [:br]
    "Stripe: " (str (count @(db :stripe))) " entries"  [:br]
    "Merkur: " (str (count @(db :merkur))) " entries"  [:br]

    ]
   ])

(render [main])

(identity js/window.innerHeight)
