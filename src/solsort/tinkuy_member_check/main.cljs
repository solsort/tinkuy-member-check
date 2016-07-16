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
  (dispatch-sync (into [:db] path)))

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
                     :overflow :visible}
             :name (prn-str id)
             :key (prn-str id)
             :size size
             :max-length max-length
             :value @(apply db id)
             :on-change #(apply db! (concat id [(.-value (.-target %1))]))}]))

(defn fix-date [s]
  (str
   (.slice s 6 10)
   "-" (.slice s 3 5)
   "-" (.slice s 0 2)
       ))
(defn str-normalise [s]
  (.replace
   (.replace
    (.toLowerCase s)
    (js/RegExp "[^ a-z]" "g")
    "")
   (js/RegExp "  *" "g")
   " "))
(defn handle-stripe [raw]
  (let [stripe (if (string/starts-with? raw "id,Description,Created (UTC),Amount,Amount Refunded,Currency,Converted Amount,Converted Amount Refunded,Fee,Tax,Converted Currency,Mode,Status,Statement Descriptor,Customer ID,Customer Description,Customer Email,Captured,Card ID,Card Last4,Card Brand,Card Funding,Card Exp Month,Card Exp Year,Card Name,Card Address Line1,Card Address Line2,Card Address City,Card Address State,Card Address Country,Card Address Zip,Card Issue Country,Card Fingerprint,Card CVC Status,Card AVS Zip Status,Card AVS Line1 Status,Card Tokenization Method,Disputed Amount,Dispute Status,Dispute Reason,Dispute Date (UTC),Dispute Evidence Due (UTC),Invoice ID,Payment Source Type,Destination,Transfer,Interchange Costs,Merchant Service Charge")
                 (map
                  (fn [line]
                    (map #(.slice (first %) 1)
                         (re-seq #"(,[^\",][^,]*|,\"[^\"]*\"|,)" line)))
                  (split raw #"\n")))
        stripe (and stripe
                    (map
                     #(into {} (map vector (first stripe) %))
                     (rest stripe)))
        stripe (and stripe
                    (map
                     (fn [o]
                       {:amount (-> (get o "Converted Amount" "\"0,0\"")
                                    (.slice 1 -1)
                                    (.replace "," ".")
                                    (js/parseInt 10))
                        :date (get o "Created (UTC)")
                        :email (get o "Customer Email")}) stripe))]
    stripe))
(defn handle-merkur [raw]
  (let [merkur (map #(split % #";") (split raw #"\n"))
        merkur (and (= 27 (count (first merkur))) merkur)]
    (map (fn [o]
           {:date (fix-date (nth o 23))
            :amount (js/parseInt (.replace (nth o 15) "," ".") 10)
            :name (nth o 21)
            :long-name
            (string/split
             (str-normalise (str (nth o 21) " " (nth o 7)))
             #" ")
            :address (nth o 7)})
         merkur)))
(defn update-users [users]
  (db! :tinkuy users)
  (doall
   (for [user users]
     (do
       (db! :users (str (get user "id")) user)
       (db! :email (get user "email") (get user "id"))))))
(go
  (update-users (<! (<ajax "https://www.tinkuy.dk/api/users"))))

(defn handle-file [id file]
  (go
    (db! :loading true)
    (let [raw (<! (<blob-text file))
          tinkuy (js->clj (u/parse-json-or-nil raw))
          stripe (handle-stripe raw)
          merkur (handle-merkur raw)]
      (cond
        tinkuy (update-users tinkuy)
        stripe (db! :stripe stripe)
        merkur (db! :merkur merkur)
        :else (js/alert "Input file not in expected format")))
    (db! :loading false)))

(defn file-input  [id & {:keys [title]
                         :or {title "upload"}}]
  [:div.ui.input
   [:label.ui.button.blue {:for (prn-str id)}
    title]
   [:input {:type :file
            :style {:display :none}
            :name (prn-str id)
            :key (prn-str id)
            :id (prn-str id)
            :value @(apply db id)
            :on-change #(handle-file id (aget (.-files (.-target %1)) 0))}]])

(defn add-entry [id entry]
  (db! :entries id 
       (conj
        (or @(db :entries id) #{})
        entry)))

(defn add-stripes []
  (doall
   (for [o @(db :stripe)]
     (let [id @(db :email (:email o))]
       (if id
         (add-entry id o)
         (db! :missing-stripe
              (conj @(db :missing-stripe) o)))))))

(defn match-merkur []
  (let [entries @(db :merkur)
        entries (filter
                 #(= -1 (.indexOf (:address %) "THE CURRENCY CLOUD LTD"))
                 entries)
        users (vals @(db :users))
        users (map
               #(into % {:names
                         (-> (get % "firstname")
                             (str " " (get % "surname"))
                             (str-normalise)
                             (string/split #" "))})
               users)]
    (doall
     (for [entry entries]
       (let [names (into #{} (:long-name entry))
             matches
             (loop [matches []
                    user (first users)
                    users (rest users)]
               (if (empty? users)
                 matches
                 (recur (if (every? names (:names user))
                          (conj matches user)
                          matches)
                        (first users)
                        (rest users))))]
         (if (= 1 (count matches))
           (add-entry (get (first matches) "id") entry)
           (db! :missing-merkur
                (conj @(db :missing-merkur) entry))))))))
(defn process []
  (when
   (and
    @(db :stripe)
    @(db :users)
    @(db :merkur)
    (not @(db :missing-stripe)))
    (db! :loading true)
    (db! :missing-stripe #{})
    (db! :missing-merkur #{})
    (go
      (<! (timeout 0))
      (add-stripes)
      (match-merkur)
      (db! :loading false)
      (log "processed..." @(db)))))
(log @(db))

(defn stripe-nouser []
  (let [stripe-missing (seq @(db :missing-stripe))
        stripe-emails (distinct (map :email stripe-missing))]
    [:p
     "Missing "
     (count stripe-emails)
     " tinkuy users for "
     (str (count stripe-missing))
     " stripe payments (no matching email addresses)"
     " The emails are:"
     [:ul
      (map (fn [o] [:li (str o)]) (sort stripe-emails))
     ; (.join (clj->js stripe-emails) ", ")
      ]]))

(defn merkur-nouser []
  (let [merkur-missing (seq @(db :missing-merkur))
        merkur-payments (distinct (map :long-name merkur-missing))]
    [:p
     "Missing tinkuy users for "
     (str (count merkur-missing))
     " merkur payments "
     " The missed payments are:"
     (into [:ul]
           (map (fn [o] [:li {key (prn-str o)}(.join (clj->js o) " ")]) (sort-by #(.join (clj->js %) " ") merkur-payments)))
     ]))

(defn show-user [obj]
  (let [id (get obj "id")
        type (get obj "status")
        payments @(db :entries id)
        latest-pay (last (sort-by :date payments))
        amount (get latest-pay :amount 0)
        expected-amount (case (get obj "membership_id")
                          1 54
                          (2 3) 108
                          4 216
                          0)
        ok (= amount expected-amount)
        ]
    (do
      [:tr {:key id
            :style {:background (if ok :white :red)}}
       [:td (get obj "firstname")
        " "
        (get obj "surname")
        [:br]
        (get obj "email")
        ]
       [:td type]
       [:td (case (get obj "membership_id")
              1 "støtte"
              2 "basis"
              3 "basis"
              4 "komplet"
              (get obj "membership_id")
              )]
       [:td amount]
       [:td (count @(db :entries id))]
       [:td (.slice (str (:date latest-pay)) 0 10)]
       ;[:td (prn-str latest-pay)]
       ;[:td (prn-str obj)]
       ]))
  )
(defn users []
  (let [objs
        (sort-by #(.toLowerCase (get % "firstname"))
                 (vals @(db :users)))
        objs (filter #(#{"pending" "active"} (get % "status")) objs)
        ]
    [:table
     [:tr
      [:th "navn"]
      [:th "type"]
      [:th "betalinger"]
      [:th "data"]]
     (doall
      (for [obj (filter #(zero? (count @(db :entries (get % "id")))) objs)]
        [show-user obj]))
     (doall
      (for [obj (filter #(< 0 (count @(db :entries (get % "id")))) objs)]
        [show-user obj]))
    ]))
(defn loading []
  (if @(subscribe [:db :loading])
    [:div
     {:style {:position :fixed
              :display :inline-block
              :top 0 :left 0
              :width "100%"
              :heigth "100%"
              :background-color "rgba(0,0,0,0.6)"
              :color "white"
              :z-index 100
              :padding-top (* 0.3 js/window.innerHeight)
              :text-align "center"
              :font-size "48px"
              :text-shadow "2px 2px 8px #000000"
              :padding-bottom (* 0.7 js/window.innerHeight)
              }}
     "Working..."
     ]
    [:span]
    ))
(defn main []
  (solsort.util/next-tick process)
  [:div.ui.container
   [loading]
   [:h1 "Tinkuy Member Check"]
   "Connect to tinkuy.dk, and upload bank and stripe report, to get a status for paying members. " [:br]
   "Upload tinkuy member export, bank statement, and stripe statement to compare."
   [:p
    [file-input [:text :users] :title "Upload"]]
   [:hr]
   [:p
    {:style {:line-wrap :none}}
    "Tinkuy: " (str (count @(db :users))) " entries loaded." [:br]
    "Stripe: " (str (count @(db :stripe))) " entries loaded."  [:br]
    "Merkur: " (str (count @(db :merkur))) " entries loaded."  [:br]]
   [merkur-nouser]
   [stripe-nouser]
   [users]
   ])
(render [main])
