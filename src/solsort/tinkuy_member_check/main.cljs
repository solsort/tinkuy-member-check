(ns solsort.tinkuy_member_check.main
  (:require-macros
   [reagent.ratom :as ratom]
   [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require
   [solsort.misc :refer [<blob-url]]
   [solsort.util
    :refer
    [<p <ajax <seq<! js-seq normalize-css load-style! put!close!
     parse-json-or-nil log page-ready render dom->clj next-tick]]
   [reagent.core :as reagent :refer []]
   [cljs.core.async :as async :refer [>! <! chan put! take! timeout close! pipe]]))

(defn main []
  [:div
   [:h1 "Tinkuy Member Check"]
   [:div
    "Connect to tinkuy.dk, and upload bank and stripe report, to get a status for paying members"]
   ])

(render [main])
