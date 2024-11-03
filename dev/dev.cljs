(ns dev
  (:require
   ["happy-dom" :as happy-dom]))

(defn create-window ^js [& {:as opts}]
  (happy-dom/Window. (clj->js opts)))

(defn dispose-window [^js/Window window]
  (-> window .-happyDOM .waitUntilComplete
    (.then
      (fn []
        (-> window .-happyDOM .close)))))

