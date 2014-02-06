(ns cljs-repl
  (:require [cemerick.austin :as austin]
            [cemerick.austin.repls :as austin.repls]
            [hiccup.core :refer [html]]
            [compojure.core :refer (GET defroutes)]
            ring.adapter.jetty
            [clojure.java.io :as io]))

(defroutes site
  (GET "/*" req (html [:html
                       [:head [:script (slurp "target/app.js")]]
                       [:body "test page"
                        [:script (austin.repls/browser-connected-repl-js)]]])))

(defn phantom-repl []
  (austin.repls/cljs-repl (austin/exec-env)))

(defn browser-repl []
  (defonce ^:private server
    (ring.adapter.jetty/run-jetty #'site {:port 8080 :join? false}))
  (println "Open a browser @ http://localhost:8080")
  (austin.repls/cljs-repl (reset! austin.repls/browser-repl-env
                                  (austin/repl-env))))
