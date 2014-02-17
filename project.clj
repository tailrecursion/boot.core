(defproject tailrecursion/boot.core "2.2.1"
  :description  "A script interpreter for Clojure. Also a build tool."
  :url          "https://github.com/tailrecursion/boot.core"
  :license      {:name  "Eclipse Public License"
                 :url   "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins      [[lein-marginalia                 "0.8.0-SNAPSHOT"]]
  :dependencies [[org.springframework/spring-core "1.2.2"]
                 [me.raynes/conch                 "0.5.0" :exclusions [org.clojure/clojure]]
                 [tailrecursion/warp              "0.1.0" :exclusions [org.clojure/clojure]]
                 [reply                           "0.2.0" :exclusions [org.clojure/clojure]]]
  :profiles     {:dev {:dependencies [[org.clojure/clojure      "1.5.1"]
                                      [com.cemerick/pomegranate "0.2.0"]
                                      [digest                   "1.4.3"]]
                       :source-paths ["../boot/src" "src"]}})
