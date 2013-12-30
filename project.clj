(defproject tailrecursion/boot.core "1.2.0"
  :description  "A dependency setup/build tool for Clojure."
  :url          "https://github.com/tailrecursion/boot.core"
  :license      {:name  "Eclipse Public License"
                 :url   "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins      [[lein-marginalia                 "0.7.1"]]
  :dependencies [[org.clojure/clojure             "1.5.1"]
                 [org.springframework/spring-core "1.2.2"]
                 [me.raynes/conch                 "0.5.0" :exclusions [org.clojure/clojure]]
                 [com.cemerick/pomegranate        "0.2.0" :exclusions [org.clojure/clojure]]
                 [digest                          "1.4.3" :exclusions [org.clojure/clojure]]]
  :main         tailrecursion.boot
  :profiles     {:dev {:dependencies [[tailrecursion/boot "0.2.2"]]}})
