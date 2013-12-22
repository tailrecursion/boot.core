(defproject tailrecursion/boot.core "1.1.1"
  :description  "A dependency setup/build tool for Clojure."
  :url          "https://github.com/tailrecursion/boot.core"
  :license      {:name  "Eclipse Public License"
                 :url   "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.springframework/spring-core "1.2.2"]
                 [me.raynes/conch                 "0.5.0" :exclusions [org.clojure/clojure]]
                 [com.cemerick/pomegranate        "0.2.0" :exclusions [org.clojure/clojure]]
                 [digest                          "1.4.3" :exclusions [org.clojure/clojure]]]
  :aot          :all
  :main         tailrecursion.boot
  :profiles     {:dev {:dependencies [[org.clojure/clojure "1.5.1"]]}})
