; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

;;
(ns tailrecursion.boot
  (:require [clojure.string                    :as string]
            [clojure.java.io                   :as io]
            [clojure.pprint                    :refer [pprint]]
            [tailrecursion.boot.core           :as core]
            [tailrecursion.boot.tmpregistry    :as tmp]
            [tailrecursion.boot.gitignore      :as git])
  (:import java.lang.management.ManagementFactory))

(def base-env
  "Returns a basic boot environment map. The `boot.edn` and `~/.boot.edn`
  configuration is merged into this and stored in a boot atom."
  (fn []
    {:project       nil
     :version       nil
     :boot-version  nil
     :dependencies  []
     :src-paths     #{}
     :src-static    #{}
     :repositories  #{"http://clojars.org/repo/"
                      "http://repo1.maven.org/maven2/"}
     :require-tasks '#{}
     :test          "test"
     :target        "target"
     :resources     "resources"
     :public        "resources/public"
     :system        {:cwd         (io/file (System/getProperty "user.dir"))
                     :home        (io/file (System/getProperty "user.home"))
                     :jvm-opts    (vec (.. ManagementFactory getRuntimeMXBean getInputArguments))
                     :bootfile    (io/file (System/getProperty "user.dir") "boot.edn")
                     :userfile    (io/file (System/getProperty "user.home") ".boot.edn")
                     :tmpregistry (tmp/init! (tmp/registry (io/file ".boot" "tmp")))
                     :gitignore   (git/make-gitignore-matcher)}
     :tasks         {}}))

(defn exists? [f]
  (when (core/guard (.exists f)) f))

(defn read-file [f]
  (try (read-string (str "(" (try (slurp f) (catch Throwable x)) ")"))
    (catch Throwable e
      (throw (Exception.
               (format "%s (Can't read forms from file)" (.getPath f)) e)))))

(defn read-config [f]
  (let [config (first (read-file f))
        asrt-m #(do (assert (map? %1) %2) %1)]
    (asrt-m config (format "%s (Configuration must be a map)" (.getPath f)))))

(defn read-cli-args [args]
  (let [s (try (read-string (str "(" (string/join " " args) ")"))
            (catch Throwable e
              (throw (Exception. "Can't read command line forms" e))))]
    (->> s
      (map #(if (vector? %) % [%]))
      (map (fn [[op & args]] `[~(if (keyword? op) op (keyword op)) ~@args])))))

(defn -main [loader-info & args]
  (let [boot  (core/init! (base-env))
        {:keys [userfile bootfile]} (:system @boot)
        argv  (or (seq (read-cli-args args)) (list [:help]))
        usr   (if-let [f (exists? userfile)] (read-config f) {})
        cfg   (merge (read-config bootfile) {:main (into [:do] argv)})
        init  {:boot-version  (:boot-version loader-info)
               :require-tasks '#{[tailrecursion.boot.core.task :refer :all]}}]
    ((core/create-app! boot init usr cfg) (core/make-event boot))
    (System/exit 0)))
