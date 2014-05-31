;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.boot.deps
  (:import
    [java.util.jar JarFile]
    [java.util.zip ZipFile])
  (:require
   [clojure.java.io             :as io]
   [tailrecursion.boot.kahnsort :as kahn]))

(defn entries [jar]
  (->> (.entries jar)
    enumeration-seq
    (map #(vector (.getName %) (.getInputStream jar %)))
    (into {})))

(defn ^:deprecated resolve-deps* [coords repos]
  (require 'cemerick.pomegranate.aether)
  (let [resolve-dependencies (resolve 'cemerick.pomegranate.aether/resolve-dependencies)]
    (->> (resolve-dependencies :coordinates coords :repositories (zipmap repos repos))
      (kahn/topo-sort)
      (map (fn [x] {:dep x :jar (.getPath (:file (meta x)))})))))

(defn resolve-deps! []
  (require 'tailrecursion.boot.loader)
  (require 'tailrecursion.boot.classlojure.core)
  (when-let [get-loader (resolve 'tailrecursion.boot.loader/get-classloader)]
    (let [eval-in (resolve 'tailrecursion.boot.classlojure.core/eval-in)]
      (fn [coords repos]
        (eval-in (get-loader)
          `(do (require 'tailrecursion.boot-classloader)
               (->> (tailrecursion.boot-classloader/resolve-dependencies!* '~coords '~repos)
                 (map (comp (fn [~'x] {:dep ~'x :jar (.getPath (:file (meta ~'x)))}) first)))))))))

(defn deps [env]
  (require 'tailrecursion.boot.loader)
  (let [{repos :repositories coords :dependencies} @env]
    (->> ((or (resolve-deps!) resolve-deps*) coords repos)
      (map :jar)
      (filter #(.endsWith % ".jar"))
      (map #(JarFile. (io/file %)))
      (map #(vector (.getName %) (entries %))))))
