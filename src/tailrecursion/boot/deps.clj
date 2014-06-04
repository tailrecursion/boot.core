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

(def ^:private memoized-resolve-deps! (atom nil))

(defn resolve-deps!
  "FIXME: look at this function. just look at it."
  []
  (require 'tailrecursion.boot.loader)
  (require 'tailrecursion.boot.classlojure.core)
  (when-let [get-loader (resolve 'tailrecursion.boot.loader/get-classloader)]
    (compare-and-set! memoized-resolve-deps! nil
      (let [eval-in (resolve 'tailrecursion.boot.classlojure.core/eval-in)]
        (memoize
          (fn [coords repos]
            (eval-in (get-loader)
              `(do (require
                     '[clojure.set                    :as ~'x-set]
                     '[cemerick.pomegranate.aether    :as ~'x-aether]
                     '[tailrecursion.boot-classloader :as ~'x-loader])
                   (let [res#  (fn [x#] (x-aether/resolve-dependencies
                                          :coordinates  x#
                                          :repositories (zipmap '~repos '~repos)))
                         sort# (partial group-by (comp zero? count second))
                         proc# (fn [x#] (->> x# (map (juxt ffirst (comp set (partial map first) second)))))
                         derp# (fn [x#] (->> x# first vector res#
                                          (filter (comp (partial = (ffirst x#)) ffirst))
                                          first second))
                         diff# (fn [x# y#] (x-set/difference y# x#))
                         d#    (res# '~coords)
                         deps# (->> d# (reduce (fn [xs# x#] (assoc xs# (ffirst x#) (first x#))) {}))
                         dd#   (->> d# (map (juxt first derp#)) proc# sort#)
                         dd#   (loop [sorted# (dd# true) unsorted# (dd# false)]
                                 (if-not (seq unsorted#)
                                   (map first sorted#)
                                   (let [set# (set (map first sorted#))
                                         ddd# (->> unsorted#
                                                (map (juxt first (comp (partial diff# set#) second)))
                                                sort#)]
                                     (recur (into sorted# (ddd# true)) (ddd# false)))))]
                     (->> dd# (map (fn [x#] {:dep (deps# x#) :jar (.getPath (:file (meta (deps# x#))))}))))))))))
    @memoized-resolve-deps!))

(defn deps [env]
  (require 'tailrecursion.boot.loader)
  (let [{repos :repositories coords :dependencies} @env]
    (->> ((or (resolve-deps!) resolve-deps*) coords repos)
      (map :jar)
      (filter #(.endsWith % ".jar"))
      (map #(JarFile. (io/file %)))
      (map #(vector (.getName %) (entries %))))))
