; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns tailrecursion.boot
  (:require
   [clojure.string                  :as string]
   [clojure.java.io                 :as io]
   [clojure.pprint                  :as pprint]
   [tailrecursion.boot.core         :as core]
   [tailrecursion.boot.file         :as file]
   [tailrecursion.boot.semver       :as semver]
   [tailrecursion.boot.tmpregistry  :as tmp]
   [tailrecursion.boot.gitignore    :as git]))

(def min-loader-version "1.0.0")
(def max-loader-version "2.0.0")

(defmacro with-rethrow [expr msg]
  `(try ~expr (catch Throwable e# (throw (Exception. ~msg e#)))))

(defn pp      [expr] (pprint/write expr :dispatch pprint/code-dispatch))
(defn pp-str  [expr] (with-out-str (pp expr)))

(defn lines-seq [f]
  (with-rethrow
    (with-open [r (io/reader f)] (doall (line-seq r)))
    (format "can't read file: %s" f)))

(defn read-script [f]
  (with-rethrow
    (read-string (str "(" (string/join "\n" (rest (lines-seq f))) ")"))
    (format "can't read file as EDN: %s" (.getPath f))))

(defn read-cli [argv]
  (with-rethrow
    (read-string (str "(" (string/join " " argv) ")"))
    (format "can't read command line as EDN: %s" argv)))

(defn build-script [forms argv argv*]
  `(~'(ns tailrecursion.boot.user
        (:require
         [tailrecursion.boot.core :refer :all]
         [tailrecursion.boot.core.task :refer :all]))
    ~@forms
    (if-let [main# (resolve '~'-main)]
      (main# ~@argv)
      (core/boot ~@argv*))))

(defn add-linums [text]
  (let [lines  (string/split text #"\n")
        doline #(printf "%d %s\n" (inc %1) %2)]
    (with-out-str (doall (map-indexed doline lines)))))

(defn assert-loader-version [info]
  (let [ver  (get (if (map? info) info {}) :vers "0.0.0")
        [min max :as vers] (list min-loader-version max-loader-version)
        [ver* min* max*] (map #(:major (semver/version %)) (cons ver vers))]
    (assert (and (<= min* ver*) (< ver* max*))
      (format "boot executable version is %s: [%s, %s) required" ver min max))))

(defn -main [loader-info script-file & argv]
  (assert-loader-version loader-info)
  (let [argv*  (or (seq (read-cli argv)) '(help))
        tmpf   (.getPath (file/tmpfile "boot-" ".clj"))
        forms  (build-script (read-script (io/file script-file)) argv argv*)
        script (string/join "\n\n" (map pp-str forms))]
    (core/set-env! :boot-version loader-info :bootscript tmpf)
    (with-rethrow
      (doto tmpf (spit script) (load-file))
      (format "\n\n%s" (add-linums script)))))
