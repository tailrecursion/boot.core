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
   [clojure.stacktrace              :as trace]
   [tailrecursion.warp              :as !]
   [tailrecursion.boot.core         :as core]
   [tailrecursion.boot.core.task    :as task]
   [tailrecursion.boot.file         :as file]
   [tailrecursion.boot.semver       :as semver]
   [tailrecursion.boot.tmpregistry  :as tmp]
   [tailrecursion.boot.gitignore    :as git]))

(def min-loader-version "1.0.0")
(def max-loader-version "2.0.0")

(defn print-traces [e nelems]
  (cond
    (nil?  nelems) (trace/print-cause-trace e)
    (zero? nelems) (loop [e e, s ""]
                     (printf "%s%s\n" s (.getMessage e))
                     (when-let [e (.getCause e)] (recur e "↳ Caused by: ")))
    :else          (trace/print-cause-trace e nelems)))

(defmacro with-terminate [& body]
  `(try
     ~@body
     (System/exit 0)
     (catch Throwable e#
       (binding [*out* *err*]
         (print-traces e# (:v core/*opts*))
         (System/exit 1)))))

(defmacro with-rethrow [expr msg]
  `(try ~expr (catch Throwable e# (throw (Exception. ~msg e#)))))

(defn pp      [expr] (pprint/write expr :dispatch pprint/code-dispatch))
(defn pp-str  [expr] (with-out-str (pp expr)))

(defn read-cli [argv]
  (let [src (str "(" (string/join " " argv) ")")]
    (with-rethrow
      (read-string src)
      (format "Can't read command line as EDN: %s" src))))

(defn parse-cli [argv]
  (!/try*
    (let [opt?   (comp keyword? first)
          dfltsk `(((core/get-env :default-task)))
          ->opt  #(if (< 1 (count %)) (vec %) [(first %) nil])
          ->expr #(cond (seq? %) % (vector? %) (list* %) :else (list %))
          tasks  (->> (or (seq (read-cli argv)) dfltsk) (map ->expr))
          argv*  (or (->> tasks (remove opt?) seq) dfltsk)
          opts   (->> tasks (filter opt?) (map ->opt) (into {}))]
      (set! core/*opts* (merge core/*opts* opts))
      argv*)
    (with-out-str (print-traces !/*e* (:v core/*opts*)))))

(defn build-script [forms argv argv* edn-ex]
  `(~'(ns tailrecursion.boot.user
        (:require
         [tailrecursion.boot.core.task :refer :all]
         [tailrecursion.boot.core :refer :all :exclude [deftask]]))
    (defmacro ~'deftask
        [~'& ~'args]
        (list* '~'deftask* ~'args))
    ~@forms
    (if-let [main# (resolve '~'-main)]
      (main# ~@argv)
      ~(if edn-ex
         `(binding [*out* *err*]
            (print ~edn-ex)
            (System/exit 1))
         `(core/boot ~@argv*)))))

(defn loader-version-compatible? [loader-version]
  (let [[ver min max] [loader-version min-loader-version max-loader-version]]
    (and ver (not (semver/older? ver min)) (semver/older? ver max))))

(defn assert-loader-version [info]
  (let [ver (get info 'tailrecursion/boot)]
    (assert (loader-version-compatible? ver)
      (format "Boot executable version is %s: boot.core requires [%s, %s)"
        (or ver "too old") min-loader-version max-loader-version))))

(def dfl-opts {:v 0})

(defn -main [loader-info script-file script-forms & argv]
  (binding [core/*opts* dfl-opts]
    (with-terminate
      (assert-loader-version loader-info)
      (let [vers   (get loader-info 'tailrecursion/boot)
            cljarg (parse-cli argv)
            ex     (when (string? cljarg) cljarg)
            argv*  (when-not (string? cljarg) cljarg)
            forms  (build-script script-forms argv argv* ex)
            script (-> (string/join "\n\n" (map pp-str forms)) (str "\n"))]
        (core/init!
          :boot-version vers
          :boot-info    loader-info
          :boot-script  script-file
          :dependencies (:dependencies loader-info)
          :default-task task/help)
        (let [tmpd (core/mktmpdir! ::bootscript)
              file #(doto (apply io/file %&) io/make-parents)
              tmpf (.getPath (file tmpd "tailrecursion" "boot" "user.clj"))]
          (core/set-env! :boot-user-ns-file tmpf)
          (doto tmpf (spit script) (load-file)))))))
