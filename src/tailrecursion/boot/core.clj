; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns tailrecursion.boot.core
  (:require
   [cemerick.pomegranate           :as pom]
   [clojure.java.io                :as io]
   [clojure.set                    :as set]
   [tailrecursion.boot.deps        :as deps]
   [tailrecursion.boot.gitignore   :as git]
   [tailrecursion.boot.tmpregistry :as tmp])
  (:import
   [java.net URLClassLoader URL]
   java.lang.management.ManagementFactory))

(declare boot-env on-env! merge-env!)

;; ## Utility Functions

(defn- tmpreg []
  (get-in @boot-env [:system :tmpregistry]))

(defn- add-dependencies! [deps repos]
  (require 'tailrecursion.boot.loader)
  ((resolve 'tailrecursion.boot.loader/add-dependencies!) deps repos))

(defn- add-directories! [dirs]
  (when (seq dirs)
    (let [meth  (doto (.getDeclaredMethod URLClassLoader "addURL" (into-array Class [URL]))
                  (.setAccessible true))
          cldr  (ClassLoader/getSystemClassLoader)
          urls  (->> dirs (map io/file) (filter #(.exists %)) (map #(.. % toURI toURL)))]
      (doseq [url urls] (.invoke meth cldr (object-array [url]))))))

(defn- configure!* [old new]
  (doseq [k (set/union (set (keys old)) (set (keys new)))]
    (let [o (get old k ::noval)
          n (get new k ::noval)]
      (if (not= o n) (on-env! k o n new)))))

(def ^:private base-env
  (fn []
    {:project       nil
     :version       nil
     :boot-version  nil
     :dependencies  []
     :src-paths     #{}
     :src-static    #{}
     :repositories  #{"http://clojars.org/repo/"
                      "http://repo1.maven.org/maven2/"}
     :test          "test"
     :target        "target"
     :resources     "resources"
     :public        "resources/public"
     :system        {:cwd         (io/file (System/getProperty "user.dir"))
                     :home        (io/file (System/getProperty "user.home"))
                     :jvm-opts    (vec (.. ManagementFactory getRuntimeMXBean getInputArguments))
                     :tmpregistry (tmp/init! (tmp/registry (io/file ".boot" "tmp")))
                     :gitignore   (git/make-gitignore-matcher)}}))

;; ## Boot Environment Extensions

(def boot-env
  "Atom containing environment key/value pairs. Environment should not be
  manipulated directly, instead use `set-env!` below. See also the `on-env!`
  and `merge-env!` multimethods below."
  (atom nil))

(doto boot-env (reset! (base-env)) (add-watch ::boot #(configure!* %3 %4)))

(defmulti on-env!
  "Event handler called when the boot atom is modified. This handler is for
  performing side-effects associated with maintaining the application state in
  the boot atom. For example, when `:dependencies` is modified the handler
  fetches dependency JARs from Maven and adds them to the classpath."
  (fn [key old-value new-value env] key) :default ::default)

(defmethod on-env! ::default     [key old new env] nil)
(defmethod on-env! :dependencies [key old new env] (add-dependencies! new (:repositories env)))
(defmethod on-env! :src-paths    [key old new env] (add-directories! (set/difference new old)))

(defmulti merge-env!
  "This function is used to modify how new values are merged into the boot atom
  when `set-env!` is called. This function's result will become the new value
  associated with the given `key` in the boot atom."
  (fn [key old-value new-value env] key) :default ::default)

(defmethod merge-env! ::default      [key old new env] new)
(defmethod merge-env! :repositories  [key old new env] (into (or old #{}) new))
(defmethod merge-env! :dependencies  [key old new env] (into (or old  []) new))
(defmethod merge-env! :src-static    [key old new env] (into (or old #{}) new))
(defmethod merge-env! :src-paths     [key old new env] (into (or old #{}) new))

;; ## Boot API Functions

(defn ignored?
  "Returns truthy if the file f is ignored in the user's gitignore config."
  [f]
  ((get-in @boot-env [:system :gitignore]) f))

(defn tmpfile?
  "Returns truthy if the file f is a tmpfile managed by the tmpregistry."
  [f]
  (tmp/tmpfile? (tmpreg) f))

(defn mk!
  "Create a temp file and return its `File` object. If `mk!` has already been
  called with the given `key` the tmpfile will be truncated. The optional `name`
  argument can be used to customize the temp file name (useful for creating temp
  files with a specific file extension, for example)."
  [key & [name]]
  (tmp/mk! (tmpreg) key name))

(defn mkdir!
  "Create a temp directory and return its `File` object. If `mkdir!` has already
  been called with the given `key` the directory's contents will be deleted. The
  optional `name` argument can be used to customize the temp directory name, as
  with `mk!` above."
  [key & [name]]
  (tmp/mkdir! (tmpreg) key name))

(defn unmk!
  "Delete the temp file or directory created with the given `key`."
  [key]
  (tmp/unmk! (tmpreg) key))

(defn add-sync!
  "Specify directories to sync after build event. The `dst` argument is the 
  destination directory. The `srcs` are an optional list of directories whose
  contents will be copied into `dst`. The `add-sync!` function is associative,
  for example:

  ```
  ;; These are equivalent:
  (add-sync! bar baz baf)
  (do (add-sync! bar) (add-sync! baz baf))
  ```"
  [dst & [srcs]]
  (tmp/add-sync! (tmpreg) dst srcs))

(defn sync!
  "Trigger syncing of directories added via `add-sync!` now. This is used
  internally by boot."
  []
  (tmp/sync! (tmpreg)))

(defn deps
  "Returns (FIXME: what exactly does this return?)"
  []
  (deps/deps boot-env))


(defmacro deftask
  "Define a new task. Task definitions may contain nested task definitions.
  Nested task definitions are reified when the task they're nested in is run.

  A typical task definition might look like this:

  ```
  (deftask my-task [& args]
    ;; Optionally set environment keys:
    (set-env!
      :repositories #{...}
      :dependencies [[...]]
      ...)
  
    ;; Optionally require some namespaces:
    (require
      '[com.me.my-library :as mylib])
  
    ;; Return middleware function (or #'clojure.core/identity to pass thru to
    ;; next middleware without doing anything).
    (fn [continue]
      (fn [event]
        (mylib/do-something args))))
  ```"
  [name & forms]
  (let [[_ name [_ & arities]] (macroexpand-1 `(defn ~name ~@forms))
        arity (fn [[bind & body]] `(~bind ~@(for [x body] `(eval '~x))))]
    `(def ~(vary-meta name assoc ::task true) (fn ~@(map arity arities)))))

(defn set-env!
  "Update the boot environment atom `this` with the given key-value pairs given
  in `kvs`. See also `on-env!` and `merge-env!`."
  [& kvs]
  (doseq [[k v] (partition 2 kvs)]
    (swap! boot-env update-in [k] (partial merge-env! k) v @boot-env)))

(defn get-env
  "Returns the value associated with the key `k` in the boot environment, or
  `not-found` if the environment doesn't contain key `k` and `not-found` was
  given. Calling this function with no arguments returns the environment map."
  [& [k not-found]]
  (if k (get @boot-env k not-found) @boot-env))

(defn make-event
  "Creates a new event map with base info about the build event. If the `event`
  argument is given the new event map is merged into it. This event map is what
  is passed down through the handler functions during the build."
  ([] (make-event {}))
  ([event]
   (let [srcs    (->> @boot-env :src-paths (map io/file)
                      (mapcat file-seq) (filter #(.isFile %)))
         watched (->> srcs (remove (partial ignored? boot-env)) set)]
     (merge event {:id        (gensym)
                   :time      (System/currentTimeMillis)
                   :watch     {:time watched, :hash watched}
                   :src-files (set srcs)}))))

(defmacro boot
  "Builds the project as if `argv` was given on the command line."
  [& argv]
  (let [->list #(cond (seq? %) % (vector? %) (seq %) :else (list %))
        ->mw   #(if (< 1 (count %)) % (first %))]
    `((~(->mw (map ->list argv)) #(do (sync!) (flush) %)) (make-event))))
