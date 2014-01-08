; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.

; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns tailrecursion.boot.core
  "The boot core API."
  (:require
   [cemerick.pomegranate           :as pom]
   [clojure.java.io                :as io]
   [clojure.set                    :as set]
   [clojure.walk                   :as walk]
   [tailrecursion.boot.deps        :as deps]
   [tailrecursion.boot.file        :as file]
   [tailrecursion.boot.gitignore   :as git]
   [tailrecursion.boot.tmpregistry :as tmp])
  (:import
   [java.net URLClassLoader URL]
   java.lang.management.ManagementFactory))

(declare boot-env on-env! merge-env! outdirs out-files)

;; ## Utility Functions
;;
;; _These functions are used internally by boot and are not part of the public API._

(defn- tmpreg
  "Get the tempfile registry object for the current boot environment."
  []
  (get-in @boot-env [:system :tmpregistry]))

(defn- add-dependencies!
  "Add Maven dependencies to the classpath, fetching them if necessary."
  [deps repos]
  (require 'tailrecursion.boot.loader)
  ((resolve 'tailrecursion.boot.loader/add-dependencies!) deps repos))

(defn- add-directories!
  "Add directories to the classpath."
  [dirs]
  (when (seq dirs)
    (let [meth  (doto (.getDeclaredMethod URLClassLoader "addURL" (into-array Class [URL]))
                  (.setAccessible true))
          cldr  (ClassLoader/getSystemClassLoader)
          urls  (->> dirs (map io/file) (filter #(.exists %)) (map #(.. % toURI toURL)))]
      (doseq [url urls] (.invoke meth cldr (object-array [url]))))))

(defn- configure!*
  "Performs side-effects associated with changes to the env atom. Boot adds this
  function as a watcher on it."
  [old new]
  (doseq [k (set/union (set (keys old)) (set (keys new)))]
    (let [o (get old k ::noval)
          n (get new k ::noval)]
      (if (not= o n) (on-env! k o n new)))))

(def ^:private base-env
  "Returns initial boot environment settings."
  (fn []
    {:project       nil
     :version       nil
     :description   nil
     :license       nil
     :url           nil
     :boot-version  nil
     :dependencies  []
     :out-path      "out"
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

(defn- bind-syms [form]
  (let [sym? #(and (symbol? %) (not= '& %))]
    (->> form (tree-seq coll? seq) (filter sym?) distinct)))

(defmacro ^:private with-let
  "Binds resource to binding and evaluates body.  Then, returns
  resource.  It's a cross between doto and with-open."
  [[binding resource] & body]
  `(let [~binding ~resource] ~@body ~binding))

(defmacro ^:private guard
  [expr]
  `(try ~expr (catch Throwable _#)))

;; ## Boot Environment
;;
;; _These functions are used internally by boot and are not part of the public
;; API._

(def ^:dynamic *opts*
  "Command line options are bound to this var."
  {})

(def boot-env
  "Atom containing environment key/value pairs. Do not manipulate this atom
  directly. Use `set-env!` (below) instead."
  (atom nil))

(defn init!
  "Initialize the boot environment. This is normally run once by boot at startup.
  There should be no need to call this function directly."
  [& kvs]
  (doto boot-env
    (reset! (merge (base-env) (apply hash-map kvs)))
    (add-watch ::boot #(configure!* %3 %4))))

;; ## Boot Environment Extensions
;;
;; _Multimethods mediating and managing modifications to the boot environment._

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
;;
;; _Functions provided for use in boot tasks._

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

(defn add-sync!
  "Specify directories to sync after build event. The `dst` argument is the 
  destination directory. The `srcs` are an optional list of directories whose
  contents will be copied into `dst`. The `add-sync!` function is associative,
  for example:

    ;; These are equivalent:
    (add-sync! bar [baz baf])
    (do (add-sync! bar [baz]) (add-sync! bar [baf]))
  "
  [dst & [srcs]]
  (tmp/add-sync! (tmpreg) dst srcs))

(def ^:private src-filters (atom []))

(defn consume-src!
  "FIXME: document"
  [filter]
  (swap! src-filters conj filter))

(defn sync!
  "Trigger syncing of directories added via `add-sync!` now. This is used
  internally by boot."
  []
  (let [outfiles (set (out-files))
        consume  #(set/difference %1 (set (%2 %1)))
        keepers  (reduce consume outfiles @src-filters)
        deletes  (set/difference outfiles (set keepers))]
    (doseq [f deletes] (.delete f))
    (tmp/sync! (tmpreg))))

(defn ignored?
  "Returns truthy if the file f is ignored in the user's gitignore config."
  [f]
  ((get-in @boot-env [:system :gitignore]) f))

(defn tmpfile?
  "Returns truthy if the file f is a tmpfile managed by the tmpregistry."
  [f]
  (tmp/tmpfile? (tmpreg) f))

(defn mktmp!
  "Create a temp file and return its `File` object. If `mktmp!` has already 
  been called with the given `key` the tmpfile will be truncated. The optional
  `name` argument can be used to customize the temp file name (useful for
  creating temp files with a specific file extension, for example)."
  [key & [name]]
  (tmp/mk! (tmpreg) key name))

(defn mktmpdir!
  "Create a temp directory and return its `File` object. If `mktmpdir!` has
  already been called with the given `key` the directory's contents will be
  deleted. The optional `name` argument can be used to customize the temp
  directory name, as with `mktmp!` above."
  [key & [name]]
  (tmp/mkdir! (tmpreg) key name))

(defn mkoutdir!
  "FIXME: document"
  [key & [name]]
  (with-let [f (mktmpdir! key name)]
    (set-env! :src-paths #{(.getPath f)})
    (add-sync! (get-env :out-path) [(.getPath f)])))

(defn unmk!
  "Delete the temp/out file or directory created with the given `key`."
  [key]
  (tmp/unmk! (tmpreg) key))

(defn outdirs
  "FIXME: document"
  []
  (->> :src-paths get-env (map io/file) (filter tmpfile?)))

(defn out-files
  "FIXME: document"
  []
  (->> (outdirs) (mapcat file-seq) (filter #(.isFile %))))

(defn deps
  "Returns (FIXME: what exactly does this return?)"
  []
  (deps/deps boot-env))

(defmacro deftask
  "Define a new task. Task definitions may contain nested task definitions.
  Nested task definitions are reified when the task they're nested in is run.

  A typical task definition might look like this:

    (deftask my-task [& args]
      ;; Optionally set environment keys:
      (set-env!
        :repositories #{...}
        :dependencies [[...]]
        ...)

      ;; Optionally require some namespaces:
      (require
        '[com.me.my-library :as mylib])

      ;; Return middleware function (or
      ;; #'clojure.core/identity to pass thru to
      ;; next middleware without doing anything).
      (fn [continue]
        (fn [event]
          (mylib/do-something args))))
  "
  [name & forms]
  (let [[_ name [_ & arities]] (macroexpand-1 `(defn ~name ~@forms))]
    `(def ~(vary-meta name assoc ::task true) (fn ~@arities))))

(defmacro deftask*
  "Used internally by boot to define ad-hoc tasks so that name resolution in the
  tasks can be done dynamically at runtime."
  [name & forms]
  (let [[_ name [_ & arities]] (macroexpand-1 `(defn ~name ~@forms))
        dynvs   (atom [])
        arity   (fn [[bind & body]]
                  (let [binds (->> bind bind-syms (map (juxt gensym identity)))]
                    (swap! dynvs into (map first binds))
                    `(~bind
                       (binding [~@(mapcat identity binds)]
                         ~@(for [x body]
                             `(eval '(let [~@(mapcat reverse binds)] ~x)))))))
        arities (doall (map arity arities))]
    `(do
       ~@(for [x @dynvs] `(def ~(vary-meta x assoc :dynamic true) nil))
       (def ~(vary-meta name assoc ::task true) (fn ~@arities)))))

(defn make-event
  "Creates a new event map with base info about the build event. If the `event`
  argument is given the new event map is merged into it. This event map is what
  is passed down through the handler functions during the build."
  ([]
     (make-event {}))
  ([event]
     (merge event {:id (gensym) :time (System/currentTimeMillis)})))

(defn prep-build!
  "FIXME: document"
  [& args]
  (doseq [f (outdirs)] (tmp/make-file! ::tmp/dir f))
  (apply make-event args))

(defmacro boot
  "Builds the project as if `argv` was given on the command line."
  [& argv]
  (let [->list #(cond (seq? %) % (vector? %) (seq %) :else (list %))
        ->app  (fn [[x & xs]] (if-not (seq xs) x `(comp ~x ~@xs)))]
    `((~(->app (map ->list argv)) #(do (sync!) %)) (prep-build!))))

;; ## Low-Level Tasks / Task Helpers

(def ^:dynamic *event* nil)

(defn pre-wrap
  "This task applies `f` to the event map and any `args`, and then passes the
  result to its continuation."
  [f & args]
  (fn [continue]
    (fn [event]
      (continue (apply f event args)))))

(defmacro with-pre-wrap
  "Emits a task wherein `body` expressions are evaluated for side effects before
  calling the continuation."
  [& body]
  `(fn [continue#]
     (fn [event#]
       (binding [*event* event#]
         ~@body)
       (continue# event#))))

(defn post-wrap
  "This task calls its continuation and then applies `f` to it and any `args`,
  returning the result."
  [f & args]
  (fn [continue]
    (fn [event]
      (apply f (continue event) args))))

(defmacro with-post-wrap
  "Emits a task wherein `body` expressions are evaluated for side effects after
  calling the continuation."
  [& body]
  `(fn [continue#]
     (fn [event#]
       (continue# event#)
       (binding [*event* event#]
         ~@body))))

;; ## Public Utility Functions

(defn src-files
  "Returns a seq of files in :src-paths."
  []
  (->> :src-paths get-env (map io/file) (mapcat file-seq) (filter #(.isFile %))))

(defn relative-path
  "FIXME: document"
  [f]
  (->> (get-env :src-paths)
    (map #(.getPath (file/relative-to (io/file %) f)))
    (some #(and (not= f (io/file %)) (guard (io/as-relative-path %)) %))))

(defn file-filter
  "FIXME: Document this."
  [mkpred]
  (fn [criteria files & [negate?]]
    ((if negate? remove filter)
     #(some identity ((apply juxt (map mkpred criteria)) %)) files)))

(def by-ext
  "FIXME: Document this."
  (file-filter #(fn [f] (.endsWith (.getName f) %))))

(def by-re
  "FIXME: Document this."
  (file-filter #(fn [f] (re-find % (.getName f)))))
