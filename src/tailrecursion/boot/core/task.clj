;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns tailrecursion.boot.core.task
  (:require
   [me.raynes.conch.low-level      :as sh]
   [clojure.java.io                :as io]
   [clojure.set                    :as set]
   [clojure.pprint                 :as pprint]
   [clojure.string                 :as string]
   [tailrecursion.warp             :as !]
   [tailrecursion.boot.core        :as core]
   [tailrecursion.boot.file        :as file]
   [tailrecursion.boot.table.core  :as table]))

(defn- first-line [s] (when s (first (string/split s #"\n"))))
(defn- not-blank? [s] (when-not (string/blank? s) s))
(defn- now        [ ] (System/currentTimeMillis))
(defn- ms->s      [x] (double (/ x 1000)))

(defmacro ^:private print-time [ok fail expr]
  `(let [start# (now)]
     (!/try*
       (let [end# (do ~expr (ms->s (- (now) start#)))]
         (printf ~ok end#))
       (!/cause-trace* ~fail (ms->s (- (now) start#))))))


(defn- print-tasks [tasks]
  (let [get-task  #(-> % :name str)
        get-desc  #(-> % :doc first-line)
        get-row   (fn [x] [(get-task x) (get-desc x)])]
    (with-out-str (table/table (into [["" ""]] (map get-row tasks)) :style :none))))

(defn- pad-left [thing lines]
  (let [pad (apply str (repeat (count thing) " "))
        pads (concat [thing] (repeat pad))]
    (string/join "\n" (map (comp (partial apply str) vector) pads lines))))

(defn- version-str []
  (let [proj "tailrecursion/boot"
        url  "http://github.com/tailrecursion/boot"]
    (format "%s %s: %s" proj (core/get-env :boot-version) url)))

(defn- available-tasks [sym]
  (let [base  {nil (the-ns sym)}
        task? #(:tailrecursion.boot.core/task %)
        addrf #(if-not (seq %1) %2 (symbol %1 (str %2)))
        proc  (fn [a [k v]] (assoc (meta v) :name (addrf a k) :var v))
        pubs  (fn [[k v]] (map proc (repeat (str k)) (ns-publics v)))]
    (->>
      (concat
        (->> sym ns-refers (map proc (repeat nil)))
        (->> sym ns-aliases (into base) (mapcat pubs)))
      (filter task?) (sort-by :name))))

(def ^:private ^:dynamic *sh-dir* nil)

(defn- sh [& args]
  (let [opts (into [:redirect-err true] (when *sh-dir* [:dir *sh-dir*]))
        proc (apply sh/proc (concat args opts))]
    (future (sh/stream-to-out proc :out))
    #(.waitFor (:process proc))))

(defn- pp-str [form]
  (with-out-str (pprint/pprint form)))

(defn- ns-requires [sym]
  (let [ns   (the-ns sym)
        core (the-ns 'clojure.core)
        sift #(remove (partial contains? #{nil ns core}) (set %))]
    (-> (->> sym ns-aliases vals)
      (concat (->> sym ns-map vals (map (comp :ns meta))))
      sift)))

;; ## Core Tasks
;;
;; These tasks are included in boot core and are listed by the `help` task.

(core/deftask debug
  "Print the value of a boot environment key.

  Multiple `keys` specify a path, similar to `#'clojure.core/get-in`."
  [& keys]
  (core/with-pre-wrap
    (let [e (core/get-env)]
      (print (pp-str (if-not (seq keys) e (get-in e keys)))))))

(core/deftask help
  "Print help and usage info for a task.
  
  Help and usage info is derived from metadata on the task var. The task must be
  required into the boot script namespace for this medatada to be found."
  ([]
     (core/with-pre-wrap
       (let [tasks (available-tasks 'tailrecursion.boot.user)]
         (printf "%s\n\n" (version-str))
         (->
           (mapv
             #(format % "boot")
             ["%s OPTS task ..."
              "%s OPTS [task arg arg] ..."
              "%s OPTS [help task]"])
           (->> (pad-left "Usage: ") println))
         (printf "\n%s\n"
           (pad-left "OPTS:  "
             (->
               [["" ""]
                [":v"     "Verbose exceptions (full cause trace)."]
                ["[:v n]" "Cause trace limited to `n` elements each."]]
               (table/table :style :none)
               with-out-str
               (string/split #"\n"))))
         (printf "\n%s\n\n"
           (pad-left "Tasks: " (string/split (print-tasks tasks) #"\n")))
         (print
           (str
             "Create a minimal boot script: `boot :strap > build.boot`\n\n")))))
  ([task]
     (core/with-pre-wrap
       (let [task* (->> (available-tasks 'tailrecursion.boot.user)
                     (map :var) (filter #(= task (var-get %))) first)
             {args :arglists doc :doc task :name} (meta task*)]
         (assert task* "Not a valid task")
         (printf "%s\n\n" (version-str))
         (printf "%s\n%s\n  %s\n\n" task args doc)))))

(defn uberboot
  "Build AOT compiled boot loader with all project deps included.
  
  The new boot executable will be created either with the path specified by the
  outfile argument or at ./boot if outfile isn't provided.

  Note that AOT compilation may not work with certain dependencies. These deps
  must be excluded from the uberjar. Add an `:uberjar-exclusions` key to 
  environment with a vector of regexes matching paths to be excluded from the
  jar file)."
  [& [outfile]]
  (core/with-pre-wrap
    (let [pdir (core/mktmpdir! ::uberboot)
          faot (io/file pdir "src" "tailrecursion" "boot" "forceaot.clj")
          pclj (io/file pdir "project.clj")]
      ((sh "git" "clone" "git@github.com:tailrecursion/boot" (.getPath pdir)))
      (let [proj (->> pclj slurp read-string)
            head (take 3 proj)
            opts (->> proj (drop 3) (apply hash-map))
            excl {:uberjar-exclusions (or (core/get-env :uberjar-exclusions) [])}
            deps (into (:dependencies opts) (core/get-env :dependencies))
            keys (merge excl (assoc opts :dependencies deps))
            proj (concat head (mapcat identity keys))
            task (map ns-name (ns-requires 'tailrecursion.boot.user))
            aots (list 'ns 'tailrecursion.boot.forceaot
                   (list* :require task)
                   (list :gen-class))]
        (spit pclj (pp-str proj))
        (spit faot (pp-str aots))
        ((binding [*sh-dir* pdir] (sh "make" "boot")))
        (io/copy (io/file pdir "boot") (io/file (or outfile "boot")))))))

(defn- generate-lein-project-file!
  [& {:keys [keep-project] :or {:keep-project true}}]
  (let [pfile (io/file "project.clj")
        pname (or (core/get-env :project) 'boot-project)
        pvers (or (core/get-env :version) "0.1.0-SNAPSHOT")
        prop  #(when-let [x (core/get-env %2)] [%1 x])
        head  (list* 'defproject pname pvers
                     (concat
                      (prop :url :url)
                      (prop :license :license)
                      (prop :description :description)
                      [:dependencies (core/get-env :dependencies)
                       :source-paths (vec (core/get-env :src-paths))]))
        proj  (pp-str (concat head (mapcat identity (core/get-env :lein))))]
    (if-not keep-project (.deleteOnExit pfile))
    (spit pfile proj)))

(core/deftask lein-generate
  "Generate a leiningen `project.clj` file.

  This task generates a leiningen `project.clj` file based on the boot
  environment configuration, including project name and version (generated
  if not present), dependencies, and source paths. Additional keys may be added
  to the generated `project.clj` file by specifying a `:lein` key in the boot
  environment whose value is a map of keys-value pairs to add to `project.clj`."
  []
  (core/with-pre-wrap
    (generate-lein-project-file! :keep-project true)))

(core/deftask lein
  "Run a leiningen task with a generated `project.clj`.

  This task creates a temporary `project.clj` file based on the boot environment
  configuration, including project name and version (generated if not present),
  dependencies, and source paths. Additional keys may be added to the generated
  `project.clj` file by specifying a `:lein` key in the boot environment whose
  value is a map of keys-value pairs to add to `project.clj`.

  Note that leiningen is run in another process. This task cannot be used to run
  interactive lein tasks (yet) because stdin is not currently piped to leiningen."
  [& args]
  (core/with-pre-wrap
    (generate-lein-project-file! :keep-project false)
    ((apply sh "lein" (map str args)))))

(defn auto
  "Run every `msec` (default 200) milliseconds."
  [& [msec]]
  (fn [continue]
    (fn [event]
      (continue event)
      (Thread/sleep (or msec 200))
      (recur (core/make-event event)))))

(defn files-changed?
  [& [type]]
  (let [dirs      (remove core/tmpfile? (core/get-env :src-paths)) 
        watchers  (map file/make-watcher dirs)
        since     (atom 0)]
    (fn [continue]
      (fn [event]
        (let [clean #(assoc %2 %1 (set (remove core/ignored? (get %2 %1))))
              info  (->> (map #(%) watchers)
                      (reduce (partial merge-with set/union))
                      (clean :time)
                      (clean :hash))]
          (if-let [mods (->> (or type :time) (get info) seq)]
            (do
              (let [path  (file/path (first mods))
                    ok    "\033[34m↳ Elapsed time: %6.3f sec ›\033[33m 00:00:00 \033[0m"
                    fail  "\n\033[31m%s\033[0m\n\033[34m↳ Elapsed time: %6.3f sec ›\033[33m 00:00:00 \033[0m"]
                (when (not= 0 @since) (println)) 
                (reset! since (:time event))
                (print-time ok fail (continue (assoc event :watch info)))))
            (let [diff  (long (/ (- (:time event) @since) 1000))
                  pad   (apply str (repeat 9 "\b"))
                  s     (mod diff 60)
                  m     (mod (long (/ diff 60)) 60)
                  h     (mod (long (/ diff 3600)) 24)]
              (core/sync!)
              (printf "\033[33m%s%02d:%02d:%02d \033[0m" pad h m s))))))))

(core/deftask watch
  "Watch `:src-paths` and call its continuation when files change.

  The `:type`option specifies how changes to files are detected and can be
  either `:time`or `:hash` (default `:time`). The `:msec` option specifies the 
  polling interval in milliseconds (default 200)."
  [& {:keys [type msec] :or {type :time msec 200}}]
  (comp (auto msec) (files-changed? type)))

(core/deftask syncdir
  "Copy/sync files between directories.

  The files in `from-dirs` will be overlayed on the `to-dir` directory. Empty 
  directories are ignored."
  [to-dir & from-dirs]
  (core/add-sync! to-dir from-dirs)
  identity)

(defn- custom-eval [sym]
  `(do (when (try (require '~sym) ::ok (catch Throwable _#)) (in-ns '~sym))))


(core/deftask repl
  "Launch nrepl in the project.

  The optional `sym` argument specifies the namespace to be in."
  [& [sym]]
  (core/with-pre-wrap
    (require 'reply.main)
    ; resolve at runtime: reply takes like 4s to load via :require in ns decl
    (let [launch (resolve 'reply.main/launch-nrepl)]
      (assert launch "Can't resolve 'reply.main/launch-nrepl")
      (launch {:custom-eval (custom-eval (or sym 'tailrecursion.boot.user))}))))
