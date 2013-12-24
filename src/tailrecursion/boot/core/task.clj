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
   [clojure.java.io                :refer [copy delete-file file resource]]
   [clojure.pprint                 :refer [pprint print-table with-pprint-dispatch code-dispatch]]
   [clojure.string                 :refer [split join blank?]]
   [tailrecursion.boot.table.core  :refer [table]]
   [tailrecursion.boot.core        :refer [deftask mkdir!]]
   [ancient-clj.core               :refer [latest-version-string!]]))

(defn first-line [s] (when s (first (split s #"\n"))))
(defn not-blank? [s] (when-not (blank? s) s))

(defn get-doc [sym]
  (when (symbol? sym)
    (when-let [ns (namespace sym)] (require (symbol ns))) 
    (join "\n" (-> sym find-var meta :doc str (split #" *\n *")))))

(defn print-tasks [tasks]
  (let [get-task  #(-> % str (subs 1))
        get-desc  #(or (-> % :doc first-line not-blank?)
                       (-> % :main first get-doc first-line))
        get-row   (fn [[k v]] [(get-task k) (get-desc v)])]
    (with-out-str (table (into [["" ""]] (map get-row tasks)) :style :none))))

(defn pad-left [thing lines]
  (let [pad (apply str (repeat (count thing) " "))
        pads (concat [thing] (repeat pad))]
    (join "\n" (map (comp (partial apply str) vector) pads lines))))

(defn version-info []
  (let [[_ proj vers & kvs]
        (try (read-string (slurp (resource "project.clj")))
          (catch Throwable _))
        {desc :description url :url lic :license}
        (into {} (map (partial apply vector) (partition 2 kvs)))]
    {:proj proj, :vers vers, :desc desc, :url url, :lic lic}))

(defn version-str []
  (let [{:keys [proj vers desc url lic]} (version-info)]
    (str (format "%s %s: %s\n" (name proj) vers url))))

(def base-artifacts '[tailrecursion/boot.core
                      tailrecursion/boot.task
                      tailrecursion/hoplon
                      org.clojure/clojurescript])

(defn latest-artifact-versions []
  (mapv #(vector % (latest-version-string! %)) base-artifacts))

(defn pprint-code [code]
  (with-pprint-dispatch code-dispatch (pprint code)))

(def boot-init-content
  (-> {:dependencies (latest-artifact-versions)
       :require-tasks '#{[tailrecursion.boot.task :refer :all]
                         [tailrecursion.hoplon.boot :refer :all]}
       :public "resources/public"
       :src-paths #{"src"}}
      pprint-code
      with-out-str))

(defn dir? [f] (.isDirectory f))

(defn file-exists? [f] (.exists f))

(defn mkdir-p [& paths]
  (.mkdirs (apply file paths)))

(def hello-world-content
  (str
   "(page \"index.html\")\n\n"
   "(html\n  (head)\n  (body\n    (h1 \"Hello, world!\")))"))

(defn make-hello-world [& path-pieces]
  (spit (apply file path-pieces) hello-world-content))

(defn build-init-structure [target-dir]
  (let [base-path (.getCanonicalPath target-dir)]
    (mkdir-p base-path "src")
    (mkdir-p base-path "resources/public")
    (mkdir-p base-path "resources/assets/css")
    (make-hello-world base-path "src" "index.cljs.hl")))


;; CORE TASKS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask nop
  "Does nothing."
  [boot]
  (fn [continue] (fn [event] (continue event))))

(deftask help
  "Print this help info.
  
  Some things more..."
  ([boot] 
   (let [tasks (:tasks @boot)]
     (fn [continue]
       (fn [event]
         (printf "%s\n" (version-str))
         (-> ["boot task ..." "boot [task arg arg] ..." "boot [help task]"]
           (->> (pad-left "Usage: ") println))
         (printf "\n%s\n\n" (pad-left "Tasks: " (split (print-tasks tasks) #"\n")))
         (flush)
         (continue event)))))
  ([boot task]
   (let [main (get-in @boot [:tasks (keyword task) :main])]
     (fn [continue]
       (fn [event]
         (assert (and (seq main) (symbol? (first main)))) 
         (let [sym (first main)]
           (when [(symbol? sym)]
             (when-let [ns (namespace sym)] (require (symbol ns))) 
             (let [{args :arglists doc :doc} (meta (find-var sym))]
               (printf "%s\n%s\n%s\n  %s\n\n" (version-str) sym args doc)
               (flush)))) 
         (continue event))))))

(def ^:dynamic *sh-dir* nil)

(defn sh [& args]
  (let [opts (into [:redirect-err true] (when *sh-dir* [:dir *sh-dir*]))
        proc (apply sh/proc (concat args opts))]
    (future (sh/stream-to-out proc :out))
    #(.waitFor (:process proc))))

(defn pp-str [form]
  (with-out-str (pprint form)))

(deftask rebuild-boot
  "Rebuild boot with AOT compiled tasks.
  
  This task builds a new boot executable by cloning the master branch of the
  boot github repo and running `make boot` in that directory. If there is a
  boot.edn file in the current directory then any tasks that are specified
  there will be AOT compiled into the exe, which greatly reduces boot's startup
  time. This task can be combined with other tasks if those tasks have different
  dependencies to build optimized boot exe's for each use case.

  Note that AOT compilation may not work with certain dependencies. The
  ClojureScript compiler is a good example of this. These deps must be excluded
  from the uberjar. Add an :uberjar-exclusions key to boot.edn with a vector of
  regexes (matching paths to be excluded from the jar file).
  
  The new boot executable will be created either with the path specified by the
  outfile argument or at ./boot if outfile isn't provided."
  [boot & [outfile]]
  (let [pdir (mkdir! boot ::optimize)
        faot (file pdir "src" "tailrecursion" "boot" "forceaot.clj")
        pclj (file pdir "project.clj")]
    ((sh "git" "clone" "git@github.com:tailrecursion/boot" (.getPath pdir)))
    (let [proj (->> pclj slurp read-string)
          head (take 3 proj)
          opts (->> proj (drop 3) (apply hash-map))
          excl {:uberjar-exclusions (or (:uberjar-exclusions @boot) [])}
          deps (into (:dependencies opts) (:dependencies @boot))
          keys (merge excl (assoc opts :dependencies deps))
          proj (concat head (mapcat identity keys))
          task (map first (:require-tasks @boot))
          aots (list 'ns 'tailrecursion.boot.forceaot
                 (list* :require task)
                 (list :gen-class))]
      (spit pclj (pp-str proj))
      (spit faot (pp-str aots))
      ((binding [*sh-dir* pdir] (sh "make" "boot")))
      (copy (file pdir "boot") (file (or outfile "boot"))))
    identity))

(deftask lein
  "Run a leiningen task.

  This task creates a temporary project.clj file based on the project's boot.edn
  configuration, including project name and version (generated if not present in
  boot.edn) and dependencies. Additional keys may be added to the project.clj
  file by specifying a :lein key in boot.edn associated with a map of keys and
  values.

  Note that leiningen is run in another process. This task cannot be used to run
  interactive lein tasks because stdin is not piped to the leiningen process."
  [boot & args]
  (let [pfile (file "project.clj")
        pname (or (:project @boot) 'boot-project)
        pvers (or (:version @boot) "0.1.0-SNAPSHOT")
        head  (list 'defproject pname pvers
                :dependencies (:dependencies @boot)
                :source-paths (vec (:src-paths @boot)))]
    (assert (not (.exists pfile)) "A project.clj file already exists.")
    (.deleteOnExit pfile)
    (spit pfile (pp-str (concat head (mapcat identity (:lein @boot)))))
    (fn [continue]
      (fn [event]
        ((apply sh "lein" (map str args)))
        (continue event)))))

(deftask init
  "Creates a simple boot.edn at the directory specified. Without
  arguments, creates a boot.edn file in the current working
  directory."
  ([] (init "."))
  ([target-dir]
     (let [target-dir (file (.getCanonicalPath target-dir))
           boot-file (file target-dir "boot.edn")]
       (if (and (dir? target-dir)
                ((complement file-exists?) boot-file))
         (do (spit boot-file boot-init-content)
             (build-init-structure target-dir))
         (println "The boot file" (str boot-file) "already exists or the path you specified is not a directory.")))))
