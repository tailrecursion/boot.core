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
   [clojure.pprint                 :refer [pprint print-table]]
   [clojure.string                 :refer [split join blank?]]
   [tailrecursion.boot.table.core  :refer [table]]
   [tailrecursion.boot.core        :refer [deftask mkdir!]]))

(defn first-line [s] (when s (first (split s #"\n"))))
(defn not-blank? [s] (when-not (blank? s) s))

(defn print-tasks [tasks]
  (comment
    (let [get-task  #(-> % str (subs 1))
          get-desc  #(->> % (get @root-tasks) :meta :doc first-line)
          get-row   (fn [x] [(get-task x) (get-desc x)])]
      (with-out-str (table (into [["" ""]] (map get-row tasks)) :style :none)))))

(defn pad-left [thing lines]
  (let [pad (apply str (repeat (count thing) " "))
        pads (concat [thing] (repeat pad))]
    (join "\n" (map (comp (partial apply str) vector) pads lines))))

(defn version-str [boot]
  (let [{:keys [proj vers description url license]} (:boot-version @boot)]
    (str (format "%s %s: %s\n" (name proj) vers url))))

;; CORE TASKS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftask nop
  "Does nothing."
  []
  (fn [continue] (fn [event] (continue event))))

(deftask help
  "Print this help info.
  
  Some things more..."
  ([]
     (comment
       (let [core? #(= "tailrecursion.boot.core" (namespace %))
             tasks (sort (remove core? (keys @root-tasks)))]
         (fn [continue]
           (fn [event]
             (printf "%s\n" (version-str boot))
             (-> ["boot task ..." "boot [task arg arg] ..." "boot [help task]"]
               (->> (pad-left "Usage: ") println))
             (printf "\n%s\n\n" (pad-left "Tasks: " (split (print-tasks tasks) #"\n")))
             (flush)
             (continue event)))))
     (println "== help ==")
     identity)
  ([task]
     (comment
       (let [task* (get @root-tasks (keyword task))]
         (assert task* (str "no such task (" task ")"))
         (let [{args :arglists doc :doc} (:meta task*)]
           (fn [continue]
             (fn [event]
               (printf "%s\n%s\n%s\n  %s\n\n" (version-str boot) task args doc)
               (flush) 
               (continue event))))))
     (println "== help 2 ==")
     identity))

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
  [& [outfile]]
  (comment
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
      identity)))

(deftask lein
  "Run a leiningen task.

  This task creates a temporary project.clj file based on the project's boot.edn
  configuration, including project name and version (generated if not present in
  boot.edn) and dependencies. Additional keys may be added to the project.clj
  file by specifying a :lein key in boot.edn associated with a map of keys and
  values.

  Note that leiningen is run in another process. This task cannot be used to run
  interactive lein tasks because stdin is not piped to the leiningen process."
  [& args]
  (comment
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
          (continue event))))))
