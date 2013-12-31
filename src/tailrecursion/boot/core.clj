; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns tailrecursion.boot.core
  (:refer-clojure :exclude [eval compile])
  (:require [cemerick.pomegranate           :as pom]
            [clojure.java.io                :as io]
            [clojure.set                    :refer [difference]]
            [clojure.string                 :refer [join]]
            [clojure.pprint                 :refer [pprint]]
            [tailrecursion.boot.deps        :as d]
            [tailrecursion.boot.tmpregistry :as tmp])
  (:import [java.net URLClassLoader URL]))

(declare analyze-task eval root-tasks prep-tasks lookup-env)

;; INTERNAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro guard [expr & [default]]
  `(try ~expr (catch Throwable _# ~default)))

(def user-ns (create-ns (symbol "tailrecursion.boot.user")))
(defn user-eval [expr] (binding [*ns* user-ns] (clojure.core/eval expr)))
(user-eval '(clojure.core/refer 'clojure.core))

(defn load-sym [sym]
  (when-let [ns (and sym (namespace sym))] (require (symbol ns)))
  (or (resolve sym) (assert false (format "Can't resolve #'%s." sym))))

(defn require-task [tasks [ns & {:keys [refer as]}]]
  {:pre [(symbol? ns)
         (or as refer)
         (or (nil? as) (symbol? as))
         (or (nil? refer) (= :all refer) (vector? refer))]}
  (require ns)
  (let [t?  #(-> % second meta :tailrecursion.boot.core/task)
        pub (->> ns ns-publics (filter t?) (into {}))
        mk  (fn [x] {:meta (meta x) :thunk (var-get x)})
        r   (when refer
              (->> pub
                (filter #(or (= :all refer) (contains? (set refer) (first %))))
                (map (fn [[k v]] [(keyword k) (mk v)])))) 
        a   (when as
              (->> pub
                (map (fn [[k v]] [(keyword (str as) (str k)) (mk v)]))))] 
    (merge (reduce into {} [r a]) tasks)))

(defn require-tasks! [clauses]
  (swap! root-tasks (partial reduce require-task) clauses))

(defn task-meta [props]
  (let [[op & args] (:main props)
        dfl-meta '{:arglists ([boot & args])
                   :doc "No documentation for this task."}]
    (merge-with #(or %2 %1)
      dfl-meta
      (if-not (and (symbol? op) (namespace op))
        {:doc (:doc props) :arglists (:arglists props)}
        (do (require (symbol (namespace op))) (meta (resolve op)))))))

(defn add-dependencies! [deps repos]
  (require 'tailrecursion.boot.loader)
  ((resolve 'tailrecursion.boot.loader/add-dependencies!) deps repos))

(defn add-directories! [dirs]
  (when (seq dirs)
    (let [meth  (doto (.getDeclaredMethod URLClassLoader "addURL" (into-array Class [URL]))
                  (.setAccessible true))
          cldr  (ClassLoader/getSystemClassLoader)
          urls  (->> dirs (map io/file) (filter #(.exists %)) (map #(.. % toURI toURL)))]
      (doseq [url urls] (.invoke meth cldr (object-array [url]))))))

(defn configure! [old new]
  (let [[nd od] [(:dependencies  new) (:dependencies  old)] 
        [ns os] [(:src-paths     new) (:src-paths     old)]
        [nr or] [(:require-tasks new) (:require-tasks old)]]
    (when-not (= nd od) (add-dependencies! nd (:repositories new)))
    (when-not (= ns os) (add-directories! (difference ns os)))
    (when-not (= nr or) (require-tasks! (difference nr or)))))

(defmacro dotmp [this & body] `(-> ~this (get-in [:system :tmpregistry]) ~@body))

;; BOOT EVALUATOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def root-tasks (atom {}))
(def prep-tasks (atom []))

(defmulti  analyze (fn [k v] k) :default ::default)
(defmethod analyze :require-tasks [k v] #(swap! % update-in [k] into v))
(defmethod analyze :repositories  [k v] #(swap! % update-in [k] into v))
(defmethod analyze :dependencies  [k v] #(swap! % update-in [k] into v))
(defmethod analyze :src-static    [k v] #(swap! % update-in [k] into v))
(defmethod analyze :src-paths     [k v] #(swap! % update-in [k] into v))
(defmethod analyze :require       [k v] (fn [_] (doseq [ns v] `(require '~ns))))
(defmethod analyze :tasks         [k v] (fn [_] (doseq [[t p] v] (analyze-task t p))))
(defmethod analyze ::default      [k v] #(swap! % assoc k v))

(defn analyze-task [task props]
  (let [preps [:src-paths :repositories :dependencies :require-tasks]]
    (->>
      (fn [env & args]
        (doseq [k preps] (when (contains? props k) ((analyze k (get props k)) env)))
        (doseq [[k v] (apply dissoc props :main :doc :arglists preps)] ((analyze k v) env))
        (if-let [main (:main props)] (eval env main args) (eval env [:do] nil)))
      (hash-map :meta (task-meta props) :thunk)
      (swap! root-tasks assoc task))))

(defmulti  lookup identity :default ::default)
(defmethod lookup :do       [op] (fn [env & args] (doseq [x args] (eval env x nil))))
(defmethod lookup ::default [op] (lookup-env op))

(defn lookup-env [op]
  (cond
    (symbol?  op) (load-sym op)
    (seq?     op) (user-eval op)
    (keyword? op) (:thunk (get @root-tasks op))))

(defn eval [env [op & args1] args2]
  (let [opfn (lookup op)
        args (if (seq args2) args2 args1)]
    (assert opfn (str "no such task (" op ")"))
    (swap! prep-tasks conj (apply opfn env args))))

(defn compile [boot & props]
  (let [mkkw #(keyword (gensym "tailrecursion.boot.core/task-"))]
    (loop [[p & q] props, expr [:do [(mkkw)]]]
      (analyze-task (first (last expr)) p)
      (if (seq q) (recur q (conj expr [(mkkw)])) (eval boot expr nil)))
    (apply comp (filter fn? @prep-tasks))))

;; BOOT API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ignored?  [this f]            ((get-in @this [:system :gitignore]) f))
(defn tmpfile?  [this f]            (dotmp @this (tmp/tmpfile? f)))
(defn mk!       [this key & [name]] (dotmp @this (tmp/mk! key name)))
(defn mkdir!    [this key & [name]] (dotmp @this (tmp/mkdir! key name)))
(defn unmk!     [this key]          (dotmp @this (tmp/unmk! key)) this)
(defn add-sync! [this dst & [srcs]] (dotmp @this (tmp/add-sync! dst srcs)) this)
(defn sync!     [this]              (dotmp @this (tmp/sync!)) this)
(defn deps      [this]              (d/deps this))

(defmacro deftask [name & args]
  `(defn ~(with-meta name {::task true}) ~@args))

(defn make-event
  ([boot]
   (make-event boot {}))
  ([boot event]
   (let [srcs    (->> @boot :src-paths (map io/file)
                      (mapcat file-seq) (filter #(.isFile %)))
         watched (->> srcs (remove (partial ignored? boot)) set)]
     (merge event {:id        (gensym)
                   :time      (System/currentTimeMillis)
                   :watch     {:time watched, :hash watched}
                   :src-files (set srcs)}))))

(defn init! [env]
  (doto (atom env) (add-watch ::_ #(configure! %3 %4))))

(defn create-app! [boot & props]
  (let [app (apply compile boot props)
        tmp (get-in @boot [:system :tmpregistry])]
    (when (and (:public @boot) (seq (:src-static @boot)))
      (add-sync! boot (:public @boot) (map io/file (:src-static @boot))))
    (app #(do (tmp/sync! tmp) (flush) %))))
