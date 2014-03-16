(ns tailrecursion.boot.warp
  "Conditionals in the exception dimension enabling faster-than-logic (FTL)
  travel.  Based on the Alcubierre drive."
  (:import java.util.WeakHashMap)
  (:require
   [clojure.stacktrace :as trace]))
 
(def ^:dynamic *e*)

(def ^:private thrown (WeakHashMap.))

(defn- do-trace
  ([handler]
     (do-trace handler nil))
  ([handler fmt & args]
     (when (bound? #'*e*)
       (let [trace (with-out-str (handler *e*))]
         (println (apply format (or fmt "%s") trace args))))))
 
(defn ex-info*
  ([] *e*)
  ([head & args]
     (let [info?        (map? head)
           info         (if info? head {})
           [fmt & args] (if info? args (cons head args))
           msg          (when fmt (apply format fmt args))]
       (ex-info msg info *e*))))

(defn throw*
  [& args]
  (let [ex (apply ex-info* args)]
    (.put @#'thrown ex true)
    (throw ex)))

(def print*       (partial do-trace trace/print-throwable))
(def print-root*  (partial do-trace (comp trace/print-throwable trace/root-cause)))
(def stack-trace* (partial do-trace trace/print-stack-trace))
(def cause-trace* (partial do-trace trace/print-cause-trace))
(def root-trace*  (partial do-trace (comp trace/print-stack-trace trace/root-cause)))

(defmacro try*
  [expr & [catch' finally']]
  `(try ~expr
        (catch Throwable e#
          (binding [*e* e#]
            (if (.get @#'thrown e#)
              (throw e#)
              ~catch')))
        (finally ~finally')))

(defmacro or*
  ([] nil)
  ([x] `(try* ~x))
  ([x & xs] `(try* ~x (or* ~@xs))))
