(ns pmbauer.boot.task.repl
  "Start a repl session with the current project."
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [tailrecursion.boot.core :as core]))

(def default-cfg
  "Default configuration is deep-merged with the options passed to repl task"
  {:host "127.0.0.1"
   :port 0
   :middlewares []
   :timeout 60000
   :init-ns 'user
   :repl-options {:history-file (str (io/file ".repl-history"))
                  :input-stream System/in}})

(defn deep-merge
  "Recursively merges maps. If vals are not maps, chooses last value"
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

(def ^:private nrepl-port-file (io/file ".nrepl-port"))

(def ^:private ack-server
  (delay ((resolve 'clojure.tools.nrepl.server/start-server)
          :bind (:host default-cfg)
          :handler ((resolve 'clojure.tools.nrepl.ack/handle-ack)
                    (resolve 'clojure.tools.nrepl.server/unknown-op)))))

(defn ^:private nrepl-started-msg
  [host port]
  (str "nREPL server started on port " port " on host " host
       " - nrepl://" host ":" port))

(defn ^:private start-server
  [{:as cfg
    :keys [host middlewares ack-port]}]
  (let [headless? (nil? ack-port)
        cfg (-> (set/rename-keys cfg {:host :bind})
                (assoc :handler (apply (resolve 'clojure.tools.nrepl.server/default-handler) middlewares))
                (select-keys [:bind :port :handler :ack-port]))
        {:as server :keys [port]} (->> (apply concat cfg)
                                       (apply (resolve 'clojure.tools.nrepl.server/start-server)))]
    (when headless?
      (println (nrepl-started-msg host port)))
    (spit (doto nrepl-port-file .deleteOnExit) port)
    @(promise)))

(defn ^:private start-server-in-thread
  [{:as cfg
    :keys [host timeout]}]
  ((resolve 'clojure.tools.nrepl.ack/reset-ack-port!))
  (let [ack-port (:port @ack-server)]
    (-> (bound-fn []
          (start-server (assoc cfg :ack-port ack-port)))
        (Thread.)
        (.start)))
  (if-let [repl-port ((resolve 'clojure.tools.nrepl.ack/wait-for-ack) timeout)]
    (do (println (nrepl-started-msg host repl-port))
        repl-port)
    (throw (ex-info "REPL server launch timed out." {}))))

(defn ^:private options-for-reply
  [opts & {:keys [attach port]}]
  (as-> opts opts
        (apply dissoc opts (concat [:init] (if attach [:host :port])))
        (merge opts (cond attach {:attach (str attach)}
                          port {:port port}
                          :else {}))
        (set/rename-keys opts {:prompt :custom-prompt
                               :welcome :custom-help})
        (if (:port opts) (update-in opts [:port] str) opts)))

(defn ^:private client
  [{:keys [repl-options]} attach]
  ((resolve 'reply.main/launch-nrepl)
   (options-for-reply repl-options :attach attach)))

(defn ^:private connect-string
  [opts]
  (as-> (str (first opts)) x
        (string/split x #":")
        (remove string/blank? x)
        (-> (drop-last (count x) [(:host default-cfg)
                                  (try (slurp nrepl-port-file)
                                       (catch Exception _ ""))])
            (concat x))
        (string/join ":" x)
        (if (re-find #":\d+($|/.*$)" x)
          x
          (throw (ex-info "Port is required" {:connect-string x})))))

(defn wrap-init-ns
  [init-ns]
  (with-local-vars
      [wrap-init-ns'
       (fn [h]
         ;; this needs to be a var, since it's in the nREPL session
         (with-local-vars [init-ns-sentinel nil]
           (fn [{:keys [session] :as msg}]
             (when-not (@session init-ns-sentinel)
               (swap! session assoc
                      (var *ns*)
                      (try (require init-ns) (create-ns init-ns)
                           (catch Throwable t (create-ns 'user)))
                      init-ns-sentinel true))
             (h msg))))]
    (doto wrap-init-ns'
      ;; set-descriptor! currently nREPL only accepts a var
      ((resolve 'clojure.tools.nrepl.middleware/set-descriptor!)
       {:requires #{(resolve 'clojure.tools.nrepl.middleware.session/session)}
        :expects #{"eval"}})
      (alter-var-root (constantly @wrap-init-ns')))))

(defn repl-cfg
  ([opts] (repl-cfg opts {}))
  ([opts event]
     (as-> (apply hash-map opts) cfg
           (deep-merge default-cfg (get event ::config {}) cfg)
           (update-in cfg [:middlewares] conj (wrap-init-ns (:init-ns cfg))))))

(defn headless
  [opts]
  (start-server (repl-cfg opts core/*event*)))

(def ^:private client-mode? (complement #{:headless :pass-through}))

(defn ^:private load-dynamic-dependencies
  [[cmd & opts :as args]]
  (core/set-env! :dependencies '[[org.clojure/tools.nrepl "0.2.3"]])
  (require 'clojure.tools.nrepl.ack)
  (require 'clojure.tools.nrepl.server)
  (when (client-mode? cmd)
    (core/set-env! :dependencies '[[reply "0.3.0"]])
    (require 'reply.main)))

(core/deftask repl
  "Start a repl session for the current project.

Subcommands:

<none> | [:host \"127.0.0.1\"] [:port random] [:middlewares []] [:init-ns 'user]

:headless [:host host] [:port port] [:middlewares []]
  This will launch an nREPL server and wait, rather than connecting
  a client to it.

:pass-through [:host host] [:port port] [:middlewares []]
  Like :headless, but does not block the task chain.

:connect [dest]
  Connects to an already running nREPL server. Dest can be:
  - host:port -- connects to the specified host and port;
  - port -- host defaults to localhost

  If no dest is given, resolves the host as described above
  and the port from .nrepl-port in the project root."
  [& [cmd & opts :as args]]
  (load-dynamic-dependencies args)
  (core/with-pre-wrap
    (condp = cmd
      :connect  (client default-cfg (connect-string opts))
      :headless (headless opts)
      :pass-through (future (headless opts))
      (let [cfg (repl-cfg args core/*event*)]
        (->> (start-server-in-thread cfg)
             (client cfg))))))
