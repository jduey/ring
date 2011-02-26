(ns ring.middleware.reload
  "Reload namespaces before requests."
  (:use ring.core))

(defn reload
  "reload all symbols in 'reloadables' before proceeding"
  [reloadables]
  (fn [req]
    (doseq [ns-sym reloadables]
      (require ns-sym :reload))
    [true req]))

(defn wrap-reload
  "Wrap an app such that before a request is passed to the app, each namespace
  identified by syms in reloadables is reloaded.
  Currently this requires that the namespaces in question are being (re)loaded
  from un-jarred source files, as apposed to source files in jars or compiled
  classes."
  [app reloadables]
  (do-ring-m
    [_ (reload reloadables)
     resp app]
    resp))
