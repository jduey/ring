(ns ring.core
  (:use clojure.contrib.monads))

(def ring-m (state-t maybe-m))

(defmacro do-ring-m [& body]
  `(domonad ring-m
            ~@body))

(defn run-ring [handler request]
  (first (handler request)))
