(ns ring.core
  (:use clojure.contrib.monads))

(def ring-m (state-t maybe-m))
