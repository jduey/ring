(ns ring.middleware.file
  "Static file serving."
  (:use clojure.contrib.monads
        ring.core
        ring.router)
  (:require [ring.util.codec :as codec]
            [ring.util.response :as response])
  (:import java.io.File))

(defn- ensure-dir
  "Ensures that a directory exists at the given path, throwing if one does not."
  [^String dir-path]
  (let [dir (File. dir-path)]
    (if-not (.exists dir)
      (throw (Exception. (format "Directory does not exist: %s" dir-path))))))

(defn static-file [^String root-path & [opts]]
  (let [opts (merge {:root root-path, :index-files? true} opts)]
    (ensure-dir root-path)
    (do-ring-m
      [method GET
       uri (fetch-val :uri)
       :let [path (.substring (codec/url-decode uri) 1)
             resp (response/file-response path opts)]
       :when resp]
      resp)))

(defn wrap-file [app ^String root-path & [opts]]
  (with-monad ring-m
              (m-plus (static-file root-path opts)
                      app)))
