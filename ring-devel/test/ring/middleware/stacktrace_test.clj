(ns ring.middleware.stacktrace-test
  (:use clojure.test
        ring.middleware.stacktrace))

(def app (wrap-stacktrace (fn [_] (throw (Exception. "fail")))))

(def html-req {})
(def js-req   {:headers {"accept" "text/javascript"}})

(deftest wrap-stacktrace-smoke
  (binding [*err* (java.io.StringWriter.)]
    (let [{:keys [status headers] :as response} (first (app html-req))]
      (is (= 500 status))
      (is (= {"Content-Type" "text/html"} headers)))
    (let [{:keys [status headers]} (first (app js-req))]
      (is (= 500 status))
      (is (= {"Content-Type" "text/javascript"} headers)))))
