(ns ring.middleware.file-test
  (:use clojure.test
        ring.middleware.file)
  (:import java.io.File))

(deftest wrap-file-no-directory
  (is (thrown-with-msg? Exception #".*Directory does not exist.*"
    (wrap-file (constantly :response) "not_here"))))

(def public-dir "test/ring/assets")
(def index-html (File. ^String public-dir "index.html"))
(def foo-html   (File. ^String public-dir "foo.html"))

(def app (wrap-file (fn [req] [:response req]) public-dir))

(deftest test-wrap-file-unsafe-method
  (is (= :response (first (app {:request-method :post :uri "/foo"})))))

(deftest test-wrap-file-forbidden-url
  (is (= :response (first (app {:request-method :get :uri "/../foo"})))))

(deftest test-wrap-file-no-file
  (is (= :response (first (app {:request-method :get :uri "/dynamic"})))))

(deftest test-wrap-file-directory
  (let [{:keys [status headers body]} (first (app {:request-method :get :uri "/"}))]
    (is (= 200 status))
    (is (= {} headers))
    (is (= index-html body))))

(deftest test-wrap-file-with-extension
  (let [{:keys [status headers body]} (first (app {:request-method :get :uri "/foo.html"}))]
    (is (= 200 status))
    (is (= {} headers))
    (is (= foo-html body))))

(deftest test-wrap-file-no-index
  (let [app  (wrap-file (fn [req] [:response req]) public-dir {:index-files? false})
        resp (first (app {:request-method :get :uri "/"}))]
    (is (= :response resp))))
