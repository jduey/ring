(ns ring.middleware.flash-test
  (:use clojure.test
        ring.middleware.flash))

(deftest flash-is-added-to-session
  (let [message  {:error "Could not save"}
        handler  (wrap-flash (fn [req]
                               [{:flash message} req]))
        response (first (handler {:session {}}))]
    (is (= (:session response) {:_flash message}))))

(deftest flash-is-retrieved-from-session
  (let [message  {:error "Could not save"}
        handler  (wrap-flash
                   (fn [request]
                     (is (= (:flash request) message))
                     [{} request]))]
    (handler {:session {:_flash message}})))

(deftest flash-is-removed-after-read
  (let [message  {:error "Could not save"}
        handler  (wrap-flash (fn [req]
                               [{:session {:foo "bar"}} req]))
        response (first (handler {:session {:_flash message}}))]
    (is (nil? (:flash response)))
    (is (= (:session response) {:foo "bar"}))))

(deftest flash-doesnt-wipe-session
  (let [message  {:error "Could not save"}
        handler  (wrap-flash (fn [req]
                               [{:flash message} req]))
        response (first (handler {:session {:foo "bar"}}))]
    (is (= (:session response) {:foo "bar", :_flash message}))))

(deftest flash-overwrites-nil-session
  (let [message  {:error "Could not save"}
        handler  (wrap-flash (fn [req]
                               [{:flash message, :session nil} req]))
        response (first (handler {:session {:foo "bar"}}))]
    (is (= (:session response) {:_flash message}))))

(deftest flash-not-except-on-nil-response
  (let [handler (wrap-flash (constantly nil))]
    (is (nil? (handler {})))))
