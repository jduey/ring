(ns ring.middleware.cookies-test
  (:use clojure.test
        clojure.contrib.monads
        ring.core
        ring.middleware.cookies))

(def cookies-wrapper (wrap-cookies (fetch-val :cookies)))

(deftest wrap-cookies-basic-cookie
  (is (= {"a" {:value "b"}} 
         (run-ring cookies-wrapper
                   {:headers {"cookie" "a=b"}}))))

(deftest wrap-cookies-multiple-cookies
  (is (= {"a" {:value "b"}, "c" {:value "d"}, "e" {:value "f"}}
         (run-ring cookies-wrapper
                   {:headers {"cookie" "a=b; c=d,e=f"}}))))

(deftest wrap-cookies-quoted-cookies
  (is (= {"a" {:value "b=c;e=f"}}
         (run-ring cookies-wrapper
                   {:headers {"cookie" "a=\"b=c;e=f\""}}))))

(deftest wrap-cookies-escaped-quotes
  (is (= {"a" {:value "\"b\""}}
         (run-ring cookies-wrapper
                   {:headers {"cookie" "a=\"\\\"b\\\"\""}}))))

(deftest wrap-cookies-extra-attrs
  (is (= {"a" {:value "b", :path "/", :domain "localhost"}}
         (run-ring cookies-wrapper
                   {:headers {"cookie" "a=b;$Path=\"/\";$Domain=localhost"}}))))

;; This test originally depended on the fact that there used to be
;; no difference between requests and responses. They were both
;; passed indiscriminately to the next stage. The call to 
;; fetch-state duplicates that behavior.
(deftest wrap-cookies-set-basic-cookie
   (is (= {:headers {"Set-Cookie" (list "a=b")}}
          (run-ring (wrap-cookies (fetch-state))
                    {:cookies {"a" "b"}}))))

(deftest wrap-cookies-set-multiple-cookies
  (is (= {:headers {"Set-Cookie" (list "a=b" "c=d")}}
         (run-ring (wrap-cookies (fetch-state))
                   {:cookies {"a" "b", "c" "d"}}))))

(deftest wrap-cookies-set-keyword-cookie
  (is (= {:headers {"Set-Cookie" (list "a=b")}}
         (run-ring (wrap-cookies (fetch-state))
                   {:cookies {:a "b"}}))))

(deftest wrap-cookies-set-extra-attrs
  (is (= {:headers {"Set-Cookie" (list "a=b;Path=/;Secure")}}
         (run-ring (wrap-cookies (fetch-state))
                   {:cookies {"a" {:value "b", :path "/", :secure true}}}))))

(deftest wrap-cookies-always-assocs-map
  (is (= {}
         (run-ring cookies-wrapper
                   {:headers {}}))))

(deftest wrap-cookies-read-urlencoded
  (is (= {"a" {:value "hello world"}} 
         (run-ring cookies-wrapper
                   {:headers {"cookie" "a=hello+world"}}))))

(deftest wrap-cookies-set-urlencoded-cookie
  (is (= {:headers {"Set-Cookie" (list "a=hello+world")}}
         (run-ring (wrap-cookies (fetch-state))
                   {:cookies {"a" "hello world"}}))))

(deftest wrap-cookies-keep-set-cookies-intact
  (is (= {:headers {"Set-Cookie" (list "a=b" "c=d")}}
         (run-ring (wrap-cookies (fetch-state))
                   {:headers {"Set-Cookie" (list "a=b")}
                    :cookies {:c "d"}}))))
