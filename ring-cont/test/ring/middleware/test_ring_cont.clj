(ns ring.middleware.test-ring-cont
  (:use ring.middleware.ring-cont)
  (:use clojure.test
        clojure.contrib.monads
        ring.core))

(deftest test-law-1
         ;; (m-bind (m-result x) f) is equal to (f x)
           (with-monad session-m
                       (let [f (fn [v]
                                 (fn [c]
                                   (c (inc v))))]
                         (is (= 6 ((m-bind (m-result 5) f) identity)))
                         (is (= 6 ((f 5) identity))))))

(deftest test-law-2
         ;; (m-bind mv m-result) is equal to mv
         (with-monad session-m
                     (let [mv (m-result 10)]
                       (is (= 10 (mv identity)))
                       (is (= 10 ((m-bind mv m-result) identity))))))
         
(deftest test-law-3
         ;; (m-bind (m-bind mv f) g) is equal to (m-bind mv (fn [x] (m-bind (f x) g)))
         (with-monad session-m
                     (let [mv (m-result 2)
                           f (fn test3-f [v]
                               (fn [c]
                                 (c (inc v))))
                           g (fn test3-g [v]
                               (fn [c]
                                 (c (* 10 v))))]
                       (is (= 30 ((m-bind (m-bind mv f) g) identity)))
                       (is (= 30 ((m-bind mv (fn [x] (m-bind (f x) g))) identity))))))


(deftest test-law-4
         ;; (m-bind m-zero f) produces m-zero
         (with-monad session-m
                       (let [f (fn [v]
                                 (fn [c]
                                   (c (inc v))))]
                         (is (= nil ((m-bind m-zero f) identity))))))

(deftest test-law-5
         ;; (m-bind mv (fn [x] m-zero)) produces m-zero
         (with-monad session-m
                     (let [mv (fn [c]
                                (c 10))]
                       (is (= nil ((m-bind mv (fn [x] m-zero)) identity))))))
         
(deftest test-law-6
         ;; (m-plus mv m-zero) produces mv
         (with-monad session-m
                     (let [mv (fn [c]
                                (c 10))]
                       (is (= 10 ((m-plus mv m-zero) identity))))))
         
(deftest test-law-7
         ;; (m-plus m-zero mv) produces mv
         (with-monad session-m
                     (let [mv (fn [c]
                                (c 10))]
                       (is (= 10 ((m-plus m-zero mv) identity))))))

(deftest test-session-handler
  (let [sh (session-handler (fn [req]
                              (if (:v req)
                                [{:body "boo"} req]
                                nil)))]
    (is (= {:body "boo" :cookies {"session-id" "test-session"}}
           (ffirst (run-cont (sh {:v true :cookies {"session-id" "test-session"}})))))
    (is (= nil
           (ffirst (run-cont (sh {:cookies {"session-id" "test-session"}})))))))

(deftest test-m-chain
         (let [f (do-ring-m
                   [v (fetch-val :v)]
                   {:v (inc v)})
               sf (session-handler f)
               g (do-ring-m
                   [v (fetch-val :v)]
                   {:v (* 10 v)})
               sg (session-handler g)
               fg (with-monad session-m (m-chain [sf sg]))]
           (is (= 9 (:v (ffirst (run-cont (fg {:v 8}))))))
           (is (= 50 (:v (ffirst ((second (first (run-cont (fg {:v 8})))) {:v 5})))))))

#_(deftest test-handle-session-req
  (let [_ (dosync (ref-set sessions {}))
        sh (session-handler (fn [req]
                              (if (:v req)
                                [{:body "boo"} req]
                                [nil req])))
        [result request] ((handle-session-req sh) {:v true})
        result-session-id (get-in result [:cookies "session-id"])
        request-session-id (get-in request [:cookies "session-id"])]
    (is (= result-session-id request-session-id))
    (is (= {:body "boo" :cookies {"session-id" result-session-id}}
           result))
    (is (= [result-session-id]
           (keys @sessions)))))


(deftest test-session-choose
         (with-monad session-m
                     (let [_ (dosync (ref-set sessions {}))
                           f (session-handler (do-ring-m
                                                [v (fetch-val :v)]
                                                {:v (inc v)}))
                           g (session-handler (do-ring-m
                                                [v (fetch-val :v)
                                                 :when false]
                                                {:v :bogus}))
                           h (session-handler (do-ring-m
                                                [v (fetch-val :v)]
                                                {:v (* 10 v)}))
                           app (session-seq
                                 (session-choose (session-seq f g)
                                                 h)
                                 h)
                           [res1 _] ((handle-session-req app) {:v 1})
                           sess-id (get-in res1 [:cookies "session-id"])
                           [res2 _] ((handle-session-req app) {:v 3 :cookies {"session-id" sess-id}})
                           [res3 _] ((handle-session-req app) {:v 6 :cookies {"session-id" sess-id}})]
                       (is (= 2 (:v res1)))
                       (is (= 10 (:v res2)))
                       (is (= 60 (:v res3))))))

(deftest test-session-repeat
         (let [_ (dosync (ref-set sessions {}))
               f (session-handler (do-ring-m
                                    [v (fetch-val :v)
                                     :when (not= 3 v)]
                                    {:v (inc v)}))
               g (session-handler (do-ring-m
                                    [v (fetch-val :v)
                                     :when (not= 5 v)]
                                    {:v (+ 5 v)}))
               last-one (session-handler (do-ring-m
                                           [v (fetch-val :v)]
                                           {:v 47}))
               app (session-seq (session-repeat (session-seq f g))
                                last-one)
               [res1 _] ((handle-session-req app) {:v 0})
               sess-id (get-in res1 [:cookies "session-id"])
               [res2 _] ((handle-session-req app) {:v 23 :cookies {"session-id" sess-id}})
               [res3 _] ((handle-session-req app) {:v 10 :cookies {"session-id" sess-id}})
               [res4 _] ((handle-session-req app) {:v 8 :cookies {"session-id" sess-id}})
               [res5 _] ((handle-session-req app) {:v 3 :cookies {"session-id" sess-id}})]
           (is (= 1 (:v res1)))
           (is (= 28 (:v res2)))
           (is (= 11 (:v res3)))
           (is (= 13 (:v res4)))
           (is (= 47 (:v res5)))))

(run-tests)
