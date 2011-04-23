(ns ring.middleware.ring-cont
  (:use clojure.contrib.monads
        [clojure.pprint :only [pprint]]
        ring.core
        [ring.middleware.cookies :only [get-cookies]])
  (:import [java.util UUID]))

; manage the history data structure so the browser back button works
(defn check-history [request]
  (get (get-in request [::session ::history])
       (get-in request [:cookies :screen-id])))

(defn update-history [request request-screen continuation]
  (if (nil? request-screen)
    request
    (merge-with merge request
                {::session {::history {request-screen continuation}}})))

(defn call-ring-handler [f request]
  (let [[result new-request] (f request)]
    (if (nil? result)
      [nil new-request]
      [result (update-history new-request
                              (get-in [:cookies :screen-id] request)
                              (get-in [::session ::current-handler] request))])))

#_(defn session-handler [f]
    (fn [request]
      (let [screen-handler (check-history request)
            [result new-request] (when (nil? screen-handler)
                                   (call-ring-handler f request))]
        (cond
          screen-handler (fn [continuation]
                           (screen-handler request))
          (nil? result) (m-result [[nil (get-in request [::session ::current-handler])]
                                   new-request])
          :else (fn [continuation]
                  [[(assoc-in result [:cookies "session-id"]
                              (get-in new-request [:cookies "session-id"]))
                    (fn handler [future-request]
                      (let [future-request (-> future-request
                                             (assoc ::session (::session new-request))
                                             (assoc-in [::session ::current-handler]
                                                       handler))]
                        (continuation future-request)))]
                   new-request])))))

(defmonad session-m
  [m-result   (fn [v]
                (fn [c]
                  (c v)))
   m-bind     (fn [mv f]
                (when mv
                  (fn [c]
                    (mv (fn [v]
                          ((f v) c))))))
   m-zero     (fn [x]
                  nil)
   m-plus     (fn session-m-plus [& mvs]
                (fn [req]
                  (->> mvs
                    (map #(% req))
                    (drop-while nil?)
                    first)
                  #_(if-let [result ((first mvs) req)]
                    result
                    ((apply session-m-plus (rest mvs)) req))))
   ])

; convert a request handler function into continuation passing style
; also implement the history functionality
(defn session-handler [rh]
  (fn [req]
    (let [r-r (rh req)
          alt-result (get-in req [::session :alternate-result])]
      (cond
        r-r (fn [c]
              (let [[result req] r-r
                    result (when result
                             (assoc-in result [:cookies "session-id"]
                                     (get-in req [:cookies "session-id"])))]
                (if result
                  [[result (fn handle-next-req [new-req]
                             (c (assoc new-req ::session (::session req))))]
                   req]
                  (c req))))
        alt-result (fn [c]
                     (alt-result req))
        :else (fn [c]
                nil)))))

(defn session-seq [& handlers]
  (fn [req]
    (fn [c]
      (let [seq-c (reduce (fn [next-c handler]
                            (fn inner-c [req]
                              ((handler req) next-c)))
                          c
                          (reverse handlers))]
      (seq-c req)))))

(defn session-repeat [sh]
  (fn repeated [req]
    (fn this-fn [c]
      (let [new-req (assoc-in req [::session :alternate-result]
                              (fn [req]
                                (c req)))]
        ((sh new-req) (fn [r]
                        ((repeated r) c)))))))

(defn session-choose [& handlers]
  (fn [req]
    (fn [c]
      (let [first-choice (reduce
                           (fn [alt-result handler]
                             (fn [_]
                               (let [req (assoc-in req
                                                   [::session :alternate-result]
                                                   alt-result)]
                                 ((handler req) c))))
                                 (get-in req [::session :alternate-result])
                                 (reverse handlers))]
        (first-choice nil)))))

(def sessions (ref {}))

(defn set-session [key value]
  (fn [req]
    [(get-in req [::session key]) (assoc-in req [::session key] value)]))

(defn get-session [key]
  (fn [req]
    [(get-in req [::session key]) req]))

(defn run-session [c]
  (c (fn session-runner [req]
       [nil req])))

(defn handle-session [session-id session-app]
  (fn [req]
    (if-let [session (and session-id (get @sessions session-id))]
      (session req)
      (-> req
        (assoc-in [:cookies "session-id"] (str (UUID/randomUUID)))
        (assoc ::session {})
        session-app
        run-session))))

; top level request handler
(defn handle-session-req [session-app]
  (do-ring-m
    [cookies (fetch-val :cookies)
     [result next-handler] (handle-session (get-in cookies ["session-id" :value])
                                           session-app)]
    (do
      (when-let [session-id (get-in result [:cookies "session-id"])]
        (dosync
          (ref-set sessions (assoc @sessions session-id next-handler))))
      result)))
