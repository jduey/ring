(ns ring.middleware.ring-cont
  (:use clojure.contrib.monads
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
                    result (assoc-in result [:cookies "session-id"]
                                     (get-in req [:cookies "session-id"]))]
                [[result (fn [new-req]
                           (c (assoc new-req ::session (::session req))))]
                 req]))
        alt-result (fn [c]
                      (alt-result req))
        :else (fn [c]
                nil)))))

; repeat a session handler until it returns nil
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
      (let [first-choice (reduce (fn [alt-result handler]
                                   (fn [_]
                                     ((handler (assoc-in req [::session :alternate-result]
                                                         alt-result))
                                        c)))
                                 (get-in req [::session :alternate-result])
                                 (reverse handlers))]
        (first-choice nil)))))

(with-monad session-m
            ; compose session handlers sequentially
            (defn session-seq [& handlers]
              (m-chain handlers))

            ; the conditional construct
            #_(defn web-cond [& preds-conts]
              (fn [[context request]]
                (let [pairs (partition 2 preds-conts)
                      successes (filter (fn [[pred c]]
                                          (or (= pred :else)
                                              (pred (:app-context context) request)))
                                        pairs)
                      cond-c (second (first successes))]
                  (if (nil? cond-c)
                    (m-result [context request])
                    (cond-c [context request])))))

            ; the 'loop while condition is true' construct
            #_(defn web-while [pred while-c]
              (fn this-fn [[context request]]
                (if (pred (:app-context context) request)
                  (m-bind (while-c [context request])
                           this-fn)
                  (m-result [context request]))))

            ; the 'loop until a condition is true' construct
            #_(defn web-until [pred until-c]
              (web-seq until-c
                       (web-while (complement pred)
                                  until-c))))

; macro to define a composable web request handler function
#_(defmacro web-fn [fn-name parms & body]
  `(def ~fn-name
     (session-handler (fn ~parms
                        ~@body))))

(def sessions (ref {}))

(defn handle-session [session-id session-app]
  (fn [req]
    (if-let [session (and session-id (get @sessions session-id))]
      (let [result (session req)]
        result)
      (-> req
        (assoc-in [:cookies "session-id"] (str (UUID/randomUUID)))
        (assoc ::session {})
        session-app
        run-cont))))

; top level request handler
(defn handle-session-req [session-app]
  (do-ring-m
    [{session-id "session-id"} (fetch-val :cookies)
     [result next-handler] (handle-session session-id session-app)]
    (do
      (when-let [session-id (get-in result [:cookies "session-id"])]
        (dosync
          (ref-set sessions (assoc @sessions
                                   session-id next-handler))))
      result)))
