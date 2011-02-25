(ns ring.middleware.flash
  "A session-based flash store that persists to the next request."
  (:use clojure.contrib.monads
        ring.core))

(defn wrap-flash
  "If a :flash key is set on the response by the handler, a :flash key with
  the same value will be set on the next request that shares the same session.
  This is useful for small messages that persist across redirects."
  [handler]
  (do-ring-m
    [session (fetch-val :session)
     :let    [flash (:_flash session)]
     _       (set-val :flash flash)
     _       (set-val :session (dissoc session :_flash))
     response handler]
    (let [session (if (contains? response :session)
                        (response :session)
                        session)
              session (if-let [flash (response :flash)]
                        (assoc (response :session session) :_flash flash)
                        session)]
          (if (or flash (response :flash) (contains? response :session))
            (assoc response :session session)
            response))))
