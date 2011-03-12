(ns ring.middleware.flash
  "A session-based flash store that persists to the next request."
  (:use clojure.contrib.monads
        ring.core))

(def get-flash
  (do-ring-m
    [{flash :_flash :as session} (fetch-val :session)
     _ (set-val :session (dissoc session :_flash))]
    flash))

(defn wrap-flash
  "If a :flash key is set on the response by the handler, a :flash key with
  the same value will be set on the next request that shares the same session.
  This is useful for small messages that persist across redirects."
  [handler]
  (do-ring-m
    [flash get-flash
     _ (set-val :flash flash)
     old-session (fetch-val :session)
     response handler]
    (let [response-flash (response :flash)
          new-session (get response :session old-session)
          new-session (if response-flash
                        (assoc new-session :_flash response-flash)
                        new-session)]
      (if (or flash response-flash)
        (assoc response :session new-session)
        response))))
