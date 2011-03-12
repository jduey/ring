(ns ring.router
  (:use clojure.contrib.monads
        ring.core))

(defn request-method [req]
  "Retrieve the request method (GET, PUT, POST, DELETE)
  from the request."
  [(:request-method req) req])

(def GET
  (do-ring-m
    [method request-method
     :when (= :get method)]
    :get))

(def PUT
  (do-ring-m
    [method request-method
     :when (= :put method)]
    :put))

(def POST
  (do-ring-m
    [method request-method
     :when (= :post method)]
    :post))

(def DELETE
  (do-ring-m
    [method request-method
     :when (= :delete method)]
    :delete))

(defn any-uri-char [req]
  (let [uri (if-let [uri (::remaining-uri req)]
              uri
              (:uri req))]
    (when (> (count uri) 0)
      [(first uri)
       (assoc req ::remaining-uri (.substring uri 1))])))

(defn test-uri-char [pred]
  (do-ring-m
    [c any-uri-char
     :when (pred c)]
    c))

(defn is-uri-char [c]
  (test-uri-char c))

(def uri-path-sep
  (test-uri-char \/))

(defn uri-is [uri-str]
  (fn [req]
    (let [uri (if-let [uri (::remaining-uri req)]
                uri
                (:uri req))]
      (when (= uri uri-str)
        [uri-str
         (assoc req ::remaining-uri
                (.substring uri (count uri-str)))]))))

(defn uri-starts-with [pref-str]
  (fn [req]
    (let [uri (if-let [uri (::remaining-uri req)]
                uri
                (:uri req))]
      (when (.startsWith uri pref-str)
        [pref-str
         (assoc req ::remaining-uri
                (.substring uri (count pref-str)))]))))

(with-monad ring-m
            (defn optional [parser]
              (m-plus parser (m-result nil)))

            (def match-first m-plus)

            (defn match-all [& parsers]
              (m-fmap (partial apply str)
                      (m-seq parsers)))

            (def one-or-more)

            (defn none-or-more [parser]
              (optional (one-or-more parser)))

            (defn one-or-more [parser]
              (domonad
                [a parser
                 as (none-or-more parser)]
                (str a as)))

            (defn one-of [target-strn]
              (let [str-chars (into #{} target-strn)]
                (test-uri-char #(contains? str-chars %))))

            (def alpha (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
            (def whitespace (one-of " \t\n\r"))
            (def digit (one-of "0123456789")))
