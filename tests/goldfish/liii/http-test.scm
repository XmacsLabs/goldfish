(import (liii check)
        (liii http)
        (liii string)
        (liii rich-json))

(check-set-mode! 'report-failed)

(let1 r (http-head "https://httpbin.org")
  (check (r 'status-code) => 200)
  (check (r 'url) => "https://httpbin.org/")
  (check-true (real? (r 'elapsed)))
  ;; NOTE: httpbin.org's LB routes to different backends.
  ;;       Some return "OK", others empty string for reason.
  ;;       HTTP/2+ allows omitting reason phrases.
  (check-true (or (equal? (r 'reason) "OK")
                  (equal? (r 'reason) "")))
  (check (r 'text) => "")
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8")
  (check ((r 'headers) "content-length") => "9593")
  (check-true (http-ok? r)))

(let1 r (http-get "https://httpbin.org")
  (check (r 'status-code) => 200)
  (check-true (> (string-length (r 'text)) 0))
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8"))

(let1 r (http-get "https://httpbin.org/get"
                  :params '(("key1" . "value1") ("key2" . "value2")))
      (check-true (string-contains (r 'text) "value1"))
      (check-true (string-contains (r 'text) "value2"))
      (check (r 'url) => "https://httpbin.org/get?key1=value1&key2=value2"))

(let1 r (http-post "https://httpbin.org/post"
                  :params '(("key1" . "value1") ("key2" . "value2")))
      (check-true (string-contains (r 'text) "value1"))
      (check-true (string-contains (r 'text) "value2"))
      (check (r 'status-code) => 200)
      (check (r 'url) => "https://httpbin.org/post?key1=value1&key2=value2"))

(let* ((r (http-post "https://httpbin.org/post"
            :data "This is raw data"))
       (json (string->json (r 'text))))
  (display* (r 'text) "\n")
  (display* json "\n")
  (display* (json->string json) "\n")
  (check (r 'status-code) => 200)
  (check (json-ref json "data") => "This is raw data"))

;; Streaming HTTP tests

;; Test streaming GET with simple endpoint
(let ((collected '())
      (userdata-received #f)
      (userdata-expected '("streaming" test "param")))
  (http-stream-get "https://httpbin.org/get"
                   (lambda (chunk userdata)
                     (display userdata)
                     (newline)
                     (set! userdata-received userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))))
                   userdata-expected
                   '(("query" . "test_values") ("limit" . "10")))
  (check-true (> (length collected) 0))
  (check userdata-received => userdata-expected))

;; Test streaming GET with JSON endpoint
(let1 collected '()
  (http-stream-get "https://jsonplaceholder.typicode.com/posts/1"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected)))))
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "userId"))))

;; Test streaming POST with JSON data
(let1 collected '()
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))))
                   '()
                   '(("param1" . "value1"))
                   "{\"test\": \"streaming-json\"}"
                   '(("Content-Type" . "application/json")))
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "streaming-json"))))

;; Test streaming POST with plain text
(let1 collected '()
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))))
                   '()
                   '()
                   "Simple streaming POST test")
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "Simple streaming POST test"))))

;; Test streaming POST with XML data
(let1 collected '()
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))))
                   '()
                   '()
                   "<root><message>stream-xml-test</message></root>"
                   '(("Content-Type" . "application/xml")))
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "stream-xml-test"))))

;; Test streaming POST with form data
(let1 collected '()
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))))
                   '()
                   '()
                   "field1=stream-test&field2=form-data"
                   '(("Content-Type" . "application/x-www-form-urlencoded")))
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "stream-test"))))

(check-report)

