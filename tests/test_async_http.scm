; Test for async HTTP support

(display "Testing async HTTP...")
(newline)

; Start scheduler
(g_coroutine-scheduler-start 4)
(display "Scheduler started")
(newline)

; Test async HTTP get
(display "Sending async HTTP request to httpbin.org...")
(newline)

(define responses-received 0)

(g_http-async-get
  "https://httpbin.org/get"
  '()
  (lambda (response)
    (display "Got response!")
    (newline)
    (display "Status code: ")
    (display (hash-table-ref response "status-code"))
    (newline)
    (set! responses-received (+ responses-received 1))))

(g_http-async-get
  "https://httpbin.org/delay/2"
  '()
  (lambda (response)
    (display "Got delayed response!")
    (newline)
    (display "Status code: ")
    (display (hash-table-ref response "status-code"))
    (newline)
    (set! responses-received (+ responses-received 1))))

; Wait for responses
(display "Waiting for responses...")
(newline)

(let loop ((i 0))
  (if (< i 10)
    (begin
      (g_coroutine-wait)
      (display "Waiting... ")
      (display i)
      (display " responses so far: ")
      (display responses-received)
      (newline)
      (g_coroutine-sleep 0.5)
      (loop (+ i 1)))
    (display "Timeout waiting for responses")))

(display "Total responses received: ")
(display responses-received)
(newline)

(g_coroutine-scheduler-stop)
(display "Scheduler stopped")
(newline)
