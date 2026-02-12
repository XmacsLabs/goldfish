; Demonstrate REAL async HTTP (I/O runs in background, not Scheme code)
; This shows that HTTP requests are truly concurrent

(display "=== Real Async HTTP Test ===")
(newline)
(newline)

; Helper to get current time
(define (current-time-seconds)
  (/ (g_monotonic-nanosecond) 1000000000.0))

(define (time-elapsed start)
  (- (current-time-seconds) start))

(g_coroutine-scheduler-start 4)

(define start-time (current-time-seconds))
(define responses-received 0)

; Request 1: Fast endpoint (~0.3s)
(display "T+0s: Sending fast request (httpbin.org/get)...")
(newline)
(g_http-async-get
  "https://httpbin.org/get"
  '()
  (lambda (response)
    (display "T+")
    (display (time-elapsed start-time))
    (display "s: FAST response received! Status: ")
    (display (hash-table-ref response "status-code"))
    (newline)
    (set! responses-received (+ responses-received 1))))

; Request 2: Slow endpoint (2 second delay)
(display "T+0s: Sending slow request (httpbin.org/delay/2)...")
(newline)
(g_http-async-get
  "https://httpbin.org/delay/2"
  '()
  (lambda (response)
    (display "T+")
    (display (time-elapsed start-time))
    (display "s: SLOW response received! Status: ")
    (display (hash-table-ref response "status-code"))
    (newline)
    (set! responses-received (+ responses-received 1))))

; Request 3: Medium endpoint (1 second delay)
(display "T+0s: Sending medium request (httpbin.org/delay/1)...")
(newline)
(g_http-async-get
  "https://httpbin.org/delay/1"
  '()
  (lambda (response)
    (display "T+")
    (display (time-elapsed start-time))
    (display "s: MEDIUM response received! Status: ")
    (display (hash-table-ref response "status-code"))
    (newline)
    (set! responses-received (+ responses-received 1))))

; Main loop waiting for responses
(newline)
(display "Main thread: Waiting for responses...")
(newline)

(let loop ((i 0))
  (g_coroutine-wait)
  (display "T+")
  (display (time-elapsed start-time))
  (display "s: Main thread check, responses so far: ")
  (display responses-received)
  (display "/3")
  (newline)

  (if (and (< i 30) (< responses-received 3))
    (begin
      (g_coroutine-sleep 0.3)
      (loop (+ i 1)))
    (begin
      (display "Done or timeout!")
      (newline))))

(define total-time (time-elapsed start-time))
(newline)
(display "=== Summary ===")
(newline)
(display "Total time: ")
(display total-time)
(display " seconds")
(newline)
(display "Expected if sequential: ~3.3 seconds")
(newline)
(display "Expected if concurrent: ~2.0 seconds (limited by slowest)")
(newline)
(display "Speedup vs sequential: ")
(display (/ 3.3 total-time))
(display "x")
(newline)

(g_coroutine-scheduler-stop)
