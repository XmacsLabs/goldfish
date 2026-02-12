; Test to verify CPR thread pool limit
; This test shows that CPR limits concurrent requests to CPU core count

(display "=== Testing CPR Thread Pool Limit ===")
(newline)

(define (current-time-seconds)
  (/ (g_monotonic-nanosecond) 1000000000.0))

(define start-time (current-time-seconds))
(define responses-received 0)
(define total-requests 8)  ; Send more requests than typical CPU cores

(g_coroutine-scheduler-start 4)

(display "Sending ")
(display total-requests)
(display " concurrent requests to httpbin.org/delay/1...")
(newline)
(display "(If CPR uses CPU core count as thread pool size, only ~4 will be truly concurrent)")
(newline)
(newline)

; Send 8 requests, each taking 1 second
; If truly concurrent: total time ~1 second
; If CPR limited to 4 threads: total time ~2 seconds (4+4)

(let loop ((i 0))
  (if (< i total-requests)
    (begin
      (g_http-async-get 
        "https://httpbin.org/delay/1"
        '()
        (lambda (response)
          (set! responses-received (+ responses-received 1))
          (display "Request ")
          (display (+ i 1))
          (display " completed at T+")
          (display (- (current-time-seconds) start-time))
          (display "s")
          (newline)))
      (loop (+ i 1)))))

; Wait for all responses
(let loop ((i 0))
  (g_coroutine-wait)
  (if (and (< i 30) (< responses-received total-requests))
    (begin
      (g_coroutine-sleep 0.2)
      (loop (+ i 1)))))

(define total-time (- (current-time-seconds) start-time))

(newline)
(display "=== Results ===")
(newline)
(display "Total time: ")
(display total-time)
(display " seconds")
(newline)
(display "Requests completed: ")
(display responses-received)
(display "/")
(display total-requests)
(newline)
(newline)
(display "Analysis:")
(newline)
(display "- If ~1s:  CPR allows all 8 concurrent (unlikely)")
(newline)
(display "- If ~2s:  CPR limited to ~4 concurrent (4+4 batches)")
(newline)
(display "- If ~4s:  CPR limited to ~2 concurrent")
(newline)
(display "- If ~8s:  Sequential execution")
(newline)

(g_coroutine-scheduler-stop)
