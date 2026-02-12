; Test to verify concurrent execution using g_coroutine-sleep
; If truly concurrent, 3 x 0.5s sleeps should take ~0.5s total
; If sequential, it would take ~1.5s total

(display "=== Testing Concurrent Sleep ===")
(newline)

(define (current-time-seconds)
  (/ (g_monotonic-nanosecond) 1000000000.0))

(g_coroutine-scheduler-start 4)

(define start (current-time-seconds))
(define completed 0)

; Launch 3 concurrent tasks that each sleep 0.5 seconds
(display "Launching 3 tasks that each sleep 0.5s...")
(newline)

(g_coroutine-run 
  (lambda ()
    (g_coroutine-sleep 0.5)
    (set! completed (+ completed 1))
    (display "Task 1 done at T+")
    (display (- (current-time-seconds) start))
    (display "s")
    (newline)))

(g_coroutine-run 
  (lambda ()
    (g_coroutine-sleep 0.5)
    (set! completed (+ completed 1))
    (display "Task 2 done at T+")
    (display (- (current-time-seconds) start))
    (display "s")
    (newline)))

(g_coroutine-run 
  (lambda ()
    (g_coroutine-sleep 0.5)
    (set! completed (+ completed 1))
    (display "Task 3 done at T+")
    (display (- (current-time-seconds) start))
    (display "s")
    (newline)))

; Wait for all to complete
(let loop ()
  (g_coroutine-wait)
  (if (< completed 3)
    (begin
      (g_coroutine-sleep 0.05)
      (loop))))

(define elapsed (- (current-time-seconds) start))
(newline)
(display "Total time: ")
(display elapsed)
(display "s")
(newline)
(display "Expected (concurrent): ~0.5s")
(newline)
(display "Expected (sequential): ~1.5s")
(newline)

(if (< elapsed 1.0)
  (display "✓ Concurrent execution confirmed!")
  (display "✗ Sequential execution (expected with current implementation)"))
(newline)

(g_coroutine-scheduler-stop)
