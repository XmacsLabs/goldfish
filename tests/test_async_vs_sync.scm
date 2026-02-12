; Demonstrate async vs sync HTTP requests
; This test shows that async requests run concurrently

(display "=== Comparing Sync vs Async HTTP ===")
(newline)
(newline)

; Helper to get current time
(define (current-time-seconds)
  (/ (g_monotonic-nanosecond) 1000000000.0))

; Helper to measure time
(define (time-elapsed start)
  (- (current-time-seconds) start))

; ============================================
; SYNC VERSION (using regular http-get)
; ============================================
(display "SYNC: Sequential requests")
(newline)

(define sync-start (current-time-seconds))

; Simulate two sync requests (we'll just sleep to show the point)
(display "  Sync: Starting request 1 (1 second)...")
(newline)
(g_sleep 1)  ; This blocks everything
(display "  Sync: Request 1 done")
(newline)

(display "  Sync: Starting request 2 (1 second)...")
(newline)
(g_sleep 1)  ; This also blocks
(display "  Sync: Request 2 done")
(newline)

(define sync-elapsed (time-elapsed sync-start))
(display "SYNC total time: ")
(display sync-elapsed)
(display " seconds")
(newline)
(newline)

; ============================================
; ASYNC VERSION (using async scheduler)
; ============================================
(display "ASYNC: Concurrent requests")
(newline)

(g_coroutine-scheduler-start 4)

(define async-start (current-time-seconds))
(define async-completed 0)

; Request 1: 1 second delay
(g_coroutine-run 
  (lambda ()
    (g_sleep 1)
    (set! async-completed (+ async-completed 1))
    (display "  Async: Request 1 done at ")
    (display (time-elapsed async-start))
    (display "s")
    (newline)))

; Request 2: 1 second delay (starts immediately, not waiting for request 1)
(g_coroutine-run 
  (lambda ()
    (g_sleep 1)
    (set! async-completed (+ async-completed 1))
    (display "  Async: Request 2 done at ")
    (display (time-elapsed async-start))
    (display "s")
    (newline)))

; Wait for both to complete
(let loop ((i 0))
  (g_coroutine-wait)
  (if (and (< i 20) (< async-completed 2))
    (begin
      (g_coroutine-sleep 0.1)
      (loop (+ i 1)))))

(define async-elapsed (time-elapsed async-start))
(display "ASYNC total time: ")
(display async-elapsed)
(display " seconds")
(newline)

(g_coroutine-scheduler-stop)

(newline)
(display "=== Results ===")
(newline)
(display "Sync time:  ")
(display sync-elapsed)
(display "s (expected ~2.0s)")
(newline)
(display "Async time: ")
(display async-elapsed)
(display "s (expected ~1.0s)")
(newline)
(display "Speedup:    ")
(display (/ sync-elapsed async-elapsed))
(display "x")
(newline)
