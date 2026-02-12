; Test for coroutine support

(display "Testing coroutine support...")
(newline)

; Test 1: Start scheduler
(display "Test 1: Start scheduler with 4 threads... ")
(let ((result (g_coroutine-scheduler-start 4)))
  (display (if result "OK" "Already started"))
  (newline))

; Test 2: Run a simple coroutine
(display "Test 2: Run coroutines... ")
(let ((counter 0))
  (g_coroutine-run (lambda ()
    (set! counter (+ counter 1))
    (display "Task 1 done ")
    (newline)))
  (g_coroutine-run (lambda ()
    (set! counter (+ counter 1))
    (display "Task 2 done ")
    (newline)))
  (g_coroutine-run (lambda ()
    (set! counter (+ counter 1))
    (display "Task 3 done ")
    (newline)))
  (g_coroutine-wait)
  (display "All tasks completed, counter=")
  (display counter)
  (newline))

; Test 3: Yield
(display "Test 3: Yield test... ")
(g_coroutine-yield)
(display "OK")
(newline)

; Test 4: Sleep
(display "Test 4: Sleep test (0.1 seconds)... ")
(g_coroutine-sleep 0.1)
(display "OK")
(newline)

; Test 5: Stop scheduler
(display "Test 5: Stop scheduler... ")
(let ((result (g_coroutine-scheduler-stop)))
  (display (if result "OK" "Already stopped"))
  (newline))

(display "All coroutine tests passed!")
(newline)
