(import (liii check)
        (liii logging)
        (liii datetime))

;; Test timestamp function
(check-true (string? (timestamp)))
(check-true (>= ($ (timestamp) :length) 19)) ;; "YYYY-MM-DD HH:MM:SS" is at least 19 chars

;; Test that timestamp matches datetime format
(let ((now (datetime :now)))
  (check ($ (timestamp) :take 4 :get) => ($ (now 'year) :to-string)))

;; Test @apply: Verify that the same logger instance is returned for the same name
(let ((logger1 (logging "test-module"))
      (logger2 (logging "test-module")))
  (check-true (eq? logger1 logger2)))

;; Test @apply: Verify that different logger instances are returned for different names
(let ((logger1 (logging "module-a"))
      (logger2 (logging "module-b")))
  (check-false (eq? logger1 logger2)))

(check-false ((logging "app") :debug?))

(check-false ((logging "app") :info?))

(check-true ((logging "app") :warning?))

(check-true ((logging "app") :error?))

(check-true ((logging "app") :critical?))

