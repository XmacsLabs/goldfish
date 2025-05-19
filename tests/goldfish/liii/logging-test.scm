(import (liii check)
        (liii logging)
        (liii string))

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

;; Test that debug logging doesn't happen when level is too high
(let ((log (logging "high-level")))
  (log :set-level! 30) ;; WARNING level
  (check-false (log :debug?))
  (check-false (log :info?))
  (check-true (log :warning?))
  (check-true (log :error?))
  (check-true (log :critical?))
  
  ;; These shouldn't produce output
  (check (log :debug "This debug message shouldn't appear") => #<unspecified>)
  (check (log :info "This info message shouldn't appear") => #<unspecified>)
  
  ;; These should produce output
  (check-true (string-contains (log :warning "This warning should appear") "This warning should appear"))
  (check-true (string-contains (log :error "This error should appear") "This error should appear"))
  (check-true (string-contains (log :critical "This critical message should appear") "This critical message should appear")))

(check-report)
