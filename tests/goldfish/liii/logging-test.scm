(import (liii check)
        (liii logging)
        (liii string)
        (liii lang))

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

;; Test logging with rich-string messages
(let ((log (logging "rich-string-test")))
  (log :set-level! 10) ;; DEBUG level
  
  ;; Test using $ to create a rich-string and logging it
  (define log-output (log :info ($ "User ID: " :+ 12345 :+ " logged in from " :+ "192.168.1.1")))
  (check-true (string-contains log-output "User ID: 12345 logged in from 192.168.1.1"))
  
  ;; Test for multi-parameters
  (define log-output2 (log :info "User ID: " "12345" " logged in from " "192.168.1.1"))
  (check-true (string-contains log-output2 "User ID: 12345 logged in from 192.168.1.1"))
  
  ;; Test with Unicode characters in rich-string
  (define unicode-msg ($ "用户: " :+ "admin" :+ " 登录成功 ✓"))
  (define log-output3 (log :error unicode-msg))
  (check-true (string-contains log-output3 " 登录成功 ✓")))

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
