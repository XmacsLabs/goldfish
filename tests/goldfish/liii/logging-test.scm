(import (liii check)
        (liii logging))

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

