(import (liii check))

;; Simple test for stacktrace display on failure
(check-set-mode! 'report-failed)

;; Test basic failure
(check (+ 1 1) => 3)  ; Should show stacktrace

(check-report)