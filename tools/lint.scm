(import (liii sys)
        (liii path)
        (liii lint)
        (liii list))

(let ((args (argv)))
  (if (>= (length args) 3)
      (let ((filename (third args)))
        (let ((result (lint-check-brackets (path-read-text filename))))
          (if (eq? (car result) 'matched)
              (display (string-append filename ": OK (balanced parentheses)\n"))
              (let ((error-info (cadr result))
                    (error-type (caadr result))
                    (location (cadadr result)))
                (let ((line (car location))
                      (col (cadr location)))
                  (cond
                    ((eq? error-type 'unclosed)
                     (display 
                      (string-append filename ": 
  ERROR: unclosed parenthesis at line " (number->string line) ", column " (number->string col) "
  FIX: add closing parenthesis ')'
")))
                    ((eq? error-type 'unmatched-close) 
                     (display
                      (string-append filename ": 
  ERROR: unmatched parenthesis at line " (number->string line) ", column " (number->string col) "
  FIX: remove extraneous closing parenthesis ')'
")))))))))
      (display "Usage: goldfish tools/lint.scm <filename>\n")))