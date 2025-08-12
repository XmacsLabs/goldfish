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
                    (location (cadr (cadr result)))
                    (context (caddr (cadr result))))
                (let ((line (car location))
                      (col (cadr location)))
                  (cond
                    ((eq? error-type 'unclosed)
                     (cond
                       ((eq? context 'anonymous)
                        (display 
                         (string-append filename ": 
  ERROR: unclosed parenthesis in anonymous structure at line " (number->string line) ", column " (number->string col) "
")))
                       (else
                        (let ((symbol (car context))
                              (parent-line (cadr context))
                              (parent-col (caddr context)))
                          (display 
                           (string-append filename ": 
  ERROR: unclosed parenthesis in '" symbol "' structure at line " (number->string line) ", column " (number->string col) "
"))))))
                    ((eq? error-type 'unmatched-close) 
                     (cond
                       ((eq? context 'anonymous)
                        (display
                         (string-append filename ": 
  ERROR: unmatched parenthesis in anonymous structure at line " (number->string line) ", column " (number->string col) "
")))
                       (else
                        (let ((symbol (car context))
                              (parent-line (cadr context))
                              (parent-col (caddr context)))
                          (display
                           (string-append filename ": 
  ERROR: unmatched parenthesis for '" symbol "' structure at line " (number->string line) ", column " (number->string col) "
"))))))))))))
      (display "Usage: goldfish tools/lint.scm <filename>\n")))