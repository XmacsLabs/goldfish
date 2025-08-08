; <PLAINTEXT>
; Copyright (c) 2005-2006 Sebastian Egner.
; 
; Permission is hereby granted, free of charge, to any person obtaining
; a copy of this software and associated documentation files (the
; ''Software''), to deal in the Software without restriction, including
; without limitation the rights to use, copy, modify, merge, publish,
; distribute, sublicense, and/or sell copies of the Software, and to
; permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
; 
; -----------------------------------------------------------------------
; 
; Lightweight testing (reference implementation)
; ==============================================
;
; Sebastian.Egner@philips.com
; in R5RS + SRFI 23 (error) + SRFI 42 (comprehensions)
;
; history of this file:
;   SE, 25-Oct-2004: first version based on code used in SRFIs 42 and 67
;   SE, 19-Jan-2006: (arg ...) made optional in check-ec
;
; Naming convention "check:<identifier>" is used only internally.
;
; Copyright (c) 2024 The Goldfish Scheme Authors
; Follow the same License as the original one

(define-library (srfi srfi-78)
  (import (liii lang))
  (export check check-set-mode! check-report check-reset!
          check-passed? check-failed?
          check:proc)
  (begin

    (define check:write display*)

    (define check:mode #f)

    (define (check-set-mode! mode)
      (set! check:mode
            (case mode
              ((off)           0)
              ((summary)       1)
              ((report-failed) 10)
              ((report)        100)
              (else (error "unrecognized mode" mode)))))


    (check-set-mode! 'report)

    (define check:correct 0)
    (define check:failed '())

    (define (check-reset!)
      (set! check:correct 0)
      (set! check:failed '()))

    (define (check:add-correct!)
      (set! check:correct (+ check:correct 1)))

    (define (check:add-failed! expression actual-result expected-result file line call-stack)
      (set! check:failed
            (cons (list expression actual-result expected-result file line call-stack)
                  check:failed)))

    (define (check:get-source-location expr)
      (if (pair? expr)
          (list (or (pair-filename expr) "unknown")
                (or (pair-line-number expr) 0))
          (list "unknown" 0)))

    (define (check:get-stacktrace-safely)
      (let ((stack (stacktrace)))
        (if (and (string? stack) (> (string-length stack) 0))
            stack
            "[no stacktrace available]")))

    (define (check:report-expression expression)
      (newline)
      (check:write expression)
      (display " => "))

    (define (check:report-actual-result actual-result)
      (check:write actual-result)
      (display " ; "))

    (define (check:report-correct cases)
      (display "correct")
      (if (not (= cases 1))
          (begin (display " (")
                 (display cases)
                 (display " cases checked)")))
      (newline))

    (define (check:report-failed expected-result)
      (display "*** failed ***")
      (newline)
      (display "; expected result: ")
      (check:write expected-result)
      (newline))

    (define (report-location-and-stacktrace location-info call-stack)
      (unless (and (string=? "unknown" (car location-info))
                   (zero? (cadr location-info)))
        (display "; position: ")
        (display (car location-info))
        (display ":")
        (display (cadr location-info))
        (newline))
      (display "; stacktrace:")
      (newline)
      (display call-stack)
      (newline))

    (define (check-passed? expected-total-count)
      (and (= (length check:failed) 0)
           (= check:correct expected-total-count)))

    (define (check-failed?)
      (>= (length check:failed) 1))

    (define* (check:proc expression thunk expected-result (equal class=?))
      (let ((location-info (check:get-source-location expression)))
        (case check:mode
          ((0) #f)
          ((1)
           (let ((actual-result (thunk)))
             (if (equal actual-result expected-result)
                 (check:add-correct!)
                 (let ((call-stack (check:get-stacktrace-safely)))
                   (check:add-failed! expression actual-result expected-result
                                     (car location-info) (cadr location-info) call-stack)))))
          ((10)
           (let ((actual-result (thunk)))
             (if (equal actual-result expected-result)
                 (check:add-correct!)
                 (let ((call-stack (check:get-stacktrace-safely)))
                   (begin
                     (check:report-expression expression)
                     (check:report-actual-result actual-result)
                     (check:report-failed expected-result)
                     (report-location-and-stacktrace location-info call-stack)
                     (check:add-failed! expression actual-result expected-result
                                       (car location-info) (cadr location-info) call-stack))))))
          ((100)
           (check:report-expression expression)
           (let ((actual-result (thunk)))
             (check:report-actual-result actual-result)
             (if (equal actual-result expected-result)
                 (begin (check:report-correct 1)
                        (check:add-correct!))
                 (let ((call-stack (check:get-stacktrace-safely)))
                   (begin
                     (check:report-failed expected-result)
                     (report-location-and-stacktrace location-info call-stack)
                     (check:add-failed! expression 
                                        actual-result 
                                        expected-result
                                        (car location-info) (cadr location-info) call-stack))))))
          (else (error "unrecognized check:mode" check:mode)))))

    (define-macro (check expr => expected)
      `(check:proc ',expr (lambda () ,expr) ,expected))

    (define (check-report)
      (if (>= check:mode 1)
          (begin
            (newline)
            (display "; *** checks *** : ")
            (display check:correct)
            (display " correct, ")
            (display (length check:failed))
            (display " failed.")
            (newline))))

    ) ; end of begin
  ) ; end of define-library

