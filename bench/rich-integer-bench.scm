;
; Copyright (C) 2025 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

;;; Rich Integer Performance Benchmark
;;; Uses alternating tests across multiple rounds to minimize external interference

(import (scheme time)
        (liii lang))

(define iterations 10000)
(define test-rounds 5)

(define (benchmark thunk)
  (let* ((start (current-jiffy))
         (result (thunk))
         (end (current-jiffy)))
    (- end start)))

(define (run-multiple times proc)
  (when (> times 0)
    (proc)
    (run-multiple (- times 1) proc)))

(define (average lst)
  (exact->inexact (/ (apply + lst) (length lst))))

(define (run-benchmark-rounds name-old old-thunk name-new new-thunk)
  (display* "Testing " name-old " vs " name-new "...\n")
  (let ((old-times '())
        (new-times '()))
    
    ;; Run multiple rounds, alternating old and new implementations
    (let loop ((round 1))
      (when (<= round test-rounds)
        (display* "  Round " round "...")
        
        ;; Test old implementation
        (let ((old-time (benchmark old-thunk)))
          (set! old-times (cons old-time old-times)))
        
        ;; Test new implementation  
        (let ((new-time (benchmark new-thunk)))
          (set! new-times (cons new-time new-times)))
        
        (display "done\n")
        (loop (+ round 1))))
    
    ;; Calculate averages
    (let ((old-avg (average old-times))
          (new-avg (average new-times)))
      (display* "  " name-old " average: " old-avg "ns\n")
      (display* "  " name-new " average: " new-avg "ns\n")
      (display* "  Performance: " 
                (if (< new-avg old-avg) "NEW FASTER" "OLD FASTER")
                " by " (abs (- new-avg old-avg)) "ns"
                " (ratio: " (number->string (/ new-avg old-avg)) "x)\n\n")
      (list old-avg new-avg))))

(display "=== Rich Integer Benchmark (Multi-Round) ===\n\n")

;; Baseline performance
(display "--- Baseline Performance ---\n")
(let ((sqrt-times '())
      (tostring-times '()))
  ;; Run baseline tests multiple times
  (let loop ((i 1))
    (when (<= i test-rounds)
      (set! sqrt-times (cons (benchmark (lambda () (run-multiple iterations (lambda () (sqrt 16))))) sqrt-times))
      (set! tostring-times (cons (benchmark (lambda () (run-multiple iterations (lambda () (number->string 42))))) tostring-times))
      (loop (+ i 1))))
  
  (let ((sqrt-baseline (average sqrt-times))
        (tostring-baseline (average tostring-times)))
    (display* "sqrt(16) average: " sqrt-baseline "ns\n")
    (display* "number->string(42) average: " tostring-baseline "ns\n\n")))

;; Test sqrt method
(display "--- sqrt Method Comparison ---\n")
(define sqrt-results
  (run-benchmark-rounds
    "old rich-integer%sqrt"
    (lambda () (run-multiple iterations (lambda () ((rich-integer 16) :sqrt))))
    "new rich-integer%sqrt"
    (lambda () 
      (import (liii integer))
      (run-multiple iterations (lambda () ((rich-integer 16) :apply :sqrt))))))

;; Test to-string method
(display "--- to-string Method Comparison ---\n")
(define tostring-results
  (run-benchmark-rounds
    "old rich-integer%to-string"
    (lambda () (run-multiple iterations (lambda () ((rich-integer 42) :to-string))))
    "new rich-integer%to-string"
    (lambda () 
      (import (liii integer))
      (run-multiple iterations (lambda () ((rich-integer 42) :apply :to-string))))))

;; Test static methods
(display "--- Static Methods Comparison ---\n")
(define maxvalue-results
  (run-benchmark-rounds
    "old rich-integer@max-value"
    (lambda () (run-multiple iterations (lambda () (rich-integer :max-value))))
    "new rich-integers@max-value"
    (lambda () 
      (import (liii integer))
      (run-multiple iterations (lambda () (rich-integers :max-value))))))

(define minvalue-results
  (run-benchmark-rounds
    "old rich-integer@min-value"
    (lambda () (run-multiple iterations (lambda () (rich-integer :min-value))))
    "new rich-integers@min-value"
    (lambda () 
      (import (liii integer))
      (run-multiple iterations (lambda () (rich-integers :min-value))))))

;; Final summary
(display "=== Final Performance Summary ===\n")
(display* "sqrt:      " (if (< (cadr sqrt-results) (car sqrt-results)) "NEW WINS" "OLD WINS") 
          " (" (number->string (/ (cadr sqrt-results) (car sqrt-results))) "x)\n")
(display* "to-string: " (if (< (cadr tostring-results) (car tostring-results)) "NEW WINS" "OLD WINS") 
          " (" (number->string (/ (cadr tostring-results) (car tostring-results))) "x)\n")
(display* "max-value: " (if (< (cadr maxvalue-results) (car maxvalue-results)) "NEW WINS" "OLD WINS") 
          " (" (number->string (/ (cadr maxvalue-results) (car maxvalue-results))) "x)\n")
(display* "min-value: " (if (< (cadr minvalue-results) (car minvalue-results)) "NEW WINS" "OLD WINS") 
          " (" (number->string (/ (cadr minvalue-results) (car minvalue-results))) "x)\n")

(display "\n=== Conclusion ===\n")
(display* "- Tests run across " test-rounds " rounds with alternating implementations\n")
(display "- Results averaged to minimize system interference\n")
(display "- New implementation uses modern let? architecture\n")
(display "- Architecture modernization provides better maintainability\n") 