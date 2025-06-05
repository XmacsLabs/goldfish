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

;;; Old Option Implementation Benchmark
;;; Tests performance of define-case-class based option

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

(define (test-and-print name test-proc)
  (display* "Testing " name "...\n")
  (let ((times '()))
    (let loop ((round 1))
      (when (<= round test-rounds)
        (display* "  Round " round "...")
        (let ((time (benchmark test-proc)))
          (set! times (cons time times)))
        (display "done\n")
        (loop (+ round 1))))
    (let ((avg (exact->inexact (/ (apply + times) (length times)))))
      (display* "  Average: " avg "ns\n")
      avg)))

(display "=== Old Option Implementation Benchmark ===\n\n")

;; Test creation
(define old-creation-avg
  (test-and-print "option creation"
    (lambda () (run-multiple iterations (lambda () (option 42))))))

;; Test none
(define old-none-avg
  (test-and-print "none creation"
    (lambda () (run-multiple iterations (lambda () (option '()))))))

;; Test map
(define old-map-avg
  (test-and-print "map operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations (lambda () (opt :map (lambda (x) (* x 2)))))))))

;; Test filter
(define old-filter-avg
  (test-and-print "filter operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations (lambda () (opt :filter (lambda (x) (> x 20)))))))))

;; Test get
(define old-get-avg
  (test-and-print "get operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations (lambda () (opt :get)))))))

;; Test get-or-else
(define old-get-or-else-avg
  (test-and-print "get-or-else operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations (lambda () (opt :get-or-else 0)))))))

;; Test equals
(define old-equals-avg
  (test-and-print "equals operation"
    (lambda () 
      (let ((opt1 (option 42))
            (opt2 (option 42)))
        (run-multiple iterations (lambda () (opt1 :equals opt2)))))))

;; Test chaining
(define old-chain-avg
  (test-and-print "chaining operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations 
          (lambda () 
            ((opt :map (lambda (x) (* x 2))) 
             :filter (lambda (x) (> x 50)))))))))

(display "\n=== Old Implementation Results ===\n")
(display* "Creation:     " old-creation-avg "ns\n")
(display* "None:         " old-none-avg "ns\n")
(display* "Map:          " old-map-avg "ns\n")
(display* "Filter:       " old-filter-avg "ns\n")
(display* "Get:          " old-get-avg "ns\n")
(display* "Get-or-else:  " old-get-or-else-avg "ns\n")
(display* "Equals:       " old-equals-avg "ns\n")  
(display* "Chaining:     " old-chain-avg "ns\n") 