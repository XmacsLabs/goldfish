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

;;; New Option Implementation Benchmark
;;; Tests performance of inlet/varlet based option

(import (scheme time)
        (liii option))

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

(display "=== New Option Implementation Benchmark ===\n\n")

;; Test creation
(define new-creation-avg
  (test-and-print "option creation"
    (lambda () (run-multiple iterations (lambda () (option 42))))))

;; Test none
(define new-none-avg
  (test-and-print "none creation"
    (lambda () (run-multiple iterations (lambda () (none))))))

;; Test map
(define new-map-avg
  (test-and-print "map operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations (lambda () (opt :map (lambda (x) (* x 2)))))))))

;; Test filter
(define new-filter-avg
  (test-and-print "filter operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations (lambda () (opt :filter (lambda (x) (> x 20)))))))))

;; Test get
(define new-get-avg
  (test-and-print "get operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations (lambda () (opt :apply :get)))))))

;; Test get-or-else
(define new-get-or-else-avg
  (test-and-print "get-or-else operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations (lambda () (opt :get-or-else 0)))))))

;; Test equals
(define new-equals-avg
  (test-and-print "equals operation"
    (lambda () 
      (let ((opt1 (option 42))
            (opt2 (option 42)))
        (run-multiple iterations (lambda () (opt1 :equals opt2)))))))

;; Test contains
(define new-contains-avg
  (test-and-print "contains operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations (lambda () (opt :contains 42)))))))

;; Test chaining
(define new-chain-avg
  (test-and-print "chaining operation"
    (lambda () 
      (let ((opt (option 42)))
        (run-multiple iterations 
          (lambda () 
            (let ((mapped (opt :map (lambda (x) (* x 2)))))
              (mapped :filter (lambda (x) (> x 50))))))))))

(display "\n=== New Implementation Results ===\n")
(display* "Creation:     " new-creation-avg "ns\n")
(display* "None:         " new-none-avg "ns\n")
(display* "Map:          " new-map-avg "ns\n")
(display* "Filter:       " new-filter-avg "ns\n")
(display* "Get:          " new-get-avg "ns\n")
(display* "Get-or-else:  " new-get-or-else-avg "ns\n")
(display* "Equals:       " new-equals-avg "ns\n")  
(display* "Contains:     " new-contains-avg "ns\n")
(display* "Chaining:     " new-chain-avg "ns\n") 