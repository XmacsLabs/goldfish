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

;;; New Rich-List Implementation Benchmark
;;; Tests performance of define-case-class-2 based rich-list

(import (scheme time)
        (liii lang_2))

(define iterations 1000)
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

(display "=== New Rich-List Implementation Benchmark ===\n\n")

;; Test creation from list
(define new-creation-avg
  (test-and-print "rich-list creation"
    (lambda () (run-multiple iterations (lambda () (rich-list '(1 2 3 4 5)))))))

;; Test static range
(define new-range-avg
  (test-and-print "range creation"
    (lambda () (run-multiple iterations (lambda () (rich-lists :range 1 100))))))

;; Test map
(define new-map-avg
  (test-and-print "map operation"
    (lambda () 
      (let ((lst (rich-lists :range 1 100)))
        (run-multiple iterations (lambda () (lst :map (lambda (x) (* x 2)))))))))

;; Test filter
(define new-filter-avg
  (test-and-print "filter operation"
    (lambda () 
      (let ((lst (rich-lists :range 1 100)))
        (run-multiple iterations (lambda () (lst :filter (lambda (x) (> x 50)))))))))

;; Test take
(define new-take-avg
  (test-and-print "take operation"
    (lambda () 
      (let ((lst (rich-lists :range 1 100)))
        (run-multiple iterations (lambda () (lst :take 10)))))))

;; Test head access
(define new-head-avg
  (test-and-print "head operation"
    (lambda () 
      (let ((lst (rich-lists :range 1 100)))
        (run-multiple iterations (lambda () (lst :apply :head)))))))

;; Test length
(define new-length-avg
  (test-and-print "length operation"
    (lambda () 
      (let ((lst (rich-lists :range 1 100)))
        (run-multiple iterations (lambda () (lst :apply :length)))))))

;; Test contains
(define new-contains-avg
  (test-and-print "contains operation"
    (lambda () 
      (let ((lst (rich-lists :range 1 100)))
        (run-multiple iterations (lambda () (lst :contains 50)))))))

;; Test collect
(define new-collect-avg
  (test-and-print "collect operation"
    (lambda () 
      (let ((lst (rich-lists :range 1 100)))
        (run-multiple iterations (lambda () (lst :apply :collect)))))))

;; Test chaining
(define new-chain-avg
  (test-and-print "chaining operation"
    (lambda () 
      (let ((lst (rich-lists :range 1 100)))
        (run-multiple iterations 
          (lambda () 
            (lst :map (lambda (x) (* x 2))
                 :filter (lambda (x) (> x 50))
                 :take 10
                 :apply :collect)))))))

;; Test fold
(define new-fold-avg
  (test-and-print "fold operation"
    (lambda () 
      (let ((lst (rich-lists :range 1 100)))
        (run-multiple iterations (lambda () (lst :fold 0 +)))))))

(display "\n=== New Implementation Results ===\n")
(display* "Creation:     " new-creation-avg "ns\n")
(display* "Range:        " new-range-avg "ns\n")
(display* "Map:          " new-map-avg "ns\n")
(display* "Filter:       " new-filter-avg "ns\n")
(display* "Take:         " new-take-avg "ns\n")
(display* "Head:         " new-head-avg "ns\n")
(display* "Length:       " new-length-avg "ns\n")
(display* "Contains:     " new-contains-avg "ns\n")
(display* "Collect:      " new-collect-avg "ns\n")
(display* "Chaining:     " new-chain-avg "ns\n")
(display* "Fold:         " new-fold-avg "ns\n") 