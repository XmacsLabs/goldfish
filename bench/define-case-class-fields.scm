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

(import (liii timeit)
        (liii oop)
        (liii base))

(define (benchmark-define-case-class-fields)
  (display "=== Define-Case-Class Fields Construction Performance Test ===\n\n")

  ; Test with 0 fields (no methods)
  (define-case-class class-0-fields
    ())

  (let ((time (timeit (lambda () (class-0-fields)) :number 100000)))
    (display* "0 fields:\t\t\t" (number->string time) " seconds\n"))

  ; Test with 1 field
  (define-case-class class-1-field
    ((value any?)))

  (let ((time (timeit (lambda () (class-1-field 42)) :number 100000)))
    (display* "1 field:\t\t\t" (number->string time) " seconds\n"))

  ; Test with 2 fields
  (define-case-class class-2-fields
    ((value1 any?)
     (value2 any?)))

  (let ((time (timeit (lambda () (class-2-fields 42 43)) :number 100000)))
    (display* "2 fields:\t\t\t" (number->string time) " seconds\n"))

  ; Test with 4 fields
  (define-case-class class-4-fields
    ((value1 any?)
     (value2 any?)
     (value3 any?)
     (value4 any?)))

  (let ((time (timeit (lambda () (class-4-fields 42 43 44 45)) :number 100000)))
    (display* "4 fields:\t\t\t" (number->string time) " seconds\n"))

  ; Test with 8 fields
  (define-case-class class-8-fields
    ((value1 any?)
     (value2 any?)
     (value3 any?)
     (value4 any?)
     (value5 any?)
     (value6 any?)
     (value7 any?)
     (value8 any?)))

  (let ((time (timeit (lambda () (class-8-fields 42 43 44 45 46 47 48 49)) :number 100000)))
    (display* "8 fields:\t\t\t" (number->string time) " seconds\n"))

  ; Test with 16 fields
  (define-case-class class-16-fields
    ((value1 any?)
     (value2 any?)
     (value3 any?)
     (value4 any?)
     (value5 any?)
     (value6 any?)
     (value7 any?)
     (value8 any?)
     (value9 any?)
     (value10 any?)
     (value11 any?)
     (value12 any?)
     (value13 any?)
     (value14 any?)
     (value15 any?)
     (value16 any?)))

  (let ((time (timeit (lambda () (class-16-fields 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57)) :number 100000)))
    (display* "16 fields:\t\t\t" (number->string time) " seconds\n"))

  ; Test with 32 fields
  (define-case-class class-32-fields
    ((value1 any?)
     (value2 any?)
     (value3 any?)
     (value4 any?)
     (value5 any?)
     (value6 any?)
     (value7 any?)
     (value8 any?)
     (value9 any?)
     (value10 any?)
     (value11 any?)
     (value12 any?)
     (value13 any?)
     (value14 any?)
     (value15 any?)
     (value16 any?)
     (value17 any?)
     (value18 any?)
     (value19 any?)
     (value20 any?)
     (value21 any?)
     (value22 any?)
     (value23 any?)
     (value24 any?)
     (value25 any?)
     (value26 any?)
     (value27 any?)
     (value28 any?)
     (value29 any?)
     (value30 any?)
     (value31 any?)
     (value32 any?)))

  (let ((time (timeit (lambda () (class-32-fields 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73)) :number 100000)))
    (display* "32 fields:\t\t\t" (number->string time) " seconds\n"))

  (display "\n=== Field Access Performance Test ===\n\n")

  ; Test field access performance for each class
  (let ((obj-1 (class-1-field 42)))
    (let ((time (timeit (lambda () (obj-1 'value)) :number 100000)))
      (display* "1 field access:\t\t\t" (number->string time) " seconds\n")))

  (let ((obj-2 (class-2-fields 42 43)))
    (let ((time (timeit (lambda () (obj-2 'value1)) :number 100000)))
      (display* "2 fields - access value1:\t" (number->string time) " seconds\n"))
    (let ((time (timeit (lambda () (obj-2 'value2)) :number 100000)))
      (display* "2 fields - access value2:\t" (number->string time) " seconds\n")))

  (let ((obj-4 (class-4-fields 42 43 44 45)))
    (let ((time (timeit (lambda () (obj-4 'value4)) :number 100000)))
      (display* "4 fields - access value4:\t" (number->string time) " seconds\n")))

  (let ((obj-8 (class-8-fields 42 43 44 45 46 47 48 49)))
    (let ((time (timeit (lambda () (obj-8 'value8)) :number 100000)))
      (display* "8 fields - access value8:\t" (number->string time) " seconds\n")))

  (let ((obj-16 (class-16-fields 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57)))
    (let ((time (timeit (lambda () (obj-16 'value16)) :number 100000)))
      (display* "16 fields - access value16:\t" (number->string time) " seconds\n")))

  (let ((obj-32 (class-32-fields 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73)))
    (let ((time (timeit (lambda () (obj-32 'value32)) :number 100000)))
      (display* "32 fields - access value32:\t" (number->string time) " seconds\n")))

  (display "\n"))

; Run the benchmark
(benchmark-define-case-class-fields)