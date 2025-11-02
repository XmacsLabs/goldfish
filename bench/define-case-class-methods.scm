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

(define (benchmark-define-case-class)
  (display "=== Define-Case-Class Construction Performance Test ===\n\n")

  ; Test with 1 instance method
  (define-case-class class-1-method
    ((value any?))
    (define (%method1) value))

  (let ((time (timeit (lambda () (class-1-method 42)) :number 100000)))
    (display* "1 instance method:\t\t" (number->string time) " seconds\n"))

  ; Test with 2 instance methods
  (define-case-class class-2-methods
    ((value any?))
    (define (%method1) value)
    (define (%method2) (+ value 1)))

  (let ((time (timeit (lambda () (class-2-methods 42)) :number 100000)))
    (display* "2 instance methods:\t\t" (number->string time) " seconds\n"))

  ; Test with 4 instance methods
  (define-case-class class-4-methods
    ((value any?))
    (define (%method1) value)
    (define (%method2) (+ value 1))
    (define (%method3) (* value 2))
    (define (%method4) (- value 1)))

  (let ((time (timeit (lambda () (class-4-methods 42)) :number 100000)))
    (display* "4 instance methods:\t\t" (number->string time) " seconds\n"))

  ; Test with 8 instance methods
  (define-case-class class-8-methods
    ((value any?))
    (define (%method1) value)
    (define (%method2) (+ value 1))
    (define (%method3) (* value 2))
    (define (%method4) (- value 1))
    (define (%method5) (/ value 2.0))
    (define (%method6) (modulo value 10))
    (define (%method7) (expt value 2))
    (define (%method8) (sqrt (abs value))))

  (let ((time (timeit (lambda () (class-8-methods 42)) :number 100000)))
    (display* "8 instance methods:\t\t" (number->string time) " seconds\n"))

  ; Test with 16 instance methods
  (define-case-class class-16-methods
    ((value any?))
    (define (%method1) value)
    (define (%method2) (+ value 1))
    (define (%method3) (* value 2))
    (define (%method4) (- value 1))
    (define (%method5) (/ value 2.0))
    (define (%method6) (modulo value 10))
    (define (%method7) (expt value 2))
    (define (%method8) (sqrt (abs value)))
    (define (%method9) (log (+ value 1)))
    (define (%method10) (sin value))
    (define (%method11) (cos value))
    (define (%method12) (tan value))
    (define (%method13) (asin (/ value 100.0)))
    (define (%method14) (acos (/ value 100.0)))
    (define (%method15) (atan value))
    (define (%method16) (floor value)))

  (let ((time (timeit (lambda () (class-16-methods 42)) :number 100000)))
    (display* "16 instance methods:\t\t" (number->string time) " seconds\n"))

  ; Test with 32 instance methods
  (define-case-class class-32-methods
    ((value any?))
    (define (%method1) value)
    (define (%method2) (+ value 1))
    (define (%method3) (* value 2))
    (define (%method4) (- value 1))
    (define (%method5) (/ value 2.0))
    (define (%method6) (modulo value 10))
    (define (%method7) (expt value 2))
    (define (%method8) (sqrt (abs value)))
    (define (%method9) (log (+ value 1)))
    (define (%method10) (sin value))
    (define (%method11) (cos value))
    (define (%method12) (tan value))
    (define (%method13) (asin (/ value 100.0)))
    (define (%method14) (acos (/ value 100.0)))
    (define (%method15) (atan value))
    (define (%method16) (floor value))
    (define (%method17) (ceiling value))
    (define (%method18) (round value))
    (define (%method19) (truncate value))
    (define (%method20) (min value 0))
    (define (%method21) (max value 100))
    (define (%method22) (abs value))
    (define (%method23) (exp value))
    (define (%method24) (log10 (+ value 1)))
    (define (%method25) (sinh value))
    (define (%method26) (cosh value))
    (define (%method27) (tanh value))
    (define (%method28) (asinh value))
    (define (%method29) (acosh (+ value 1)))
    (define (%method30) (atanh (/ value 100.0)))
    (define (%method31) (real-part value))
    (define (%method32) (imag-part value)))

  (let ((time (timeit (lambda () (class-32-methods 42)) :number 100000)))
    (display* "32 instance methods:\t\t" (number->string time) " seconds\n"))

  (display "\n=== Method Call Performance Test ===\n\n")

  ; Test method call performance for each class
  (let ((obj-1 (class-1-method 42)))
    (let ((time (timeit (lambda () (obj-1 :method1)) :number 100000)))
      (display* "1 method call:\t\t\t" (number->string time) " seconds\n")))

  (let ((obj-2 (class-2-methods 42)))
    (let ((time (timeit (lambda () (obj-2 :method1)) :number 100000)))
      (display* "2 methods - call method1:\t" (number->string time) " seconds\n"))
    (let ((time (timeit (lambda () (obj-2 :method2)) :number 100000)))
      (display* "2 methods - call method2:\t" (number->string time) " seconds\n")))

  (let ((obj-4 (class-4-methods 42)))
    (let ((time (timeit (lambda () (obj-4 :method4)) :number 100000)))
      (display* "4 methods - call method4:\t" (number->string time) " seconds\n")))

  (let ((obj-8 (class-8-methods 42)))
    (let ((time (timeit (lambda () (obj-8 :method8)) :number 100000)))
      (display* "8 methods - call method8:\t" (number->string time) " seconds\n")))

  (let ((obj-16 (class-16-methods 42)))
    (let ((time (timeit (lambda () (obj-16 :method16)) :number 100000)))
      (display* "16 methods - call method16:\t" (number->string time) " seconds\n")))

  (let ((obj-32 (class-32-methods 42)))
    (let ((time (timeit (lambda () (obj-32 :method32)) :number 100000)))
      (display* "32 methods - call method32:\t" (number->string time) " seconds\n")))

  (display "\n"))

; Run the benchmark
(benchmark-define-case-class)