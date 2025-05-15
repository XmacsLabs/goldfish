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

(define-library (liii hashset)
(import (liii lang)
        (srfi srfi-125)
        (srfi srfi-128))
(export hashset)
(begin

(define case-class-comparator
  (begin
    (define (base-type? x) (???))
    (define (type-test x)
      (or (base-type? x)
          (case-class? x)))
    (define equality ==)
    (define ordering #t)
    (define (hash x)
      (if (base-type? x)
        (hash-code x)
        (x :hash-code)))  ; not implemented
    (make-comparator type-test equality ordering hash)))

(define-case-class hashset ((data hash-table?) (comparator comparator?))
  (define (%contains x)
    (hash-table-contains? data x))

  (chained-define (@empty) (hashset (make-hash-table) (make-default-comparator)))

  (chained-define (%add! x)
    (hash-table-set! data x #t)
    (%this))

  (chained-define (%remove! x)
    (hash-table-delete! data x)
    (%this))

  (define (%internal-table) data)

  (chained-define (%concat other)
    (define ht (copy data))
    (hash-table-for-each
      (lambda (key val)
        (when (not (equal? val #t))
          (value-error "unexpected value for hashset"))
        (hash-table-set! ht key #t))
      (other :internal-table))
    (hashset ht comparator))


) ; end of hashset

) ; end of begin
)
