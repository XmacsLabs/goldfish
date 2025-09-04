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

;; TODO(jinser): check-error (out of range)

(import (srfi srfi-1)
        (srfi srfi-158)
        (srfi srfi-78))

(check-set-mode! 'report-failed)

;; =======================================
;; Constructor
;; =======================================

; generator
(let ((g (generator 1 2 3)))
  (check (g) => 1)
  (check (g) => 2)
  (check (g) => 3)
  (check (g) => (eof-object)))

; circular-generator
(let ((g (circular-generator 1 2)))
  (check (list (g) (g) (g) (g)) => '(1 2 1 2)))

; make-iota-generator
(let ((g (make-iota-generator 3 10 2)))
  (check (g) => 10)
  (check (g) => 12)
  (check (g) => 14)
  (check (g) => (eof-object)))

; make-range-generator
(let ((g (make-range-generator 1 5 2)))
  (check (generator->list g) => '(1 3)))

; make-coroutine-generator
(let* ((g (make-coroutine-generator
           (lambda (yield)
             (yield 10)
             (yield 20)))))
  (check (g) => 10)
  (check (g) => 20)
  (check (g) => (eof-object)))

(let* ((counter 0)
       (g (make-coroutine-generator
            (lambda (yield)
              (set! counter (+ counter 1)) ; 1
              (yield counter)
              (set! counter (+ counter 1)) ; 2
              (yield counter)))))
  (check counter => 0)
  (check (g) => 1)
  (check counter => 1)
  (check (g) => 2)
  (check counter => 2))

; make-for-each-generator
(let ((g (make-for-each-generator for-each '(1 2 3))))
  (check (generator->list g) => '(1 2 3)))

; make-unfold-generator
(let ((g (make-unfold-generator (lambda (x) (>= x 3))
                                (lambda (x) x)
                                (lambda (x) (+ x 1))
                                0)))
  (check (generator->list g) => '(0 1 2)))

;; =======================================
;; ->generator
;; =======================================

; list->generator
(let ((g (list->generator '(7 8))))
  (check (g) => 7)
  (check (g) => 8)
  (check (g) => (eof-object)))

; vector->generator
(let ((g (vector->generator '#(1 2))))
  (check (generator->list g) => '(1 2)))

; reverse-vector->generator
(let ((g (reverse-vector->generator '#(1 2))))
  (check (generator->list g) => '(2 1)))

; string->generator
(let ((g (string->generator "ab")))
  (check (generator->list g) => '(#\a #\b)))

; bytevector->generator
(let ((g (bytevector->generator #u8(1 2))))
  (check (generator->list g) => '(1 2)))

;; =======================================
;; generator->
;; =======================================

; generator->list
(let ((g (generator 4 5 6)))
  (check (generator->list g) => '(4 5 6)))

; generator->reverse-list
(let ((g (generator 1 2 3)))
  (check (generator->reverse-list g) => '(3 2 1)))

; generator->vector
(let ((g (generator 1 2)))
  (check (generator->vector g) => '#(1 2)))

; generator->vector!
(let ((g (generator 1 2 3))
      (v (make-vector 3)))
  (generator->vector! v 0 g)
  (check v => '#(1 2 3)))

(let ((g (generator 1 2 3))
      (v (make-vector 3 #f)))
  (generator->vector! v 1 g)
  (check v => '#(#f 1 2)))

; generator->string
(let ((g (generator #\a #\b)))
  (check (generator->string g) => "ab"))

; generator-map->list
(let ((g (generator 1 2 3)))
  (check (generator-map->list (lambda (x) (+ x 1)) g) => '(2 3 4)))

;; =======================================
;; Generator operations
;; =======================================

; gcons*
(let ((g (gcons* 1 2 (generator 3 4))))
  (check (generator->list g) => '(1 2 3 4)))

; gappend
(let ((g (gappend (generator 1 2) (generator 3) (generator))))
  (check (generator->list g) => '(1 2 3)))


; gflatten
(let ((g (gflatten (generator (list 1 2) (list 3)))))
  (check (generator->list g) => '(1 2 3)))

; ggroup
(let ((g (ggroup (generator 1 2 3 4 5)
                 2)))
  (check (generator->list g) => '((1 2) (3 4) (5))))

(let ((g (ggroup (generator 1 2 3 4 5)
                 3 #f)))
  (check (generator->list g) => '((1 2 3) (4 5 #f))))

; gmerge
(let ((g (gmerge <
                 (generator 1 3 5)
                 (generator 2 4 6))))
  (check (generator->list g) => '(1 2 3 4 5 6)))

(let ((g (gmerge >
                 (generator 1 3 5)
                 (generator 2 4 6))))
  (check (generator->list g) => '(2 4 6 1 3 5)))

(let ((g (gmerge char<?
                 (generator #\a #\b #\c)
                 (generator #\d #\e #\f))))
  (check (generator->list g) => '(#\a #\b #\c #\d #\e #\f)))

; gmap
(let ((g (gmap (lambda (x) (* x x)) (generator 1 2 3))))
  (check (g) => 1)
  (check (g) => 4)
  (check (g) => 9)
  (check (g) => (eof-object)))

; gcombine
(let* ((f (lambda (item seed) (values (+ item seed) (+ seed 1))))
       (g (gcombine f 0 (generator 1 1 1))))
  (check (g) => 1) ; 1 + 0
  (check (g) => 2) ; 1 + 1
  (check (g) => 3) ; 1 + 2
  (check (g) => (eof-object)))

(let* ((f (lambda (item1 item2 seed) (values (+ item1 item2 seed) (+ seed 1))))
       (g (gcombine f 0 (generator 1 1 1) (generator 2 2))))
  (check (g) => 3) ; 1 + 2 + 0
  (check (g) => 4) ; 1 + 2 + 1
  (check (g) => (eof-object)))

; gfilter
(let ((g (gfilter even? (generator 1 2 3 4))))
  (check (g) => 2)
  (check (g) => 4)
  (check (g) => (eof-object)))

; gremove
(let ((g (gremove even? (generator 1 2 3 4))))
  (check (generator->list g) => '(1 3)))

; gstate-filter
(let ((g (gstate-filter (lambda (x s) (values (odd? x) s)) 0 (generator 1 2 3))))
  (check (generator->list g) => '(1 3)))

; gtake
(let ((g (gtake (generator 1 2 3) 2)))
  (check (generator->list g) => '(1 2)))

(let ((g (gtake (make-iota-generator 1024) 3)))
  (check (generator->list g) => '(0 1 2)))

; gdrop
(let ((g (gdrop (generator 1 2 3) 2)))
  (check (generator->list g) => '(3)))

; gtake-while
(let ((g (gtake-while odd? (generator 1 3 4 5))))
  (check (generator->list g) => '(1 3)))

; gdrop-while
(let ((g (gdrop-while odd? (generator 1 3 4 5))))
  (check (generator->list g) => '(4 5)))

; gdelete
(let ((g (gdelete 2 (generator 1 2 3 2 4))))
  (check (generator->list g) => '(1 3 4)))

; gdelete-neighbor-dups
(let ((g (gdelete-neighbor-dups (generator 1 1 2 2 1) =)))
  (check (generator->list g) => '(1 2 1)))

; gindex
(let ((g (gindex (generator 'a 'b 'c 'd)
                 (generator 1 2))))
  (check (generator->list g) => '(b c)))

; gselect
(let ((g (gselect (generator 1 2 3) (generator #t #f #t))))
  (check (generator->list g) => '(1 3)))

(let ((g (gselect (generator 1 2 3) (generator #f #t))))
  (check (generator->list g) => '(2)))

(let ((g (gselect (generator 1 2 3) (generator #f #t #t #t))))
  (check (generator->list g) => '(2 3)))

; generator-fold
(let ((g (generator 1 2 3)))
  (check (generator-fold + 0 g) => 6))

(let ((g (generator 1 2 3)))
  (check (generator-fold * 1 g) => 6))

; generator-for-each
(let ((g (generator 1 2 3))
      (sum 0))
  (generator-for-each (lambda (x) (set! sum (+ sum x))) g)
  (check sum => 6))

; generator-find
(let ((g (generator 1 2 3 4)))
  (check (generator-find even? g) => 2))

(let ((g (generator 1 4)))
  (check (generator-find even? g) => 4))

; generator-count
(let ((g (generator 1 2 3 4 5)))
  (check (generator-count even? g) => 2))

; generator-any
(let ((g (generator 1 3 5)))
  (check (generator-any even? g) => #f))
(let ((g (generator 1 4 5)))
  (check (generator-any even? g) => #t))

; generator-every
(let ((g (generator 2 4 6)))
  (check (generator-every even? g) => #t))
(let ((g (generator 2 3 4)))
  (check (generator-every even? g) => #f))

; generator-unfold
(define* (unfold p f g seed (tail-gen (lambda (x) '())))
  (if (p seed)
      '()
      (cons (f seed)
            (unfold p f g (g seed)))))

(check (list->string
         (generator-unfold (make-for-each-generator string-for-each
                                                    "abc")
                           unfold))
       => "abc")

;; =======================================
;; Accumulator
;; =======================================

; make-accumulator
(let ((a (make-accumulator + 0 (lambda (x) x))))
  (a 1)
  (check (a #<eof>) => 1)
  (a 2)
  (check (a #<eof>) => 3)
  (a 3)
  (check (a #<eof>) => 6))

(let* ((res #f)
       (accum (make-accumulator * 1 (lambda (state) (set! res state)))))
  (accum 2)
  (check res => #f)
  (accum 3)
  (accum #<eof>)
  (check res => 6)
  (accum 2)
  (accum #<eof>)
  (check res => 12))

; list-accumulator
(let ((a (list-accumulator)))
  (a 1)
  (a 2)
  (check (a #<eof>) => '(1 2)))

; reverse-list-accumulator
(let ((a (reverse-list-accumulator)))
  (a 1)
  (a 2)
  (check (a #<eof>) => '(2 1)))

; vector-accumulator
(let ((a (vector-accumulator)))
  (a 1)
  (a 2)
  (check (a #<eof>) => '#(1 2)))

; reverse-vector-accumulator
(let ((a (reverse-vector-accumulator)))
  (a 1)
  (a 2)
  (check (a #<eof>) => '#(2 1)))

; vector-accumulator!
(let* ((v #(0 1 0 0 0))
       (a (vector-accumulator! v 0)))
  (a 2)
  (check v => '#(2 1 0 0 0))
  (a 2)
  (a 2)
  (check v => '#(2 2 2 0 0))
  (a 2)
  (check v => '#(2 2 2 2 0)))

(let* ((v #(0 1 0 0 0))
       (a (vector-accumulator! v 1)))
  (a 2)
  (check v => '#(0 2 0 0 0))
  (a 2)
  (a 2)
  (check v => '#(0 2 2 2 0))
  (a 2)
  (check v => '#(0 2 2 2 2)))

; string-accumulator
(let ((a (string-accumulator)))
  (a #\a)
  (a #\b)
  (check (a #<eof>) => "ab"))

; bytevector-accumulator
(let ((a (bytevector-accumulator)))
  (a 1)
  (a 2)
  (check (a #<eof>) => #u8(1 2)))

; bytevector-accumulator!
(let* ((bv (make-bytevector 2))
       (a (bytevector-accumulator! bv 0)))
  (a 1)
  (a 2)
  (check bv => #u8(1 2)))

(let* ((bv (make-bytevector 3))
       (a (bytevector-accumulator! bv 1)))
  (a 1)
  (a 2)
  (check bv => #u8(0 1 2)))

; sum-accumulator
(let ((a (sum-accumulator)))
  (a 1)
  (a 2)
  (a 3)
  (check (a #<eof>) => 6))

; product-accumulator
(let ((a (product-accumulator)))
  (a 2)
  (a 3)
  (check (a #<eof>) => 6))

(check-report)
