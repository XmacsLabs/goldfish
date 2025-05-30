;
; Copyright (C) 2024 The Goldfish Scheme Authors
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

(import (liii list)
        (liii check)
        (liii vector)
        (liii cut)
        (liii base)
        (only (scheme base) let-values))

(check-set-mode! 'report-failed)

(for-each (lambda (p) (check (procedure? p) => #t))
  (list
   vector-empty?
   vector-count
   vector-any vector-every vector-copy vector-copy!
   vector-index vector-index-right vector-partition
   vector-swap! vector-reverse! vector-cumulate reverse-list->vector
   vector=))

(check-true (vector? (int-vector 1 2 3)))
(check-catch 'wrong-type-arg (int-vector 1 2 'a))

(let1 v (int-vector 1 2 3)
  (check (vector-ref v 0) => 1)
  (check (vector-ref v 1) => 2)
  (check (vector-ref v 2) => 3))

(check (vector-copy #(0 1 2 3)) => #(0 1 2 3))
(check (vector-copy #(0 1 2 3) 1) => #(1 2 3))
(check (vector-copy #(0 1 2 3) 3) => #(3))
(check (vector-copy #(0 1 2 3) 4) => #())

(check-catch 'out-of-range (vector-copy #(0 1 2 3) 5))
(check-catch 'out-of-range (vector-copy #(0 1 2 3) 1 5))

(define my-vector #(0 1 2 3))
(check (eqv? my-vector (vector-copy #(0 1 2 3))) => #f)
(check-true
  (eqv? (vector-ref my-vector 2)
        (vector-ref (vector-copy #(0 1 2 3)) 2)))

(check (vector-copy #(0 1 2 3) 1 1) => #())
(check (vector-copy #(0 1 2 3) 1 2) => #(1))
(check (vector-copy #(0 1 2 3) 1 4) => #(1 2 3))

(check-true (int-vector? (int-vector 1 2 3)))
(check-false (int-vector? (vector 1 2 3)))

(check-true (vector-empty? (vector)))
(check-false (vector-empty? (vector 1)))
(check-catch 'type-error (vector-empty? 1))

; trivial cases
(check-true (vector= eq?))
(check-true (vector= eq? '#(a)))
; basic cases
(check-true (vector= eq? '#(a b c d) '#(a b c d)))
(check-false (vector= eq? '#(a b c d) '#(a b d c)))
(check-false (vector= = '#(1 2 3 4 5) '#(1 2 3 4)))
(check-true (vector= = '#(1 2 3 4) '#(1 2 3 4)))
(check-true (vector= equal? '#(1 2 3) '#(1 2 3) '#(1 2 3)))
(check-false (vector= equal? '#(1 2 3) '#(1 2 3) '#(1 2 3 4)))
; error cases
(check-catch 'type-error (vector= 1 (vector (vector 'a)) (vector (vector 'a))))
; complex cases in srfi-133
(check-true (vector= equal? (vector (vector 'a)) (vector (vector 'a))))
(check-false (vector= eq? (vector (vector 'a)) (vector (vector 'a))))
(check (vector-fold + 0 #(1 2 3 4)) => 10)  ; 1 + 2 + 3 + 4 = 10
(check (vector-fold * 1 #(1 2 3 4)) => 24)  ; 1 * 2 * 3 * 4 = 24

(check (vector-fold (lambda (x acc) (cons x acc)) '() #(1 2 3)) => '(3 2 1))

(check (vector-fold (lambda (x acc) (+ acc (if (even? x) 1 0))) 0 #(1 2 3 4)) => 2)

(check (vector-fold + 0 #()) => 0)
(check (vector-fold * 1 #()) => 1)

(check (vector-fold + 0 #(5)) => 5)
(check (vector-fold * 1 #(5)) => 5)

(check (vector-fold string-append "" #("a" "b" "c")) => "cba")
(check (vector-fold (lambda (x acc) (and acc x)) #t #(#t #t #f)) => #f)

(check (vector-fold-right + 0 #(1 2 3 4)) => 10)  ; 4 + 3 + 2 + 1 = 10
(check (vector-fold-right * 1 #(1 2 3 4)) => 24)  ; 4 * 3 * 2 * 1 = 24
(check (vector-fold-right (lambda (x acc) (cons x acc)) '() #(1 2 3)) => '(1 2 3))  ; 保持顺序
(check (vector-fold-right (lambda (x acc) (+ acc (if (even? x) 1 0))) 0 #(1 2 3 4)) => 2)  ; 统计偶数个数

(check (vector-fold-right + 0 #()) => 0)
(check (vector-fold-right * 1 #()) => 1)

(check (vector-fold-right + 0 #(5)) => 5)
(check (vector-fold-right * 1 #(5)) => 5)

(check (vector-fold-right string-append "" #("a" "b" "c")) => "abc")
(check (vector-fold-right (lambda (x acc) (and acc x)) #t #(#t #t #f)) => #f)

(check (vector-fold (lambda (x acc) (cons x acc)) '() #(1 2 3)) => '(3 2 1))  ; vector-fold 反转向量
(check (vector-fold-right (lambda (x acc) (cons x acc)) '() #(1 2 3)) => '(1 2 3))  ; vector-fold-right 保持顺序

(check
  (let ((lst (make-list 5)))
    (vector-for-each
      (lambda (i) (list-set! lst i (* i i)))
      #(0 1 2 3 4))
    lst)
  => '(0 1 4 9 16))

(check
  (let ((lst (make-list 5)))
    (vector-for-each
      (lambda (i) (list-set! lst i (* i i)))
      #(0 1 2))
    lst)
  => '(0 1 4 #f #f))

(check
  (let ((lst (make-list 5)))
    (vector-for-each
      (lambda (i) (list-set! lst i (* i i)))
      #())
    lst)
  => '(#f #f #f #f #f))

(check (vector-count even? #()) => 0)
(check (vector-count even? #(1 3 5 7 9)) => 0)
(check (vector-count even? #(1 3 4 7 8)) => 2)

; Trivial cases.
(check (vector-cumulate + 0 '#(1 2 3 4)) => #(1 3 6 10))
(check (vector-cumulate - 0 '#(1 2 3 4)) => #(-1 -3 -6 -10))
(check (vector-cumulate * 1 '#(-1 -2 -3 -4)) => #(-1 2 -6 24))

;;; Test cases of vec.
; Not a vec input.
(check-catch 'type-error (vector-cumulate + 0 'a))
; Empty vec test.
(check (vector-cumulate + 0 '#()) => #())

;; Test cases of fn.
; A case with consant fn.
(check (vector-cumulate (lambda (x y) 'a) 0 '#(1 2 3)) => #(a a a))
; A wrong-number-of-args case with 1-arg fn.
(check-catch 'wrong-number-of-args (vector-cumulate (lambda (x) 'a) 0 '#(1 2 3)))
; A wrong-type-arg case with args can't be mapped by fn.
(check-catch 'wrong-type-arg (vector-cumulate + '(1) '#(1 2 3)))

;;; Test cases of knil.
; A case of different type of knil/cumu and vec-i.
(check (vector-cumulate (lambda (x y) (+ x 2)) 0 '#('a 'b 'c)) => #(2 4 6))
(check (vector-any even? #()) => #f)
(check (vector-any even? #(1 3 5 7 9)) => #f)
(check (vector-any even? #(1 3 4 7 8)) => #t)

(check (vector-every odd? #()) => #t)
(check (vector-every odd? #(1 3 5 7 9)) => #t)
(check (vector-every odd? #(1 3 4 7 8)) => #f)

(check (vector-index even? #()) => #f)
(check (vector-index even? #(1 3 5 7 9)) => #f)
(check (vector-index even? #(1 3 4 7 8)) => 2)

(check (vector-index-right even? #()) => #f)
(check (vector-index-right even? #(1 3 5 7 9)) => #f)
(check (vector-index-right even? #(1 3 4 7 8)) => 4)

(check (vector-skip even? #(1 2 3 4)) => 0)  ; 第一个元素 1 不满足 even?
(check (vector-skip odd? #(1 3 5 7)) => #f)  ; 所有元素都满足 odd?
(check (vector-skip (lambda (x) (< x 5)) #(1 2 3 4 5)) => 4)  ; 第一个不满足谓词的元素是 5
(check (vector-skip (lambda (x) (char=? x #\a)) #(#\a #\a #\b #\c)) => 2)  ; 第一个不满足谓词的元素是 #\b

(check (vector-skip even? #()) => #f)  ; 空向量没有元素，返回 #f

(check (vector-skip even? #(1)) => 0)  ; 第一个元素 1 不满足 even?
(check (vector-skip odd? #(2)) => 0)  ; 第一个元素 2 满足 even?

(check (vector-skip (lambda (x) (string=? x "a")) #("a" "a" "b" "c")) => 2)  ; 第一个不满足谓词的元素是 "b"
(check (vector-skip (lambda (x) (eq? x #t)) #(#t #t #f #t)) => 2)  ; 第一个不满足谓词的元素是 #f

(check (vector-skip (lambda (x) (> x 0)) #(1 2 3 4)) => #f)  ; 所有元素都满足谓词
(check (vector-skip (lambda (x) (char-alphabetic? x)) #(#\a #\b #\c)) => #f)  ; 所有元素都满足谓词

(check (vector-skip-right even? #(1 2 3 4)) => 2)  ; 从右开始，第一个不满足 even? 的元素是 3
(check (vector-skip-right odd? #(1 3 5 7)) => #f)  ; 所有元素都满足 odd?
(check (vector-skip-right (lambda (x) (< x 5)) #(1 2 3 4 5)) => 4)  ; 从右开始，第一个不满足谓词的元素是 5
(check (vector-skip-right (lambda (x) (char=? x #\a)) #(#\a #\a #\b #\c)) => 3)  ; 从右开始，第一个不满足谓词的元素是 #\c

(check (vector-skip-right even? #()) => #f)  ; 空向量没有元素，返回 #f

(check (vector-skip-right even? #(1)) => 0)  ; 第一个元素 1 不满足 even?
(check (vector-skip-right odd? #(2)) => 0)  ; 第一个元素 2 满足 even?

(check (vector-skip-right (lambda (x) (string=? x "a")) #("a" "a" "b" "c")) => 3)  ; 从右开始，第一个不满足谓词的元素是 "c"
(check (vector-skip-right (lambda (x) (eq? x #t)) #(#t #t #f #t)) => 2)  ; 从右开始，第一个不满足谓词的元素是 #f

(check (vector-skip-right (lambda (x) (> x 0)) #(1 2 3 4)) => #f)  ; 所有元素都满足谓词
(check (vector-skip-right (lambda (x) (char-alphabetic? x)) #(#\a #\b #\c)) => #f)  ; 所有元素都满足谓词

(define (vector-partition->list pred v)
  (let-values (((ret cnt) (vector-partition pred v))) (list ret cnt)))

(check (vector-partition->list even? #()) => '(#() 0))
(check (vector-partition->list even? #(1 3 5 7 9)) => '(#(1 3 5 7 9) 0))
(check (vector-partition->list even? #(1 3 4 7 8)) => '(#(4 8 1 3 7) 2))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 1 2)
(check my-vector => #(0 2 1 3))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 1 1)
(check my-vector => #(0 1 2 3))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 0 (- (vector-length my-vector) 1))
(check my-vector => #(3 1 2 0))

(check-catch 'out-of-range
  (vector-swap! my-vector 1 (vector-length my-vector)))

(let ((vec (vector 1 2 3 4)))
  (vector-reverse! vec)
  (check vec => #(4 3 2 1)))

(let ((vec (vector 'a 'b 'c 'd)))
  (vector-reverse! vec 1 3)
  (check vec => #(a c b d)))

(let ((vec (vector 10 20 30)))
  (vector-reverse! vec 2 2)
  (check vec => #(10 20 30)))

(check-catch 'wrong-number-of-args 
  (vector-reverse! (vector 1 2) 0 2 3)) 

(check-catch 'type-error 
  (vector-reverse! (vector 1 2) 'a 2)) 

(check-catch 'type-error 
  (vector-reverse! (vector 1 2) 0 'b)) 

(check-catch 'out-of-range 
  (vector-reverse! (vector 1 2) -1 2)) 

(check-catch 'out-of-range 
  (vector-reverse! (vector 1 2) 0 5)) 

(check-catch 'out-of-range 
  (vector-reverse! (vector 1 2) 2 1))

(let ((vec (vector)))
  (vector-reverse! vec 0 0)
  (check vec => #()))

(let ((vec (vector 100)))
  (vector-reverse! vec)
  (check vec => #(100)))

(let ((vec (vector 1 2 3)))
  (vector-reverse! vec)
  (vector-reverse! vec)
  (check vec => #(1 2 3)))

(define my-vector (vector 0 1 2 3 4))
(fill! my-vector #f)
(check my-vector => #(#f #f #f #f #f)) 

(define my-vector (vector 0 1 2 3 4))
(fill! my-vector #f 1 2)
(check my-vector => #(0 #f 2 3 4)) 

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))

;(< at 0)
(check-catch 'out-of-range (vector-copy! b -1 a))

;(< start 0)
(check-catch 'out-of-range (vector-copy! b 0 a -1))

;(> start (vector-length from))
(check-catch 'out-of-range (vector-copy! b 0 a 6))

;(> end (vector-length from))
(check-catch 'out-of-range (vector-copy! b 0 a 0 6))

;(> start end)
(check-catch 'out-of-range (vector-copy! b 0 a 2 1))

;(> (+ at (- end start)) (vector-length to))
(check-catch 'out-of-range (vector-copy! b 6 a))

(check-catch 'out-of-range (vector-copy! b 1 a))

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))
(vector-copy! b 0 a 1)
(check b => #("a1" "a2" "a3" "a4" "b4"))

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))
(vector-copy! b 0 a 0 5)
(check b => #("a0" "a1" "a2" "a3" "a4")) 

(check (reverse-list->vector '()) => '#())
(check (reverse-list->vector '(1 2 3)) => '#(3 2 1))

(check-catch 'type-error (reverse-list->vector '(1 2 . 3)))

(check-catch 'type-error (reverse-list->vector (circular-list 1 2 3)))

(check (vector->string (vector #\0 #\1 #\2 #\3)) => "0123")
(check (vector->string (vector #\a #\b #\c)) => "abc")

(check (vector->string (vector #\0 #\1 #\2 #\3) 0 4) => "0123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1) => "123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 4) => "123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 3) => "12")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 2) => "1")

(check-catch 'out-of-range (vector->string (vector #\0 #\1 #\2 #\3) 2 10))

(check (vector->string (vector 0 1 #\2 3 4) 2 3) => "2")

(check-catch 'wrong-type-arg (vector->string (vector 0 1 #\2 3 4) 1 3))

(check (string->vector "0123") => (vector #\0 #\1 #\2 #\3))
(check (string->vector "abc") => (vector #\a #\b #\c))

(check (string->vector "0123" 0 4) => (vector #\0 #\1 #\2 #\3))
(check (string->vector "0123" 1) => (vector #\1 #\2 #\3))
(check (string->vector "0123" 1 4) => (vector #\1 #\2 #\3))
(check (string->vector "0123" 1 3) => (vector #\1 #\2))
(check (string->vector "0123" 1 2) => (vector #\1))

(check-catch 'out-of-range (string->vector "0123" 2 10))

(check (vector-filter even? #(1 2 3 4 5 6)) => #(2 4 6))
(check (vector-filter (lambda (x) (> x 3)) #(1 2 3 4 5 6)) => #(4 5 6))
(check (vector-filter (lambda (x) (string? x)) #(1 "a" 2 "b" 3)) => #("a" "b"))
(check (vector-filter (lambda (x) #t) #()) => #())
(check (vector-filter (lambda (x) #f) #(1 2 3)) => #())

(check-report)

