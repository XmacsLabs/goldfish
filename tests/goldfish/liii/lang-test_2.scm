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

(import (liii check)
        (liii lang_2)
        (only (liii base) let1 identity)
        (liii cut)
        (liii case)
        (liii error))

(define == class=?)
; (check-set-mode! 'report-failed)

;; Custom check that uses lang_2's class=? function
(define-macro (check2 expr => expected)
  `(check:proc ',expr (lambda () ,expr) ,expected ,class=?))

(define check check2)

(check ((@ + _ 2) 1) => 3)
(check ((@ list 1 _ 3 _ 5) 2 4) => (list 1 2 3 4 5))
(check ((@ list _ _) 'a 'b) => (list 'a 'b))

(check
  (let ((a 10))
    (define add (@ + (* a 2) _))  
    (set! a 100)
    (add 5))
=> 25)

(let ((x 5))
  (check 
    ((@ cons (+ x 1) _) 'y) 
   => (cons 6 'y)))

(check (procedure? (@ list 1 2)) => #t)
(check ((@ list 1 2)) => '(1 2))

(check ((@ _ 'a 'b) list) => (list 'a 'b))
(check ((@ map _ '(1 2 3)) (lambda (x) (+ x 1))) => '(2 3 4))
(check ((@ apply _ '(1 2 3)) +) => 6)

(check ((@ (@ + _ 1) _) 2) => 3)
(check ((@ _ _) (@ * _ 2) 3) => 6)

(typed-define (person (name string? "Bob") (age integer?))
  (string-append name " is " (number->string age) " years old"))

(check (person :age 21) => "Bob is 21 years old")
(check (person :name "Alice" :age 25) => "Alice is 25 years old")
(check-catch 'type-error (person :name 123 :age 25))

(check (rich-lists :range 1 5) => ($ (list 1 2 3 4)))
(check (rich-lists :range 1 5 2) => ($ (list 1 3)))
(check (rich-lists :range 1 6 2) => ($ (list 1 3 5)))
(check (rich-lists :range 5 1 -1) => ($ (list 5 4 3 2)))
(check ((rich-lists :range 1 5 2) :apply :collect) => (list 1 3))
(check ((rich-lists :range 1 5) :map (lambda (x) (* x 2))) => ($ (list 2 4 6 8)))
(check ((rich-lists :range 1 10 1) :map (lambda (x) (+ x 1))) => ($ (list 2 3 4 5 6 7 8 9 10)))
(check (rich-lists :range 5 1 1) => ($ (list )))

(check-catch 'value-error (rich-lists :range 1 5 0))

(check ((rich-lists :empty) :apply :empty?) => #t)
(check ((rich-lists :empty) :apply :head-option) => (none))


(check (rich-lists :concat ($ (list 1)) ($ (list 2))) => ($ (list 1 2)))
(check (rich-lists :concat ($ (list 1 2)) ($ (list 3 4))) => ($ (list 1 2 3 4)))
(check (rich-lists :concat (rich-lists :range 1 4) ($ (list 3 4))) => ($ (list 1 2 3 3 4)))
(check ((rich-lists :concat ($ (list 1)) ($ (list 2)))
           :apply :collect) => (list 1 2))
(check ((rich-lists :concat (rich-list '(1)) (rich-list '(2))) :apply :count) => 2)

(let1 result (rich-lists :fill 3 "a")
  (check (result :apply :collect) => '("a" "a" "a")))

(let1 result (rich-lists :fill 0 "a")
  (check (result :apply :collect) => '()))

(check-catch 'value-error (rich-lists :fill -1 "a"))

(let1 result (rich-lists :fill 2 42)
  (check (result :apply :collect) => '(42 42)))

(let1 result (rich-lists :fill 1000 "x")
  (check (length (result :apply :collect)) => 1000))

; (check ($ '(1 2 3) :apply 0) => 1)
; (check ($ '(1 2 3) 0) => 1)

(let1 lst (rich-list '(1 2 3 4 5))
  (check ((lst :find (lambda (x) (= x 3))) :apply :get) => 3)
  (check ((lst :find (lambda (x) (> x 2))) :apply :get) => 3)
  (check ((lst :find (lambda (x) (> x 10))) :apply :empty?) => #t)
  (check ((lst :find even?) :apply :get) => 2)
  (check ((lst :find (lambda (x) (< x 0))) :apply :empty?) => #t))

(let1 lst (rich-list '(1 2 3 4 5))
  (check ((lst :find-last even?) :apply :get) => 4)  ; 最后一个偶数是4
  (check ((lst :find-last (@ > _ 3)) :apply :get) => 5)  ; 最后一个大于3的元素是5
  (check ((lst :find-last (@ > _ 5)) :apply :empty?) => #t)  ; 没有大于5的元素
  (check ((lst :find-last zero?) :apply :empty?) => #t)  ; 没有0
  (check ((rich-list '()) :find-last even?) => (none)))  ; 空列表返回none

(check ($ (list 1 2 3) :apply :head) => 1)
(check-catch 'out-of-range ((rich-lists :empty) :apply :head))
(check ($ (list 1 2 3) :apply :head-option) => (option 1))
(check ((rich-lists :empty) :apply :head-option) => (none))

(check ($ (list 1 2 3) :apply :last) => 3)
(check-catch 'index-error ((rich-lists :empty) :apply :last))
(check ($ (list 1 2 3) :apply :last-option) => (option 3))
(check ((rich-lists :empty) :apply :last-option) => (none))

(let ((lst ($ '(1 2 3 4 5))))
  ;; 基本切片
  (check (lst :slice 1 3 :apply :collect) => '(2 3))

  ;; from超出范围
  (check (lst :slice 10 3 :apply :collect) => '())

  ;; until超出范围
  (check (lst :slice 2 10 :apply :collect) => '(3 4 5))

  ;; from > until
  (check (lst :slice 3 1 :apply :collect) => '())

  ;; 负数索引
  (check (lst :slice -1 3 :apply :collect) => '(1 2 3))

  ;; 链式调用
  (check (lst :slice 1 4 :map (@ * _ 2) :apply :collect) => '(4 6 8))

  ;; 空切片
  (check (lst :slice 2 2 :apply :collect) => '())
)

(check-true ($ (list) :apply :empty?))
(check-false ($ '(1 2 3) :apply :empty?))

; (check ($ (list ($ 1) ($ 2) ($ 3))) => (($ 1 :to 3) :map $))

(let1 lst ($ '(1 2 3 4 5))
  (check (lst :forall (@ > _ 0)) => #t)
  (check (lst :forall (@ > _ 3)) => #f)
)

(check ((rich-lists :empty) :forall (@ > _ 0)) => #t)

(let1 l (rich-list '(1 2 3))
  (check-true (l :exists even?)))

(let1 l (rich-list '(1 2 3))
  (check-true (l :contains 1))
  (check-false (l :contains 4)))

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :reverse :apply :collect) => '(5 4 3 2 1)))

(let ((lst (rich-list '(a b c d e))))
  (check (lst :reverse :apply :collect) => '(e d c b a)))

(let ((lst (rich-list '())))
  (check (lst :reverse :apply :collect) => '()))

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :take -1 :apply :collect) => '())
  (check (lst :take 0 :apply :collect) => '())
  (check (lst :take 3 :apply :collect) => '(1 2 3))
  (check (lst :take 5 :apply :collect) => '(1 2 3 4 5))
  (check (lst :take 10 :apply :collect) => '(1 2 3 4 5))
)

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :drop -1 :apply :collect) => '(1 2 3 4 5))
  (check (lst :drop 0 :apply :collect) => '(1 2 3 4 5))
  (check (lst :drop 3 :apply :collect) => '(4 5))
  (check (lst :drop 5 :apply :collect) => '())
  (check (lst :drop 10 :apply :collect) => '())
)

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :take-right -1 :apply :collect) => '())
  (check (lst :take-right 0 :apply :collect) => '())
  (check (lst :take-right 3 :apply :collect) => '(3 4 5))
  (check (lst :take-right 5 :apply :collect) => '(1 2 3 4 5))
  (check (lst :take-right 10 :apply :collect) => '(1 2 3 4 5))
)

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :drop-right -1 :apply :collect) => '(1 2 3 4 5))
  (check (lst :drop-right 0 :apply :collect) => '(1 2 3 4 5))
  (check (lst :drop-right 3 :apply :collect) => '(1 2))
  (check (lst :drop-right 5 :apply :collect) => '())
  (check (lst :drop-right 10 :apply :collect) => '())
)

(check ((rich-list (list 1 2 3)) :apply :count) => 3)
(check ((rich-list (list 1 2 3)) :count (cut > <> 1)) => 2)

(check ($ '() :apply :length) => 0)
(check ($ '(1) :apply :length) => 1)
(check ($ '(1 2) :apply :length) => 2)
(check ($ '(1 2 3) :apply :length) => 3)
(check ($ '(1 2 3 4 5) :apply :length) => 5)
(check ($ '(1 2 3 4 5 6 7 8 9 10) :apply :length) => 10)


(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :fold 0 +) => 15)
  (check (lst :fold '() (lambda (x acc) (cons x acc))) => '(5 4 3 2 1))

  (check (lst :fold-right 0 +) => 15)
  (check (lst :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3 4 5))
)

(check ($ '(3 1 2 4 5)
        :sort-with (lambda (x y) (< x y)))
    => ($ '(1 2 3 4 5)))

(check ($ (list 1 3 4 2 5) :sort-with < :take 2 :apply :collect) => '(1 2))

(check 
  ($ (list 1 3 4 2 5) 
     :sort-with <
     :take 2
     :apply :collect)
  => '(1 2))

(check 
  ($ '((3 . a) (1 . b) (2 . c) (1 . d))
     :sort-with (lambda (x y) (< (car x) (car y)))  ;; 按 car 排序
     :apply :collect)
  => '((1 . b) (1 . d) (2 . c) (3 . a)))

;; 测试按绝对值排序
(check ($ '(-3 1 -2 4 0) :sort-by abs :apply :collect) => '(0 1 -2 -3 4))
  
;; 测试按结构体字段排序
(let ((people ($ '((name . "Alice") (name . "Bob") (name . "Charlie")))))
  (check (people :sort-by (lambda (p) (string-length (cdr p))) :apply :collect)
         => '((name . "Bob") (name . "Alice") (name . "Charlie"))))
  
;; 测试空列表
(check ($ '() :sort-by identity :apply :collect) => '())
  
;; 测试链式调用
(check ($ '(-3 1 -2 4 0) 
         :sort-by abs 
         :filter positive? 
         :apply :collect)
       => '(1 4))

;; TODO: rich-hash-table not implemented yet
;; (check  (($ '(1 2 3 4 5 6) :group-by (@ modulo _ 2)) :apply :collect)
;;         =>  (hash-table 0 '(2 4 6) 1 '(1 3 5)))
;; 
;; (check  (($ '(1 2 3 4 5 6) :group-by (@ modulo _ 3)) :apply :collect)
;;         =>  (hash-table 0 '(3 6) 1 '(1 4) 2 '(2 5)))
;; 
;; (check  (($ '(1 2 3 4 5 6 7) :group-by (@ modulo _ 3)) :apply :collect)
;;         =>  (hash-table 0 '(3 6) 1 '(1 4 7) 2 '(2 5)))
;; 
;; (let ((result ($ '("apple" "banana" "cat" "dog") :group-by (@ string-length _))))
;;   (check (result :apply :collect) 
;;           => (hash-table 3 '("cat" "dog") 5 '("apple") 6 '("banana"))))

;; Single-argument sliding for rich-list
(check ($ '() :sliding 2) => #())
(check ($ '(1) :sliding 2) => #((1)))
(check ($ '(1 2) :sliding 2) => #((1 2)))
(check ($ '(1 2 3) :sliding 2) => #((1 2) (2 3)))
(check ($ '(1 2 3 4 5) :sliding 3) => #((1 2 3) (2 3 4) (3 4 5)))
(check ($ '(1 2 3 4 5) :sliding 1) => #((1) (2) (3) (4) (5)))
(check ($ '(1 2 3) :sliding 3) => #((1 2 3)))
(check ($ '(1 2 3) :sliding 4) => #((1 2 3)))

;; Error cases for size (single-arg) for rich-list
(check-catch 'value-error ($ '(1 2 3) :sliding 0))
(check-catch 'value-error ($ '(1 2 3) :sliding -1))
(check-catch 'type-error ($ '(1 2 3) :sliding 1.5))

;; Two-argument sliding for rich-list
(check ($ '() :sliding 2 2) => #())
(check ($ '(1 2 3 4 5) :sliding 2 2) => #((1 2) (3 4) (5)))
(check ($ '(1 2 3 4 5 6) :sliding 2 3) => #((1 2) (4 5)))
(check ($ '(1 2 3 4 5) :sliding 3 1) => #((1 2 3) (2 3 4) (3 4 5) (4 5) (5)))
(check ($ '(1 2 3 4) :sliding 2 2) => #((1 2) (3 4)))
(check ($ '(1 2) :sliding 3 1) => #((1 2) (2)))
(check ($ '(1 2 3 4 5) :sliding 3 2) => #((1 2 3) (3 4 5) (5)))
(check ($ '(1 2 3 4 5 6 7) :sliding 3 3) => #((1 2 3) (4 5 6) (7)))
(check ($ '(1 2 3 4 5) :sliding 5 1) => #((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)))
(check ($ '(1 2 3 4 5) :sliding 6 1) => #((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)))


;; Error cases for step (two-arg) for rich-list
(check-catch 'value-error ($ '(1 2 3) :sliding 2 0))
(check-catch 'value-error ($ '(1 2 3) :sliding 2 -1))
(check-catch 'type-error ($ '(1 2 3) :sliding 2 1.5))

(check (($ '(1 2 3)) :zip '(a b c) :apply :collect) => '((1 . a) (2 . b) (3 . c)))
(check (($ '(1 2 3)) :zip '(a b) :apply :collect) => '((1 . a) (2 . b)))

(check  ($ '(a b c) :zip-with-index :apply :collect)  
        => '((0 . a) (1 . b) (2 . c)))

(check  ($ '() :zip-with-index :apply :collect) 
        => '())

(check  ($ '(1 1 2 2 2 3 4 5 6 7) :zip-with-index :apply :collect)
        => '((0 . 1) (1 . 1) (2 . 2) (3 . 2) (4 . 2) (5 . 3) (6 . 4) (7 . 5) (8 . 6) (9 . 7)))

(check  ($ '(a a b c b) :distinct :apply :collect) 
        => '(a b c))

(check  ($ '(1 1 1 2 2 3 3 3 3 5 5 5) :distinct :apply :collect) 
        => '(1 2 3 5))

(check  ($ '() :distinct :apply :collect) 
        => '())

(check-catch 'value-error ($ '() :reduce +))

(check ($ '(1 2 3) :reduce +) => 6)  
(check ($ '(2 3 4) :reduce *) => 24)  
(check ($ '(5) :reduce (lambda (x y) (+ x y 10))) => 5)

(check ($ '() :reduce-option +) => (none))

(check ($ '(1 2 3) :reduce-option +) => (option 6))  
(check ($ '(2 3 4) :reduce-option *) => (option 24))  
(check ($ '(5) :reduce-option (lambda (x y) (+ x y 10))) => (option 5))

(check ($ '(1 2 3 4 5 6 7) :take-while (@ < _ 5) :apply :collect) => '(1 2 3 4))
(check ($ '() :take-while (@ < _ 5) :apply :collect) => '())
(check ($ '(1 2 3) :take-while number? :apply :collect) => '(1 2 3))
(check ($ '(5 1 2 3) :take-while (@ < _ 3) :apply :collect) => '())

(check ($ '(1 2 3 4 5 6 7) :drop-while (@ < _ 5) :apply :collect) => '(5 6 7))
(check ($ '() :drop-while (@ < _ 5) :apply :collect) => '())
(check ($ '(1 2 3) :drop-while number? :apply :collect) => '())
(check ($ '(5 1 2 3) :drop-while (@ < _ 3) :apply :collect) => '(5 1 2 3))

(let ((xs ($ '(1 2 3 4 5))))
  (check (xs :index-where even?) => 1)
  (check (xs :index-where (@ > _ 3)) => 3)
  (check (xs :index-where (@ > _ 5)) => #f)
)

(check ($ '(1 2 3) :max-by identity) => 3)
(check ($ '((1) (3) (2)) :max-by car) => '(3))
(check-catch 'value-error ($ '() :max-by identity))
(check-catch 'type-error ($ '(1 2 3) :max-by "not-function"))
(check-catch 'type-error ($ '("a" "b" "c") :max-by identity))

(check ($ '(1 2 3) :min-by identity) => 1)
(check ($ '((1) (3) (2)) :min-by car) => '(1))
(check-catch 'value-error ($ '() :min-by identity))
(check-catch 'type-error ($ '(1 2 3) :min-by "not-function"))
(check-catch 'type-error ($ '("a" "b" "c") :min-by identity))

(check ((rich-lists :empty) :append (list 1 2)) => ($ (list 1 2)))
(check ($ (list 1 2) :append (list )) => ($ (list 1 2)))
(check ($ (list 1 2) :append (list 3 4)) => ($ (list 1 2 3 4)))

(check ($ '() :max-by-option identity) => (none))

(check ($ '() :min-by-option identity) => (none))

; (check (object->string ($ '(1 2 3))) => "(1 2 3)")

(let1 l (rich-list (list 1 2 3))
  (check (l :apply :make-string) => "123")
  (check (l :make-string " ") => "1 2 3")
  (check (l :make-string "[" "," "]") => "[1,2,3]")
  
  (check-catch 'wrong-number-of-args (l :make-string "[" ","))
  (check-catch 'type-error (l :make-string 123 "," "]"))
  (check-catch 'type-error (l :make-string "[" 123 "]"))
  (check-catch 'type-error (l :make-string "[" "," 123))
)

(check ($ (list "a" "b") :apply :make-string) => "ab")
(check ($ (list "a" "b") :make-string " ") => "a b")

(let ((lst (rich-list '(1 2 3))))
  (check (lst :apply :to-vector) =>  #(1 2 3)))

; (let ((lst (rich-list '(1 2 3))))
  ;; TODO: rich-vector not implemented yet
;; (check (lst :apply :to-rich-vector) => (rich-vector #(1 2 3)))
;; (check ((lst :apply :to-rich-vector) :apply :collect) => #(1 2 3)))

(check-report)
