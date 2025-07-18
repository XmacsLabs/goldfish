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
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error))

(check-set-mode! 'report-failed)

(check ((lambda (x) (* x x)) 5) => 25)
(check ((lambda (x) (* x x)) 0) => 0)
(check ((lambda (x) (* x x)) -3) => 9)

(check ((lambda (x y) (+ x y)) 3 5) => 8)
(check ((lambda (x y) (* x y)) 4 6) => 24)

(check ((lambda () 42)) => 42)

(check ((lambda (x) ((lambda (y) (+ x y)) 5)) 3) => 8)

(define (apply-function f x) (f x))
(check (apply-function (lambda (x) (* x x)) 5) => 25)
(check (apply-function (lambda (x) (+ x 1)) 10) => 11)

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))
(check (map (lambda (x) (* x 2)) '(1 2 3 4)) => '(2 4 6 8))
(check (map (lambda (x) (+ x 1)) '(0 1 2 3)) => '(1 2 3 4))

(check (filter (lambda (x) (> x 2)) '(1 2 3 4 5)) => '(3 4 5))

(check (if (> 3 2) ((lambda () 3)) ((lambda () 2))) => 3)
(check (if (< 3 2) ((lambda () 3)) ((lambda () 2))) => 2)

(check (cond ((> 3 2) ((lambda () 3))) (else ((lambda () 2)))) => 3)
(check (cond ((< 3 2) ((lambda () 3))) (else ((lambda () 2)))) => 2)

(let ((create-counter (lambda () (let ((count 0)) (lambda () (set! count (+ count 1)) count)))))
  (let ((counter1 (create-counter)) (counter2 (create-counter)))
    (counter1) (counter1) (counter2) (check (counter1) => 3) (check (counter2) => 2)))

(check-catch 'unbound-variable ((lambda (x) y) 5))
(check-catch 'wrong-type-arg (map (lambda (x) (+ x 1)) '(1 2 a 4)))

(check (if (> 3 2) 3 2) => 3)
(check (if (< 3 2) 3 2) => 2)

(check (if (and (> 3 1) (< 3 4)) 'true-branch 'false-branch) => 'true-branch)
(check (if (or (> 3 4) (< 3 1)) 'true-branch 'false-branch) => 'false-branch)

(check (cond ((> 3 2) 3) (else 2)) => 3)
(check (cond ((< 3 2) 3) (else 2)) => 2)
(check (cond ((and (> 3 1) (< 3 4)) 'true-branch) (else 'false-branch)) => 'true-branch)
(check (cond ((or (> 3 4) (< 3 1)) 'true-branch) (else 'false-branch)) => 'false-branch)

(check (cond (2 => (lambda (n) (* n 2)))) => 4)
(check (cond (#f => (lambda (n) (* n 2))) (else 'no-match)) => 'no-match)
(check (cond (3 => (lambda (n) (* n 2))) (else 'no-match)) => 6)

(check (case '+
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p0)

(check (case '-
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p0)

(check (case '*
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p1)

(check (case '@
         ((+ -) 'p0)
         ((* /) 'p1))
  => #<unspecified>)

(check (case '&
         ((+ -) 'p0)
         ((* /) 'p1))
  => #<unspecified>)

#|
and
对任意数量的参数执行逻辑与操作，支持短路求值。

语法
----
(and [expr ...])

参数
----
expr : any
    任意类型的表达式。在 Scheme 中，除了 #f 之外的所有值都被视为真值。

返回值
-----
any
    如果没有任何表达式，返回 #t
    如果只有一个表达式，返回该表达式的结果
    对于多个表达式，返回最后一个真值表达式的结果，或者遇到第一个假值时立即返回 #f

短路求值
-------
从左到右依次求值，一旦遇到 #f 就立即停止求值并返回 #f

|#

;; 基础测试用例
(check-true (and))  ; 零参数情况

(check (and 1) => 1)  ; 单参数 - 真值
(check-false (and #f))  ; 单参数 - 假值

;; 多参数真值情况
(check-true (and #t #t #t))
(check (and 1 2 3) => 3)  ; 返回最后一个真值
(check (and #t "string" 'symbol) => 'symbol)

;; 多参数假值情况
(check-false (and #t #f #t))
(check-false (and #f #t #f))
(check-false (and #f #f #f))

;; 混合类型测试
(check-true (and 1 '() "non-empty" #t))
(check-false (and #f '() "non-empty" #t))
(check-false (and 1 '() "non-empty" #f))

;; 表达式求值测试
(check-true (and (> 5 3) (< 5 10)))
(check-false (and (> 5 3) (> 5 10)))

;; 短路求值测试
(check-catch 'error-name
  (and (error 'error-name "This should not be evaluated") #f))
(check-false (and #f (error "This should not be evaluated")))

;; 边缘情况测试
(check (and 0) => 0)  ; 0 在 Scheme 中是真值
(check (and '()) => '())  ; 空列表是真值
(check (and #t #t '()) => '())  ; 返回最后一个真值
(check-false (and #t #t #f #t))  ; 在第三个参数短路

;; 确保返回的是原始值而非转换后的布尔值
(check (and #t 42) => 42)
(check (and #t 'a 'b 'c) => 'c)
(check-false (and 'a 'b #f 'd))

(check-true (or #t #t #t))
(check-true (or #t #f #t))
(check-true (or #f #t #f))
(check-false (or #f #f #f))

(check-false (or))

(check (or 1 '() "non-empty" #t) => 1)
(check (or #f '() "non-empty" #t) => '())
(check (or 1 '() "non-empty" #f) => 1)

(check-true (or (> 5 3) (< 5 10)))
(check-true (or (> 5 3) (> 5 10)))
(check-false (or (< 5 3) (> 5 10)))

(check-true (or #t (error "This should not be evaluated")))  ; 短路，不会执行error
(check-catch 'error-name
  (or (error 'error-name "This should be evaluated") #f))  ; 第一个条件为error，不会短路


(check (or #f 1) => 1)  ; 返回第一个为真的值
(check (or #f #f 2) => 2)  ; 返回第一个为真的值
(check (or #f #f #f) => #f)  ; 所有都为假，返回假


(check (when #t 1) => 1)

(check (when #f 1 ) => #<unspecified>)

(check (when (> 3 1) 1 ) => 1)

(check (when (> 1 3) 1 ) => #<unspecified>)

(check (let ((x 1)) x) => 1)

(check (let ((x 1) (y 2)) (+ x y)) => 3)

(check (let ((x 1))
         (let ((x 2))
           x)) => 2)

(check (let ((x 1))
         (if (> x 0)
             x
             -x)) => 1)

(check (let loop ((n 5) (acc 0))
         (if (zero? n)
           acc
           (loop (- n 1) (+ acc n)))) => 15)

(check (let factorial ((n 5))
         (if (= n 1)
           1
           (* n (factorial (- n 1))))) => 120)

(check (let sum ((a 3) (b 4))
         (+ a b)) => 7)

(check (let outer ((x 2))
         (let inner ((y 3))
           (+ x y))) => 5)

;; 基础测试 - 验证顺序绑定的功能
(check
  (let* ((x 10)
         (y (+ x 5)))  ; y 可以使用之前定义的 x
    y)
  => 15)

;; 多层嵌套绑定
(check
  (let* ((a 1)
         (b (+ a 1))
         (c (* b 2)))
    (* a b c))
  => 8)  ; 1 * 2 * 4 = 8 

;; 变量更新
(check
  (let* ((x 1)
         (x (+ x 1))
         (x (* x 2)))
    x)
  => 4)

;; 空绑定
(check
  (let* ()
    "result")
  => "result")

;; 作用域测试
(check
  (let* ((x 10))
    (let* ((y (+ x 5)))
      (+ x y)))
  => 25)

;; 嵌套 let*
(check
  (let* ((a 1)
         (b 2))
    (let* ((c (+ a b))
           (d (* a b c)))
      (+ a b c d)))
  => 12)  ; 1 + 2 + 3 + (1*2*3) = 12 

;; 闭包测试
(check
  (let ((x 1))
    (let* ((y (+ x 1))
           (z (lambda () (+ x y))))
      (z)))
  => 3)

;; 副作用测试
(check
  (let ((counter 0))
    (let* ((a (begin (set! counter (+ counter 1)) 10))
           (b (begin (set! counter (+ counter 1)) 20)))
      counter))
  => 2)

;; 类型混用
(check
  (let* ((s "Hello")
         (len (string-length s))
         (lst (cons len (cons s '()))))
    lst)
  => '(5 "Hello"))

;; 错误用法测试
(check-catch 'unbound-variable
  (let* ((x y)  ; y 未定义
         (y 10))
    x))

;; 复杂表达式
(check
  (let* ((x (if #t 10 20))
         (y (let* ((a x)
                   (b (+ a 5)))
              (+ a b))))
    y)
  => 25)  ; 10 + (10+5) = 25

(define (test-letrec)
  (letrec ((even?
             (lambda (n)
               (if (= n 0)
                   #t
                   (odd? (- n 1)))))
            (odd?
             (lambda (n)
               (if (= n 0)
                   #f
                   (even? (- n 1))))))
    (list (even? 10) (odd? 10))))

(check (test-letrec) => (list #t #f))

(check-catch 'wrong-type-arg
  (letrec ((a 1) (b (+ a 1))) (list a b)))

(check
  (letrec* ((a 1) (b (+ a 1))) (list a b))
  => (list 1 2))

(check (let-values (((ret) (+ 1 2))) (+ ret 4)) => 7)
(check (let-values (((a b) (values 3 4))) (+ a b)) => 7)

(check (and-let* ((hi 3) (ho #f)) (+ hi 1)) => #f)
(check (and-let* ((hi 3) (ho #t)) (+ hi 1)) => 4)

(check
  (do ((i 0 (+ i 1)))
      ((= i 5) i))
  => 5)

(check
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((= i 5) sum))
  => 10)

(check
  (do ((i 0))
      ((= i 5) i)
    (set! i (+ i 1)))
  => 5)

(check
  (let1 vec (make-vector 5)
    (do ((i 0 (+ i 1)))
        ((= i 5) vec)
      (vector-set! vec i i)))
  => #(0 1 2 3 4))

(define* (hi a (b 32) (c "hi")) (list a b c))

(check (hi 1) => '(1 32 "hi"))
(check (hi :b 2 :a 3) => '(3 2 "hi"))
(check (hi 3 2 1) => '(3 2 1))

(define* (g a (b a) (k (* a b)))
  (list a b k))

(check (g 3 4) => '(3 4 12))
(check (g 3 4 :k 5) => '(3 4 5))

(let ()
  (define-values (value1 value2) (values 1 2))
  (check value1 => 1)
  (check value2 => 2))

(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(check (pare? (kons 1 2)) => #t)
(check (pare? (cons 1 2)) => #f)
(check (kar (kons 1 2)) => 1)
(check (kdr (kons 1 2)) => 2)

(check
 (let ((k (kons 1 2)))
   (set-kar! k 3)
   (kar k))
  => 3)

(define-record-type :person
  (make-person name age)
  person?
  (name get-name set-name!)
  (age get-age))

(check (person? (make-person "Da" 3)) => #t)
(check (get-age (make-person "Da" 3)) => 3)
(check (get-name (make-person "Da" 3)) => "Da")
(check
  (let ((da (make-person "Da" 3)))
    (set-name! da "Darcy")
    (get-name da))
  => "Darcy")

#|
number?
判断一个对象是否是数（包括整数、浮点数、有理数、复数）。

语法
----
(number? obj)

参数
----
obj : any
    任意类型的对象。

返回值
-----
boolean?
    如果 obj 是数值类型（整数、浮点数、有理数、复数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (number? 123))          ; 整数
(check-true (number? 123.456))      ; 浮点数
(check-true (number? 1/2))          ; 有理数
(check-true (number? 1+2i))         ; 复数
(check-false (number? "123"))       ; 字符串
(check-false (number? #t))          ; 布尔值
(check-false (number? 'symbol))     ; 符号
(check-false (number? '(1 2 3)))    ; 列表

#|
complex?
判断一个对象是否是复数（包括整数、浮点数、有理数、复数）。

语法
----
(complex? obj)

参数
----
obj : any
    任意类型的对象。

返回值
-----
boolean?
    如果 obj 是数值类型（整数、浮点数、有理数、复数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (complex? 1+2i))        ; 复数
(check-true (complex? 123))         ; 整数也是复数
(check-true (complex? 123.456))     ; 浮点数也是复数
(check-true (complex? 1/2))         ; 有理数也是复数
(check-false (complex? "123"))      ; 字符串
(check-false (complex? #t))         ; 布尔值
(check-false (complex? 'symbol))    ; 符号
(check-false (complex? '(1 2 3)))   ; 列表

#|
real?
判断一个对象是否实数（包括整数、浮点数、有理数）。

语法
----
(real? obj)

参数
----
obj : any
    任意类型的对象。

返回值
-----
boolean?
    如果 obj 是数值类型（整数、浮点数、有理数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (real? 123))            ; 整数
(check-true (real? 123.456))        ; 浮点数
(check-true (real? 1/2))            ; 有理数
(check-false (real? 1+2i))          ; 复数
(check-false (real? "123"))         ; 字符串
(check-false (real? #t))            ; 布尔值
(check-false (real? 'symbol))       ; 符号
(check-false (real? '(1 2 3)))      ; 列表

#|
rational?
判断一个对象是否是有理数（包括整数、有理数）。

语法
----
(rational? obj)

参数
----
obj : any
    任意类型的对象。

返回值
-----
boolean?
    如果 obj 是数值类型（整数、有理数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (rational? 123))        ; 整数
(check-true (rational? 1/2))        ; 有理数
(check-false (rational? 123.456))   ; 浮点数
(check-false (rational? 1+2i))      ; 复数
(check-false (rational? "123"))     ; 字符串
(check-false (rational? #t))        ; 布尔值
(check-false (rational? 'symbol))   ; 符号
(check-false (rational? '(1 2 3)))  ; 列表

#|
integer?
判断一个对象是否是整数（包括整数）。

语法
----
(integer? obj)

参数
----
obj : any
    任意类型的对象。

返回值
-----
boolean?
    如果 obj 是数值类型（整数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (integer? 123))         ; 整数
(check-false (integer? 123.456))    ; 浮点数
(check-false (integer? 1/2))        ; 有理数
(check-false (integer? 1+2i))       ; 复数
(check-false (integer? "123"))      ; 字符串
(check-false (integer? #t))         ; 布尔值
(check-false (integer? 'symbol))    ; 符号
(check-false (integer? '(1 2 3)))   ; 列表
(check-true (exact? 1))
(check-true (exact? 1/2))
(check-false (exact? 0.3))
; (check-true (exact? #e3.0))

(check-false (inexact? 42))             ;整数
(check-false (inexact? 3/4))            ;有理数
(check-true (inexact? 3.14))            ;浮点数
(check-true (inexact? 1.0e3))           ;指数
(check-true (inexact? 1+2i))            ;复数
(check-true (inexact? (+ 1.0 2.0)))     ;运算
(check-true (inexact? +inf.0))          ;特殊数值
;(check-false (inexact? e3))             ;前缀

(let1 zero-int 0
  (check-true (and (integer? zero-int) (zero? zero-int))))
(let1 zero-exact (- 1/2 1/2)
  (check-true (and (exact? zero-exact) (zero? zero-exact))))
(let1 zero-inexact 0.0
  (check-true (and (inexact? zero-inexact) (zero? zero-inexact))))

(check-false (zero? 1+1i))
(check-false (zero? #b11))

(check-catch 'wrong-type-arg (zero? #\A))
(check-catch 'wrong-type-arg (zero? #t))
(check-catch 'wrong-type-arg (zero? #f))

(check-true (positive? 1))
(check-true (positive? 0.1))
(check-true (positive? 1/2))
(check-true (positive? +inf.0))
(check-true (positive? 1+0i))

(check-false (positive? 0))
(check-false (positive? -1))
(check-false (positive? -1.1))
(check-false (positive? -1/2))
(check-false (positive? -inf.0))
(check-false (positive? +nan.0))

(check-catch 'wrong-type-arg (positive? 1+1i))
(check-catch 'wrong-type-arg (positive? #\A))
(check-catch 'wrong-type-arg (positive? #t))
(check-catch 'wrong-type-arg (positive? "not-a-number"))
(check-catch 'wrong-type-arg (positive? 'symbol))
(check-catch 'wrong-type-arg (positive? '(1 2 3)))

(check-true (negative? -1))
(check-true (negative? -0.1))
(check-true (negative? -1/2))
(check-true (negative? -inf.0))
(check-true (negative? -1+0i))

(check-false (negative? 0))
(check-false (negative? 1))
(check-false (negative? 1.1))
(check-false (negative? 1/2))
(check-false (negative? +inf.0))
(check-false (negative? -nan.0))

(check-catch 'wrong-type-arg (negative? -1-1i))
(check-catch 'wrong-type-arg (negative? #\A))
(check-catch 'wrong-type-arg (negative? #t))
(check-catch 'wrong-type-arg (negative? "not-a-number"))
(check-catch 'wrong-type-arg (negative? 'symbol))
(check-catch 'wrong-type-arg (negative? '(1 2 3)))

(check-true (odd? 1))
(check-false (odd? 0))

(check-catch 'wrong-type-arg (odd? 1+i))
(check-catch 'wrong-type-arg (odd? 1.0))
(check-catch 'wrong-type-arg (odd? 0.0))
(check-catch 'wrong-type-arg (odd? #\A))
(check-catch 'wrong-type-arg (odd? #t))
(check-catch 'wrong-type-arg (odd? #f))

(check-true (even? 0))
(check-false (even? 1))

(check-catch 'wrong-type-arg (even? 0.0))
(check-catch 'wrong-type-arg (even? 1.0))
(check-catch 'wrong-type-arg (even? 1+i))
(check-catch 'wrong-type-arg (even? #\A))
(check-catch 'wrong-type-arg (even? #t))
(check-catch 'wrong-type-arg (even? #f))

(check (max 7) => 7)  
(check (max 3.5) => 3.5) 
(check (max 1/3) => 1/3) 
(check (max +inf.0) => +inf.0) 
(check (max -inf.0) => -inf.0) 
(check (nan? (max +nan.0)) => #t) 


(check (max 7 3) => 7)  
(check (max 3.0 7.0) => 7.0)  
(check (max 3 7.0) => 7.0)  
(check (max 7.0 3) => 7.0)  
(check (max 1/2 1/3) => 1/2)  
(check (max 1/3 2/3) => 2/3)  
(check (max +inf.0 7) => +inf.0)  
(check (max 7 +inf.0) => +inf.0)  
(check (max -inf.0 7) => 7.0)  
(check (max 7 -inf.0) => 7.0)  
(check (nan? (max +nan.0 7)) => #t)  
(check (nan? (max 7 +nan.0)) => #t)  

(check (max 7 3 5) => 7)  
(check (max 3.0 7.0 2.0) => 7.0)  
(check (max 7 3.0 5) => 7.0)  
(check (max 1/2 1/3 2/3) => 2/3) 
(check (max +inf.0 7 3) => +inf.0)  
(check (max -inf.0 7 3) => 7.0) 
(check (nan? (max +nan.0 7 3)) => #t)  
(check (nan? (max 7 +nan.0 3)) => #t) 
(check (nan? (max +nan.0 +inf.0 -inf.0)) => #t) 

(check (max 7 3 5) => 7)  
(check (max 3.0 7.0 2.0) => 7.0)  
(check (max 7 3.0 5) => 7.0)  
(check (max 1/2 1/3 2/3) => 2/3) 
(check (max +inf.0 7 3) => +inf.0)  
(check (max -inf.0 7 3) => 7.0) 
(check (nan? (max +nan.0 7 3)) => #t)  
(check (nan? (max 7 +nan.0 3)) => #t) 
(check (nan? (max +nan.0 +inf.0 -inf.0)) => #t) 

(check-catch 'wrong-number-of-args (max))  
(check-catch 'type-error (max 'hello 7))  
(check-catch 'type-error (max "world" 7))  
(check-catch 'type-error (max #t 7))  
(check-catch 'type-error (max #f 7)) 
(check-catch 'type-error (max '(1 3 5) 7)) 
(check-catch 'type-error (max '() 7))  
(check-catch 'type-error (max 1+2i 2))  

(check (min 7) => 7)
(check (min 3.5) => 3.5)
(check (min 1/3) => 1/3)
(check (min +inf.0) => +inf.0)
(check (min -inf.0) => -inf.0)
(check (nan? (min +nan.0)) => #t)

(check (min 7 3) => 3)

(check (min 3.0 7.0) => 3.0)

(check (min 3 7.0) => 3.0)
(check (min 7.0 3) => 3.0)

(check (min 1/2 1/3) => 1/3)
(check (min 1/3 2/3) => 1/3)

(check (min +inf.0 7) => 7.0)
(check (min 7 +inf.0) => 7.0)
(check (min -inf.0 7) => -inf.0)
(check (min 7 -inf.0) => -inf.0)

(check (nan? (min +nan.0 7)) => #t)
(check (nan? (min 7 +nan.0)) => #t)

(check (min 7 3 5) => 3)

(check (min 3.0 7.0 2.0) => 2.0)

(check (min 7 3.0 5) => 3.0)

(check (min 1/2 1/3 2/3) => 1/3)

(check (min +inf.0 7 3) => 3.0)
(check (min -inf.0 7 3) => -inf.0)

(check (nan? (min +nan.0 7 3)) => #t)
(check (nan? (min 7 +nan.0 3)) => #t)
(check (nan? (min +nan.0 +inf.0 -inf.0)) => #t)

(check (min 7 3 5) => 3)

(check (min 3.0 7.0 2.0) => 2.0)

(check (min 7 3.0 5) => 3.0)

(check (min 1/2 1/3 2/3) => 1/3)

(check (min +inf.0 7 3) => 3.0)
(check (min -inf.0 7 3) => -inf.0)

(check (nan? (min +nan.0 7 3)) => #t)
(check (nan? (min 7 +nan.0 3)) => #t)
(check (nan? (min +nan.0 +inf.0 -inf.0)) => #t)

(check-catch 'wrong-number-of-args (min))

(check-catch 'type-error (min 'hello 7))

(check-catch 'type-error (min "world" 7))

(check-catch 'type-error (min #t 7))
(check-catch 'type-error (min #f 7))

(check-catch 'type-error (min '(1 3 5) 7))
(check-catch 'type-error (min '() 7))

(check-catch 'type-error (min 1+2i 2))

#|
+
计算所有给定数字的和。

语法
----
(+ num ...)

参数
----
num : number?
    任意个数字。

返回值
------
number?
    如果没有参数，返回加法单位元 0
    否则，返回其所有参数的和

错误
----
wrong-type-arg
    如果存在任何参数不是数字，抛出错误。

|#

(check (+) => 0)
(check (+ 1) => 1)
(check (+ 1 2) => 3)
(check (+ 1 2 3) => 6)
(check (+ 1 2 3 4) => 10)

(check (+ 1.5 2.5) => 4.0)
(check (+ 0.1 0.2) => 0.30000000000000004)
(check (< (abs (- 3.3 (+ 1.1 2.2))) 1e-15) => #t)

(check (+ 1/2 1/2) => 1)
(check (+ 1/3 1/2) => 5/6)
(check (+ 1/3 1/4 1/5) => 47/60)

(check (+ 1+i 2+2i) => 3.0+3.0i)
(check (+ 3+2i 4-3i) => 7.0-1.0i)
(check (+ 1+i 1) => 2.0+1.0i)
(check (+ 1+i 1/2) => 1.5+1.0i)

(check (+ +inf.0 0.7) => +inf.0)
(check (+ -inf.0 7) => -inf.0)
(check (+ +inf.0 1+i) => +inf.0+1.0i)
(check (nan? (+ +nan.0 1)) => #t)
(check (nan? (+ +inf.0 -inf.0)) => #t)

(check (+ 1.0e308 1.0e308) => +inf.0)
(check (+ -1.0e308 -1.0e308) => -inf.0)
(check (+ #x7fffffffffffffff 1) => #x8000000000000000)

(check-catch 'wrong-type-arg (+ 'hello 7))
(check-catch 'wrong-type-arg (+ "world" 7))
(check-catch 'wrong-type-arg (+ #t 7))
(check-catch 'wrong-type-arg (+ '(1 3 5) 7))
(check-catch 'unbound-variable (+ 1+i 2i))

#|
-
计算所有给定数字的差。

语法
----
(- num ...)

参数
----
num : number?
    一个或多个数字。

返回值
------
number?
    如果只有一个参数，返回其加法逆元（相反数）
    如果有多个参数，返回其所有参数左结合的差

错误
----
wrong-type-arg
    如果存在任何参数不是数字，抛出错误。
wrong-number-of-args
    如果没有提供参数，抛出错误。

|#

(check (- 5) => -5)
(check (- 2 1) => 1)
(check (- 7 2 1) => 4)
(check (- 10 1 2 3) => 4)

(check (- 1.5 0.5) => 1.0)
(check (< (abs(- 2.7 (- 6.98 2.5 1.78))) 1e-15) => #t)

(check (- 2/3 1/3) => 1/3)
(check (- 1/2 1/5 1/7) => 11/70)
(check (- 1 1/3) => 2/3)

(check (- 2+2i 1+i) => 1.0+1.0i)
(check (- 2+i 1) => 1.0+1.0i)
(check (- 1+i 1/2) => 0.5+1.0i)
(check (- 3+4i 0+2i 1+i) => 2.0+1.0i)

(check (- -inf.0 1) => -inf.0)
(check (- +inf.0 1) => +inf.0)
(check (- +inf.0 1+i) => +inf.0-1.0i)
(check (- 1 +inf.0) => -inf.0)
(check (- 1 -inf.0) => +inf.0)
(check (- 1+i +inf.0) => -inf.0+1.0i)
(check (nan? (- +nan.0 0.5)) => #t)
(check (nan? (- 1 2 -nan.0)) => #t)
(check (nan? (- +inf.0 +inf.0)) => #t)

(check-catch 'wrong-number-of-args (-))
(check-catch 'wrong-type-arg (- 'hello 7))
(check-catch 'wrong-type-arg (- "world" 7))
(check-catch 'wrong-type-arg (- #f 7))
(check-catch 'wrong-type-arg (- '(1 3 5) 7))
(check-catch 'unbound-variable (- 1+i 2i))

(check (* 0 0) => 0)
(check (* 0 -1) => 0)
(check (* 0 1) => 0)
(check (* 0 2147483647) => 0)
(check (* 0 -2147483648) => 0)
(check (* 0 2147483648) => 0)
(check (* 0 -2147483649) => 0)
(check (* 0 9223372036854775807) => 0)
(check (* 0 -9223372036854775808) => 0)
(check (* 0 -9223372036854775809) => 0)

(check (* 1 0) => 0)
(check (* 1 -1) => -1)
(check (* 1 1) => 1)
(check (* 1 2147483647) => 2147483647)
(check (* 1 -2147483648) => -2147483648)
(check (* 1 2147483648) => 2147483648)
(check (* 1 -2147483649) => -2147483649)
(check (* 1 9223372036854775807) => 9223372036854775807)
(check (* 1 -9223372036854775808) => -9223372036854775808)
(check (* 1 9223372036854775807) => 9223372036854775807)

(check (* -1 0) => 0)
(check (* -1 -1) => 1)
(check (* -1 1) => -1)
(check (* -1 2147483647) => -2147483647)
(check (* -1 -2147483648) => 2147483648)
(check (* -1 2147483648) => -2147483648)
(check (* -1 -2147483649) => 2147483649)
(check (* -1 9223372036854775807) => -9223372036854775807)
(check (* -1 -9223372036854775808) => -9223372036854775808)
(check (* -1 9223372036854775807) => -9223372036854775807)

(check (* 2147483647 0) => 0)
(check (* 2147483647 -1) => -2147483647)
(check (* 2147483647 1) => 2147483647)
(check (* 2147483647 2147483647) => 4611686014132420609)
(check (* 2147483647 -2147483648) => -4611686016279904256)
(check (* 2147483647 2147483648) => 4611686016279904256)
(check (* 2147483647 -2147483649) => -4611686018427387903)
(check (* 2147483647 9223372036854775807) => 9223372034707292161)
(check (* 2147483647 -9223372036854775808) => -9223372036854775808)

(check (* -2147483648 0) => 0)
(check (* -2147483648 -1) => 2147483648)
(check (* -2147483648 1) => -2147483648)
(check (* -2147483648 2147483647) => -4611686016279904256)
(check (* -2147483648 -2147483648) => 4611686018427387904)
(check (* -2147483648 2147483648) => -4611686018427387904)
(check (* -2147483648 -2147483649) => 4611686020574871552)
(check (* -2147483648 9223372036854775807) => 2147483648)
(check (* -2147483648 -9223372036854775808) => 0)

(check (* 2147483648 0) => 0)
(check (* 2147483648 -1) => -2147483648)
(check (* 2147483648 1) => 2147483648)
(check (* 2147483648 2147483647) => 4611686016279904256)
(check (* 2147483648 -2147483648) => -4611686018427387904)
(check (* 2147483648 2147483648) => 4611686018427387904)
(check (* 2147483648 -2147483649) => -4611686020574871552)
(check (* 2147483648 9223372036854775807) => -2147483648)
(check (* 2147483648 -9223372036854775808) => 0)

(check (* -2147483649 0) => 0)
(check (* -2147483649 -1) => 2147483649)
(check (* -2147483649 1) => -2147483649)
(check (* -2147483649 2147483647) => -4611686018427387903)
(check (* -2147483649 -2147483648) => 4611686020574871552)
(check (* -2147483649 2147483648) => -4611686020574871552)
(check (* -2147483649 -2147483649) => 4611686022722355201)
(check (* -2147483649 9223372036854775807) => -9223372034707292159)
(check (* -2147483649 -9223372036854775808) => -9223372036854775808)

(check (* 9223372036854775807 0) => 0)
(check (* 9223372036854775807 -1) => -9223372036854775807)
(check (* 9223372036854775807 1) => 9223372036854775807)
(check (* 9223372036854775807 2147483647) => 9223372034707292161)
(check (* 9223372036854775807 -2147483648) => 2147483648)
(check (* 9223372036854775807 2147483648) => -2147483648)
(check (* 9223372036854775807 -2147483649) => -9223372034707292159)
(check (* 9223372036854775807 9223372036854775807) => 1)
(check (* 9223372036854775807 -9223372036854775808) => -9223372036854775808)

(check (* -9223372036854775808 0) => 0)
(check (* -9223372036854775808 -1) => -9223372036854775808)
(check (* -9223372036854775808 1) => -9223372036854775808)
(check (* -9223372036854775808 2147483647) => -9223372036854775808)
(check (* -9223372036854775808 -2147483648) => 0)
(check (* -9223372036854775808 2147483648) => 0)
(check (* -9223372036854775808 -2147483649) => -9223372036854775808)
(check (* -9223372036854775808 9223372036854775807) => -9223372036854775808)
(check (* -9223372036854775808 -9223372036854775808) => 0)

(check (floor 1.1) => 1.0)
(check (floor 1) => 1)
(check (floor 1/2) => 0)
(check (floor 0) => 0)
(check (floor -1) => -1)
(check (floor -1.2) => -2.0)

(check (s7-floor 1.1) => 1)
(check (s7-floor -1.2) => -2)

(check (ceiling 1.1) => 2.0)
(check (ceiling 1) => 1)
(check (ceiling 1/2) => 1)
(check (ceiling 0) => 0)
(check (ceiling -1) => -1)
(check (ceiling -1.2) => -1.0)

(check (s7-ceiling 1.1) => 2)
(check (s7-ceiling -1.2) => -1)

(check (truncate 1.1) => 1.0)
(check (truncate 1) => 1)
(check (truncate 1/2) => 0)
(check (truncate 0) => 0)
(check (truncate -1) => -1)
(check (truncate -1.2) => -1.0)

(check (s7-truncate 1.1) => 1)
(check (s7-truncate -1.2) => -1)

(check (round 1.1) => 1.0)
(check (round 1.5) => 2.0)
(check (round 1) => 1)
(check (round 1/2) => 0)
(check (round 0) => 0)
(check (round -1) => -1)
(check (round -1.2) => -1.0)
(check (round -1.5) => -2.0)

(check (floor-quotient 11 2) => 5)
(check (floor-quotient 11 -2) => -6)
(check (floor-quotient -11 2) => -6)
(check (floor-quotient -11 -2) => 5)

(check (floor-quotient 10 2) => 5)
(check (floor-quotient 10 -2) => -5)
(check (floor-quotient -10 2) => -5)
(check (floor-quotient -10 -2) => 5)

(check-catch 'division-by-zero (floor-quotient 11 0))
(check-catch 'division-by-zero (floor-quotient 0 0))

(check (floor-quotient 0 2) => 0)
(check (floor-quotient 0 -2) => 0)

(check (quotient 11 2) => 5)
(check (quotient 11 -2) => -5)
(check (quotient -11 2) => -5)
(check (quotient -11 -2) => 5)

(check-catch 'division-by-zero (quotient 11 0))
(check-catch 'division-by-zero (quotient 0 0))
(check-catch 'wrong-type-arg (quotient 1+i 2))

(check (modulo 13 4) => 1)    
(check (modulo -13 4) => 3)    
(check (modulo 13 -4) => -3)   
(check (modulo -13 -4) => -1)  
(check (modulo 0 5) => 0)    
(check (modulo 0 -5) => 0)    

(check (modulo 13 4.0) => 1.0)     
(check (modulo -13.0 4) => 3.0)    
(check (modulo 13.0 -4.0) => -3.0) 
(check (modulo 1000000 7) => 1)    

;(check-catch 'division-by-zero (modulo 1 0)) 

(check (gcd) => 0)
(check (gcd 0) => 0)
(check (gcd 1) => 1)
(check (gcd 2) => 2)
(check (gcd -1) => 1)

(check (gcd 0 1) => 1)
(check (gcd 1 0) => 1)
(check (gcd 1 2) => 1)
(check (gcd 1 10) => 1)
(check (gcd 2 10) => 2)
(check (gcd -2 10) => 2)

(check (gcd 2 3 4) => 1)
(check (gcd 2 4 8) => 2)
(check (gcd -2 4 8) => 2)

(check (lcm) => 1)
(check (lcm 1) => 1)
(check (lcm 0) => 0)
(check (lcm 32 -36) =>  288)
(check (lcm 32 -36.0) => 288.0)
(check (lcm 2 4) => 4)
(check (lcm 2 4.0) => 4.0)
(check (lcm 2.0 4.0) => 4.0)
(check (lcm 2.0 4) => 4.0)

(check (numerator 1/2) => 1)
(check (numerator 4/5) => 4)
(check (numerator -3/7) => -3)
(check (numerator 5) => 5)
(check (numerator 0) => 0)
(check (numerator (inexact->exact 2.5)) => 5)
(check (denominator 1/2) => 2)
(check (denominator 4/5) => 5)
(check (denominator -3/7) => 7)
(check (denominator 5) => 1)
(check (denominator 0) => 1)
(check (denominator (inexact->exact 2.5)) => 2)
(check (square 2) => 4)

(check (list (exact-integer-sqrt 9)) => (list 3 0))
(check (list (exact-integer-sqrt 5)) => (list 2 1))
(check (list (exact-integer-sqrt 0)) => (list 0 0))
(check-catch 'type-error (exact-integer-sqrt "a"))
(check-catch 'value-error (exact-integer-sqrt -1))
(check-catch 'type-error (exact-integer-sqrt 1.1))
(check-catch 'type-error (exact-integer-sqrt 1+i)) 

(check (number->string 123) => "123")
(check (number->string -456) => "-456")

(check (number->string 123 2) => "1111011")

(check (number->string 123 8) => "173")

(check (number->string 255 16) => "ff")

(check-catch 'wrong-type-arg (number->string 123 'not-a-number))

(check (number->string 1/2) => "1/2")
(check (number->string 1/2 2) => "1/10")
(check (number->string 3/4 2) => "11/100")

(check (number->string 123.456) => "123.456")

(check (number->string 1+2i) => "1.0+2.0i")
(check (number->string 0+2i) => "0.0+2.0i")

(check-true (boolean=? #t #t))
(check-true (boolean=? #f #f))
(check-true (boolean=? #t #t #t))
(check-false (boolean=? #t #f))
(check-false (boolean=? #f #t))

(check-true (symbol? 'foo))
(check-true (symbol? (car '(foo bar))))
(check-true (symbol? 'nil))

(check-false (symbol? "bar"))
(check-false (symbol? #f))
(check-false (symbol? '()))
(check-false (symbol? '123))

(check-catch 'wrong-number-of-args (symbol=? 'a))
(check-catch 'wrong-number-of-args (symbol=? 1))

(check-true (symbol=? 'a 'a))
(check-true (symbol=? 'foo 'foo))
(check-false (symbol=? 'a 'b))
(check-false (symbol=? 'foo 'bar))

(check-true (symbol=? 'bar 'bar 'bar))

(check-true (symbol=? (string->symbol "foo") (string->symbol "foo")))
(check-false (symbol=? (string->symbol "foo") (string->symbol "bar")))

(check-false (symbol=? 1 1))
(check-false (symbol=? 'a 1))
(check-false (symbol=? (string->symbol "foo") 1))

(check-false (symbol=? 'a 'b '()))

(check (symbol->string `MathAgape) => "MathAgape")
(check (symbol->string 'goldfish-scheme) => "goldfish-scheme")

(check (symbol->string (string->symbol "MathApage")) => "MathApage")
(check (symbol->string (string->symbol "Hello World")) => "Hello World")

(check (string->symbol "MathAgape") => `MathAgape)
(check-false (equal? (string->symbol "123") '123))
(check (string->symbol "+") => '+)

(check (string->symbol (symbol->string `MathAgape)) => `MathAgape)

(check (char? #\A) => #t)
(check (char? 1) => #f)

(check (char=? #\A #\A) => #t)
(check (char=? #\A #\A #\A) => #t)
(check (char=? #\A #\a) => #f)

(check (char->integer #\A) => 65)
(check (char->integer #\a) => 97)
(check (char->integer #\newline) => 10)
(check (char->integer #\space) => 32)
(check (char->integer #\tab) => 9)

(check (integer->char 65) => #\A)   
(check (integer->char 97) => #\a)  
(check (integer->char 48) => #\0)
(check (integer->char 36) => #\$)

(check (bytevector 1) => #u8(1))
(check (bytevector) => #u8())
(check (bytevector 1 2 3) => #u8(1 2 3))

(check (bytevector 255) => #u8(255))
(check-catch 'wrong-type-arg (bytevector 256))
(check-catch 'wrong-type-arg (bytevector -1))

(check-true (bytevector? #u8(0)))
(check-true (bytevector? #u8()))

(check (make-bytevector 3 0) => #u8(0 0 0))
(check (make-bytevector 3 3) => #u8(3 3 3))

(check (bytevector-length (bytevector 10 20 30 40)) => 4)
(check (bytevector-length (make-bytevector 7 1)) => 7)

(check (bytevector-u8-ref #u8(5 15 25) 2) => 25)
(check (bytevector-u8-ref (make-bytevector 3 99) 1) => 99)
(check (bytevector-u8-ref (string->utf8 "XYZ") 0) => 88) ;; ASCII 'X'

(let1 bv (bytevector 1 2 3 4 5)
  (check (bytevector-copy bv 1 4) => #u8(2 3 4)))

(check (bytevector-append #u8() #u8()) => #u8())
(check (bytevector-append #u8() #u8(1)) => #u8(1))
(check (bytevector-append #u8(1) #u8()) => #u8(1))

(check (u8-string-length "中文") => 2)
(check (u8-string-length "") => 0)

(check (utf8->string (bytevector #x48 #x65 #x6C #x6C #x6F)) => "Hello")
(check (utf8->string #u8(#xC3 #xA4)) => "ä")
(check (utf8->string #u8(#xE4 #xB8 #xAD)) => "中")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D)) => "👍")

(check-catch 'value-error (utf8->string (bytevector #xFF #x65 #x6C #x6C #x6F)))

(check (string->utf8 "Hello") => (bytevector #x48 #x65 #x6C #x6C #x6F))
(check (utf8->string (string->utf8 "Hello" 1 2)) => "e")
(check (utf8->string (string->utf8 "Hello" 0 2)) => "He")
(check (utf8->string (string->utf8 "Hello" 2)) => "llo")
(check (utf8->string (string->utf8 "Hello" 2 5)) => "llo")

(check-catch 'out-of-range (string->utf8 "Hello" 2 6))

(check (utf8->string (string->utf8 "汉字书写")) => "汉字书写")
(check (utf8->string (string->utf8 "汉字书写" 1)) => "字书写")
(check (utf8->string (string->utf8 "汉字书写" 2)) => "书写")
(check (utf8->string (string->utf8 "汉字书写" 3)) => "写")

(check-catch 'out-of-range (string->utf8 "汉字书写" 4))

(check (string->utf8 "ä") => #u8(#xC3 #xA4))
(check (string->utf8 "中") => #u8(#xE4 #xB8 #xAD))
(check (string->utf8 "👍") => #u8(#xF0 #x9F #x91 #x8D))

(check (string->utf8 "") => #u8())

(check (u8-substring "汉字书写" 0 1) => "汉")
(check (u8-substring "汉字书写" 0 4) => "汉字书写")
(check (u8-substring "汉字书写" 0) => "汉字书写")

(check (apply + (list 3 4)) => 7)
(check (apply + (list 2 3 4)) => 9)

(check (values 4) => 4)
(check (values) => #<unspecified>)

(check (+ (values 1 2 3) 4) => 10)

(check (string-ref ((lambda () (values "abcd" 2)))) => #\c)

(check (+ (call/cc (lambda (ret) (ret 1 2 3))) 4) => 10)

(check (call-with-values (lambda () (values 4 5))
                         (lambda (x y) x))
       => 4)

(check (*) => 1)
(check (call-with-values * -) => -1)

(check
  (receive (a b) (values 1 2) (+ a b))
  => 3)

(guard (condition
         (else
          (display "condition: ")
          (write condition)
          (newline)
          'exception))
  (+ 1 (raise 'an-error)))
; PRINTS: condition: an-error

(guard (condition
         (else
          (display "something went wrong")
          (newline)
          'dont-care))
 (+ 1 (raise 'an-error)))
; PRINTS: something went wrong

(with-input-from-string "(+ 1 2)"
  (lambda ()
    (let ((datum (read))) 
      (check-true (list? datum))
      (check datum => '(+ 1 2)))))

(check (eof-object) => #<eof>)

(check-true ((compose not zero?) 1))
(check-false ((compose not zero?) 0))

(check (let1 x 1 x) => 1)
(check (let1 x 1 (+ x 1)) => 2)

(let1 add1/add (lambda* (x (y 1)) (+ x y))
  (check (add1/add 1) => 2)
  (check (add1/add 0) => 1)
  (check (add1/add 1 2)=> 3))

(define add3
  (typed-lambda
    ((i integer?) (x real?) z)
    (+ i x z)))

(check (add3 1 2 3) => 6)
(check-catch 'type-error (add3 1.2 2 3))

(check-report)

(check (make-list 3 #\a) => (list #\a #\a #\a))
(check (make-list 3) => (list #f #f #f))

(check (make-list 0) => (list ))

(check-true (pair? '(a . b)))
(check-true (pair? '(a b c)))

(check-false (pair? '()))
(check-false (pair? '#(a b)))

(check-true (list? '()))
(check-true (list? '(a)))
(check-true (list? '(a b c)))
(check-true (list? '(1 . 2)))
(check-true (list? '(1 2 . 3)))

(check-true (list? '((a) (b) (c))))
(check-true (list? '(a (b) c)))

(check-true (list? (let ((x '(1 2 3))) (set-cdr! (cddr x) x) x)))

(check-false (list? #t))
(check-false (list? #f))
(check-false (list? 123))
(check-false (list? "Hello"))
(check-false (list? '#(1 2 3))) 
(check-false (list? '#()))
(check-false (list? '12345))

(check (null? '()) => #t)
(check (null? '(1)) => #f)
(check (null? '(1 2)) => #f)

#|
car
一个序对由两部分组成：
    car （第一个元素）
    cdr （第二个元素）
结构表示为： (car . cdr)
car 是 S7 Scheme 内置的 R7RS 定义的函数，用于获取序对的第一个元素。

语法
----
(car pair)

参数
----
pair : pair?
    必须是一个序对（非空列表或显式点对）。

返回值
-----
序对的 car 部分（即第一个元素）。

错误
----
如果参数不是序对（如空列表 '() ），抛出 wrong-type-arg 错误。

|#

(check (car '(a b c . d)) => 'a)
(check (car '(a b c)) => 'a)

(check-catch 'wrong-type-arg (car '()))

(check (cdr '(a b c . d)) => '(b c . d))
(check (cdr '(a b c)) => '(b c))
  
(check-catch 'wrong-type-arg (cdr '()))

(check (caar '((a . b) . c)) => 'a)

(check-catch 'wrong-type-arg (caar '(a b . c)))
(check-catch 'wrong-type-arg (caar '()))

(check (list-ref (cons '(1 2) '(3 4)) 1) => 3)

(check (list-ref '(a b c) 2) => 'c)

(check-catch 'wrong-type-arg (list-ref '() 0))

(check-catch 'out-of-range (list-ref '(a b c) -1))
(check-catch 'out-of-range (list-ref '(a b c) 3))

(check (length ()) => 0)
(check (length '(a b c)) => 3)
(check (length '(a (b) (c d e))) => 3)

(check (length 2) => #f)
(check (length '(a . b)) => -1)

(check (append '(a) '(b c d)) => '(a b c d))
(check (append '(a b) 'c) => '(a b . c))

(check (append () 'c) => 'c)
(check (append) => '())

(check (reverse '()) => '())
(check (reverse '(a)) => '(a))
(check (reverse '(a b)) => '(b a))

(check (map square (list 1 2 3 4 5)) => '(1 4 9 16 25))

(check
  (let ((v (make-vector 5)))
    (for-each (lambda (i) (vector-set! v i (* i i)))
              (iota 5))
    v)
  => #(0 1 4 9 16))

(check
  (let ((v (make-vector 5 #f)))
    (for-each (lambda (i) (vector-set! v i (* i i)))
              (iota 4))
    v)
  => #(0 1 4 9 #f))

(check
  (let ((v (make-vector 5 #f)))
    (for-each (lambda (i) (vector-set! v i (* i i)))
              (iota 0))
    v)
  => #(#f #f #f #f #f))

(check (memq #f '(1 #f 2 3)) => '(#f 2 3))
(check (memq 'a '(1 a 2 3)) => '(a 2 3))
(check (memq 2 '(1 2 3)) => '(2 3))

(check (memq 2.0 '(1 2.0 3)) => #f)
(check (memq 2+0i '(1 2+0i 3)) => #f)

(define num1 3)
(define num2 3)
(check (memq num1 '(3 num2)) => '(3 num2))
(check (memq 3 '(num1 num2)) => #f)
(check (memq 'num1 '(num1 num2)) => '(num1 num2))

(check (memq (+ 1 1) '(1 2 3)) => '(2 3))

(check (memv 2 '(1 2 3)) => '(2 3))
(check (memv 2.0 '(1 2.0 3)) => '(2.0 3))
(check (memv 2+0i '(1 2+0i 3)) => '(2+0i 3))

(check (memv 2 '(1 2.0 3)) => #f)
(check (memv 2 '(1 2+0i 3)) => #f)

#|
member
在列表中搜索指定元素，若存在则返回从该元素开始的子列表，否则返回 #f

语法
----
(member item list)

参数
----
item:any
    待查找的元素（支持任意类型，包括数字、字符串、点对、列表等）。
list:list
    被搜索的列表（可为空列表）。

返回值
-----
list
    若 item 在 list 中，返回从第一个匹配项开始直到列表末尾的子列表。
#f
    若未找到或 list 为空，返回 #f。

错误
----
wrong-type-arg
    若 list 不是有效列表（如非列表结构），可能引发类型错误。

额外信息
----
使用 equal? 进行元素比较（支持复杂类型如字符串 "1"、点对 (1 . 2) 和列表 (1 2)）。
匹配时返回 原始列表的尾部片段（保留原内存结构），而非复制新列表。

|#

(check-catch 'wrong-type-arg (member 0 "text"))

(check (member 2 '(1 2 3)) => '(2 3))

(check (member 0 '(1 2 3)) => #f)
(check (member 0 '()) => #f)
 
(check (member "1" '(0 "1" 2 3)) => '("1" 2 3))
(check (member '(1 . 2) '(0 (1 . 2) 3)) => '((1 . 2) 3))
(check (member '(1 2) '(0 (1 2) 3)) => '((1 2) 3))

(check (string? "MathAgape") => #t)
(check (string? "") => #t)

(check (string? 'MathAgape) => #f)
(check (string? #/MathAgape) => #f)
(check (string? 123) => #f)
(check (string? '(1 2 3)) => #f)

(check (string->list "MathAgape")
  => '(#\M #\a #\t #\h #\A #\g #\a #\p #\e))

(check (string->list "") => '())

(check
  (list->string '(#\M #\a #\t #\h #\A #\g #\a #\p #\e))
  => "MathAgape")

(check (list->string '()) => "")

(check (string-length "MathAgape") => 9)
(check (string-length "") => 0)

(check
  (catch 'wrong-type-arg
    (lambda () (string-length 'not-a-string))
    (lambda args #t))
  =>
  #t)

(check (string-ref "MathAgape" 0) => #\M)
(check (string-ref "MathAgape" 2) => #\t)

(check-catch 'out-of-range (string-ref "MathAgape" -1))
(check-catch 'out-of-range (string-ref "MathAgape" 9))
(check-catch 'out-of-range (string-ref "" 0))

(check (string-append "Math" "Agape") => "MathAgape")

(check (string-append) => "")

(check (make-vector 1 1) => (vector 1))
(check (make-vector 3 'a) => (vector 'a 'a 'a))

(check (make-vector 0) => (vector ))
(check (vector-ref (make-vector 1) 0) => #<unspecified>)

(check (vector 'a 'b 'c) => #(a b c))
(check (vector) => #())

(check (vector-append #(0 1 2) #(3 4 5)) => #(0 1 2 3 4 5))

(check (vector? #(1 2 3)) => #t)
(check (vector? #()) => #t)
(check (vector? '(1 2 3)) => #f)

(check (vector-length #(1 2 3)) => 3)
(check (vector-length #()) => 0)

(let1 v #(1 2 3)
  (check (vector-ref v 0) => 1)
  (check (v 0) => 1)
  
  (check (vector-ref v 2) => 3)
  (check (v 2) => 3))

(check-catch 'out-of-range (vector-ref #(1 2 3) 3))
(check-catch 'out-of-range (vector-ref #() 0))
  
(check-catch 'wrong-type-arg (vector-ref #(1 2 3) 2.0))
(check-catch 'wrong-type-arg (vector-ref #(1 2 3) "2"))

(define my-vector #(0 1 2 3))
(check my-vector => #(0 1 2 3))

(check (vector-set! my-vector 2 10) => 10)
(check my-vector => #(0 1 10 3))

(check-catch 'out-of-range (vector-set! my-vector 4 10))

(check (vector->list #()) => '())
(check (vector->list #() 0) => '())

(check-catch 'out-of-range (vector->list #() 1))

(check (vector->list #(0 1 2 3)) => '(0 1 2 3))
(check (vector->list #(0 1 2 3) 1) => '(1 2 3))
(check (vector->list #(0 1 2 3) 1 1) => '())
(check (vector->list #(0 1 2 3) 1 2) => '(1))

(check (list->vector '(0 1 2 3)) => #(0 1 2 3))
(check (list->vector '()) => #())

#|
open-input-string
将一个字符串转换为输入端口

语法
----
(open-input-string string)

参数
----
string : string?
    一个字符串对象

返回值
-----
port
    一个文本输入端口，该端口会从给定的字符串中读取字符。
    注意：如果在端口使用期间修改了原始字符串，其行为是未定义的。

|#

;; eof on empty
(let1 port (open-input-string "")
  (check (eof-object? (read-char port)) => #t))

;; read-char
(let1 port (open-input-string "abc")
  (check (read-char port) => #\a)
  (check (read-char port) => #\b)
  (check (read-char port) => #\c)
  (check (eof-object? (read-char port)) => #t))

;; read-char, Unicode (Not Support)
(let1 port (open-input-string "λμ") ; #\x03bb #\x03bc
  (check (read-char port) => #\xce)
  (check (read-char port) => #\xbb)
  (check (read-char port) => #\xce)
  (check (read-char port) => #\xbc))

;; read-string, Unicode
(let1 port (open-input-string "λμ")
  (check (read-string 2 port) => "λ")
  (check (read-string 2 port) => "μ"))

#|
open-output-string
创建一个字符串输出端口用于累积字符

语法
----
(open-output-string)

返回值
-----
port
    返回一个新的文本输出端口，所有写入该端口的字符会被累积，
    可通过 get-output-string 函数获取累积的字符串。

|#

;; empty
(let1 port (open-output-string)
  (check (get-output-string port) => ""))

(let1 port (open-output-string)
  (display "abc" port)
  (check (get-output-string port) => "abc"))

(let1 port (open-output-string)
  (display "λμ" port)
  (check (get-output-string port) => "λμ"))

#|
get-output-string
获取输出端口累积的字符串

语法
----
(get-output-string port)

参数
----
port : port?
    必须是由 open-output-string 创建的输出端口

返回值
-----
string?
    返回一个字符串，包含按输出顺序累积到端口的所有字符。
    注意：如果修改返回的字符串，其行为是未定义的。

错误
----
wrong-type-arg
    如果 port 参数不是由 open-output-string 创建的端口，抛出错误。
|#

(let1 port (open-output-string)
  (display "xyz" port)
  (check (get-output-string port) => "xyz"))

(let1 port (open-input-string "ERROR")
  (check-catch 'wrong-type-arg (get-output-string port)))
