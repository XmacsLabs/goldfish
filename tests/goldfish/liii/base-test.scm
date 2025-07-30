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

#|
exact?
判断一个数是否是精确数。

语法
----
(exact? obj)

参数
----
obj : number?
任意数值类型的对象。

返回值
-----
boolean?
如果 obj 是精确数（整数、有理数、精确浮点数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (exact? 1))
(check-true (exact? 1/2))
(check-false (exact? 0.3))
; (check-true (exact? #e3.0))

#|
inexact?
用于判断一个数值是否为不精确值。

语法
----
(inexact? obj)

参数
----
obj : number?
任何数值类型的对象

返回值
-----
boolean?
如果 obj 是不精确数（不精确的浮点数、运算结果中的不精确部分、复数的任何部分是不精确的等）返回 #t，否则返回 #f。

说明
----
1. 整数和有理数（精确分数）通常返回 #f，表示它们是精确的
2. 浮点数和运算中涉及不精确数的表达式通常返回 #t
3. 对于复数，如果实部或虚部任何一部分是不精确的，则返回 #t
4. 特殊数值如无穷大和NaN返回 #t
5. 精确浮点数（使用精确前缀）返回 #f

错误处理
--------
wrong-type-arg
如果参数不是数字类型，抛出错误。

|#

;; 基本测试
(check-false (inexact? 42))             ;整数是精确的
(check-false (inexact? 3/4))            ;有理数是精确的
(check-true (inexact? 3.14))            ;浮点数是不精确的
(check-true (inexact? 1.0e3))           ;科学计数法是不精确的
(check-true (inexact? 1+2i))            ;复数通常是不精确的
(check-true (inexact? +inf.0))          ;特殊数值是不精确的
(check-true (inexact? -inf.0))          ;特殊数值是不精确的
(check-true (inexact? +nan.0))          ;NaN是不精确的

;; 精确值测试
(check-false (inexact? 0))
(check-false (inexact? 1))
(check-false (inexact? -1))
(check-false (inexact? 1000000))
(check-false (inexact? -1000000))
(check-false (inexact? 1/2))
(check-false (inexact? 1/3))
(check-false (inexact? 5/3))
(check-false (inexact? -1/2))
(check-false (inexact? -5/7))

;; 不精确值测试
(check-true (inexact? 0.0))
(check-true (inexact? 1.0))
(check-true (inexact? -1.0))
(check-true (inexact? 0.5))
(check-true (inexact? 3.14159))
(check-true (inexact? -3.14159))
(check-true (inexact? 1e10))
(check-true (inexact? 1.0+0.0i))        ;复数的实部/虚部是浮点数

;; 运算结果测试
(check-true (inexact? (+ 1.0 2.0)))     ;涉及不精确数的运算
(check-false (inexact? (+ 1 2)))        ;纯整数运算返回精确值
(check-true (inexact? (+ 1 2.0)))       ;混合运算返回不精确值
(check-false (inexact? (* 1/2 4)))      ;纯有理数运算返回精确值
(check-true (inexact? (* 0.5 4)))       ;涉及浮点的运算返回不精确值

;; 边界测试
(check-true (inexact? 1.7976931348623157e308))  ;最大浮点数
(check-true (inexact? 2.2250738585072014e-308)) ;最小正规化浮点数

;; 错误测试
(check-catch 'wrong-type-arg (inexact? "not a number"))
(check-catch 'wrong-type-arg (inexact? 'symbol))

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

#|
positive?
判断一个对象是否是正数。

语法
----
(positive? obj)

参数
----
obj : any
实数。

返回值
-----
boolean?
如果 obj 是实数类型，当其为正数时返回 #t，否则返回 #f。

错误
----
wrong-type-arg
如果参数不是实数类型（包括复数和非数值类型）

|#

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

#|
negative?
判断一个对象是否是负数。

语法
----
(negative? obj)

参数
----
obj : real?
实数。

返回值
-----
boolean?
如果 obj 是实数类型，当其为负数时返回 #t，否则返回 #f。

错误
----
wrong-type-arg
如果参数不是实数类型（包括复数和非数值类型）

|#


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

#|
odd?
判断一个整数是否是奇数。

语法
----
(odd? obj)

参数
----
obj : integer?
整数。

返回值
-----
boolean?
如果 obj 是整数类型，当其为奇数时返回 #t，否则返回 #f。

错误
----
wrong-type-arg
如果参数不是整数类型

|#

(check-true (odd? 1))
(check-false (odd? 0))

(check-catch 'wrong-type-arg (odd? 1+i))
(check-catch 'wrong-type-arg (odd? 1.0))
(check-catch 'wrong-type-arg (odd? 0.0))
(check-catch 'wrong-type-arg (odd? #\A))
(check-catch 'wrong-type-arg (odd? #t))
(check-catch 'wrong-type-arg (odd? #f))

#|
even?
判断一个整数是否是偶数。

语法
----
(even? obj)

参数
----
obj : integer?
整数。

返回值
-----
boolean?
如果 obj 是整数类型，当其为偶数时返回 #t，否则返回 #f。

错误
----
wrong-type-arg
如果参数不是整数类型

|#

(check-true (even? 0))
(check-false (even? 1))

(check-catch 'wrong-type-arg (even? 0.0))
(check-catch 'wrong-type-arg (even? 1.0))
(check-catch 'wrong-type-arg (even? 1+i))
(check-catch 'wrong-type-arg (even? #\A))
(check-catch 'wrong-type-arg (even? #t))
(check-catch 'wrong-type-arg (even? #f))

#|
max
返回所有给定实数的最大值。

语法
----
(max num ...)

参数
----
num : real?
任意个实数（大于等于1）。

返回值
------
real?
返回所给所有值的最大值。
如果存在NaN，返回NaN。
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
type-error
如果存在任何参数不是实数，抛出错误。
wrong-number-of-args
如果没有提供参数，抛出错误。
|#

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

(check (max 7 3.0 5/4) => 7.0)  
(check (max 5.0 7/2 8) => 8.0)
(check (max +inf.0 7 3/4) => +inf.0)  
(check (max -inf.0 7 3.0) => 7.0) 
(check (nan? (max +nan.0 7.0 3)) => #t)  
(check (nan? (max 7/3 +nan.0 3)) => #t) 

(check-catch 'wrong-number-of-args (max))  
(check-catch 'type-error (max 'hello 7))  
(check-catch 'type-error (max "world" 7))  
(check-catch 'type-error (max #t 7))  
(check-catch 'type-error (max #f 7)) 
(check-catch 'type-error (max '(1 3 5) 7)) 
(check-catch 'type-error (max '() 7))  
(check-catch 'type-error (max 1+2i 2))  

#|
min
返回所有给定实数的最小值。

语法
----
(min num ...)

参数
----
num : real?
任意个实数（大于等于1）。

返回值
------
real?
返回所给所有值的最小值。
如果存在NaN，返回NaN。
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
type-error
如果存在任何参数不是实数，抛出错误。
wrong-number-of-args
如果没有提供参数，抛出错误。
|#

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

(check (min 7 3.0 15/4) => 3.0)  
(check (min 5.0 7/2 3) => 3.0)
(check (min +inf.0 7 39/4) => 7.0)  
(check (min -inf.0 7 3.0) => -inf.0) 
(check (nan? (min +nan.0 7.0 3)) => #t)  
(check (nan? (min 7/3 +nan.0 3)) => #t) 

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

#|
*
乘法函数，支持整数、浮点数、有理数和复数的乘法运算。

语法
----
(* num ...)

参数
----
num : number?
任意个数字作为乘数。如果没有参数，则返回 1；如果只有一个参数，则返回该参数本身；
如果有多个参数，则依次相乘得到最终结果。

返回值
------
number?
如果没有参数，返回乘法单位元 1
如果只有一个参数，返回该参数本身
如果有多个参数，返回所有参数的乘积

说明
----
支持任意精确度和混合类型的乘法运算：
- 整数乘法：精确计算
- 浮点数乘法：可能出现精度误差
- 有理数乘法：保持精确分数
- 复数乘法：按复数乘法规则计算

错误
----
wrong-type-arg
如果存在任何参数不是数字类型，则抛出此错误
|#

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

#|
abs
返回给定数值的绝对值。

语法
----
(abs num)

参数
----
num : real?
任意实数，包括整数、有理数或浮点数。

返回值
------
real?
输入数值的绝对值，保持输入值的类型精度。

说明
----
1. 对于非负输入返回输入值本身
2. 对于负输入返回其相反数
3. 对于有理数返回有理数绝对值
4. 对于浮点数返回浮点数绝对值
5. 零的绝对值是0

错误处理
--------
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; abs 基本测试
(check (abs 0) => 0)
(check (abs 1) => 1)
(check (abs -1) => 1)
(check (abs 42) => 42)
(check (abs -42) => 42)
(check (abs 0.0) => 0.0)
(check (abs 1.5) => 1.5)
(check (abs -1.5) => 1.5)

;; 有理数测试
(check (abs 1/2) => 1/2)
(check (abs -1/2) => 1/2)
(check (abs 22/7) => 22/7)
(check (abs -22/7) => 22/7)

;; 边界测试
(check (abs 1000000000) => 1000000000)
(check (abs -1000000000) => 1000000000)

;; 零端点测试
(check (abs 0/1) => 0)
(check (abs 1/3) => 1/3)
(check (abs -1/3) => 1/3)

;; 错误处理测试
(check-catch 'wrong-type-arg (abs 1+2i))
(check-catch 'wrong-type-arg (abs "hello"))
(check-catch 'wrong-type-arg (abs 'symbol))
(check-catch 'wrong-number-of-args (abs))
(check-catch 'wrong-number-of-args (abs 1 2 3))


#|
floor
返回不大于给定数的最大整数。

语法
----
(floor num )

参数
----
num : real?
实数

返回值
------
返回不大于给定数的最大整数
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
wrong-type-arg
如果参数不是实数，抛出错误。
wrong-number-of-args
如果参数数量不为一，抛出错误。
|#

(check (floor 1.1) => 1.0)
(check (floor 1) => 1)
(check (floor 1/2) => 0)
(check (floor 0) => 0)
(check (floor -1) => -1)
(check (floor -1.2) => -2.0)
(check-catch 'wrong-type-arg (floor 2+4i))
(check-catch 'wrong-type-arg (floor 'hello'))
(check-catch 'wrong-number-of-args (floor 4 5))
(check (s7-floor 1.1) => 1)
(check (s7-floor -1.2) => -2)

#|
ceiling
返回不小于给定数的最小整数。

语法
----
(ceiling num )

参数
----
num : real?
实数

返回值
------
返回不小于给定数的最小整数
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
wrong-type-arg
如果参数不是实数，抛出错误。
wrong-number-of-args
如果参数数量不为一，抛出错误。
|#

(check (ceiling 1.1) => 2.0)
(check (ceiling 1) => 1)
(check (ceiling 1/2) => 1)
(check (ceiling 0) => 0)
(check (ceiling -1) => -1)
(check (ceiling -1.2) => -1.0)
(check-catch 'wrong-type-arg (ceiling 2+4i))
(check-catch 'wrong-type-arg (ceiling 'hello'))
(check-catch 'wrong-number-of-args (ceiling 4 5))

(check (s7-ceiling 1.1) => 2)
(check (s7-ceiling -1.2) => -1)

#|
truncate
返回在靠近零的方向上最靠近给定数的整数。

语法
----
(truncate num )

参数
----
num : real?
实数

返回值
------
返回在靠近零的方向上最靠近给定数的整数，即正数向下取整，负数向上取整
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
wrong-type-arg
如果参数不是实数，抛出错误。
wrong-number-of-args
如果参数数量不为一，抛出错误。
|#

(check (truncate 1.1) => 1.0)
(check (truncate 1) => 1)
(check (truncate 1/2) => 0)
(check (truncate 0) => 0)
(check (truncate -1) => -1)
(check (truncate -1.2) => -1.0)
(check-catch 'wrong-type-arg (truncate 2+4i))
(check-catch 'wrong-type-arg (truncate 'hello'))
(check-catch 'wrong-number-of-args (truncate 4 5))

(check (s7-truncate 1.1) => 1)
(check (s7-truncate -1.2) => -1)

#|
round
round用于返回最接近给定数的整数。

语法
----
(round num)

参数
----
num :real?
实数值，精确的或非精确的

返回值
------
实数? -> (or (integer? integer)
             (real? real-with-trailing-decimal))
返回最接近给定数的整数，如果两个整数同样接近，则取往远离零的方向取整。
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的。

说明
----
1. 当小数部分等于0.5时，round按照IEEE 754标准（向偶数取整）
   (例如：round(1.5) => 2, round(0.5) => 0, round(2.5) => 2, round(-1.5) => -2)
2. 对于精确值(整数、有理数)返回精确值
   (例如：round(1/3) => 0, round(3/4) => 1)
3. 对于非精确值(浮点数、复数)返回非精确值
   (例如：round(1.1) => 1.0, round(3.9) => 4.0)
4. 对于实部为复数的数值，round会分别对实部和虚部四舍五入

错误处理
--------
wrong-type-arg
如果参数不是实数，抛出错误。
wrong-number-of-args
如果参数数量不为一，抛出错误。

|#

(check (round 1.1) => 1.0)
(check (round 1.5) => 2.0)
(check (round 1) => 1)
(check (round 1/2) => 0)
(check (round 0) => 0)
(check (round -1) => -1)
(check (round -1.2) => -1.0)
(check (round -1.5) => -2.0)

;; 测试四舍五入到最近的整数
(check (round 0) => 0)
(check (round 0.4) => 0.0)
(check (round 0.5) => 0.0)      ; 0.5 -> 0 (IEEE 754向偶数取整)
(check (round 0.6) => 1.0)
(check (round 1.4) => 1.0)
(check (round 1.5) => 2.0)      ; 1.5 -> 2
(check (round 1.6) => 2.0)
(check (round 2.5) => 2.0)      ; 2.5 -> 2 (IEEE 754向偶数取整)
(check (round 3.5) => 4.0)      ; 3.5 -> 4

;; 测试负数情况
(check (round -0.4) => 0.0)
(check (round -0.5) => -0.0)    ; -0.5 -> -0.0 (IEEE 754向偶数取整)
(check (round -0.6) => -1.0)
(check (round -1.4) => -1.0)
(check (round -1.5) => -2.0)    ; -1.5 -> -2
(check (round -2.5) => -2.0)    ; -2.5 -> -2 (IEEE 754向偶数取整)
(check (round -3.5) => -4.0)    ; -3.5 -> -4

;; 测试整数边界
(check (round 2147483647) => 2147483647)
(check (round -2147483648) => -2147483648)

;; 测试有理数情况
(check (round 1/3) => 0)
(check (round 2/3) => 1)
(check (round 3/4) => 1)
(check (round -1/3) => 0)
(check (round -2/3) => -1)
(check (round -3/4) => -1)

;; 测试错误情况
(check-catch 'wrong-type-arg (round "not a number"))
(check-catch 'wrong-type-arg (round 'symbol))
(check-catch 'wrong-type-arg (round 1+2i))  ; 复数目前不支持
(check-catch 'wrong-number-of-args (round))
(check-catch 'wrong-number-of-args (round 1 2))

#|
floor-quotient
用于计算两个数的地板除法，返回向负无穷取整的商。

语法
----
(floor-quotient dividend divisor)

参数
----
dividend : number? - 被除数
divisor : number? - 除数，不能为零

返回值
------
number?
返回一个整数，表示向负无穷方向取整的商。

错误
----
division-by-zero
当除数为零时抛出错误。
wrong-type-arg
当参数不是数字时抛出错误。
|#

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
(check-catch 'wrong-type-arg (floor-quotient 1+i 2))

(check (floor-quotient 0 2) => 0)
(check (floor-quotient 0 -2) => 0)

#|
quotient
用于计算两个数的精确除法商（向零取整）。

语法
----
(quotient dividend divisor)

参数
----
dividend : real? - 被除数
divisor : real? - 除数，不能为零

返回值
------
integer?
返回一个整数，表示向零方向取整的商。

与floor-quotient的区别
-------------
quotient与floor-quotient的主要区别在于对负数除法的处理：
- quotient：向零取整（截断除法），如(quotient -11 2) => -5
- floor-quotient：向负无穷取整，如(floor-quotient -11 2) => -6

错误
----
division-by-zero
当除数为零时抛出错误。
wrong-type-arg
当参数不是数字时抛出错误。
|#

(check (quotient 11 2) => 5)
(check (quotient 11 -2) => -5)
(check (quotient -11 2) => -5)
(check (quotient -11 -2) => 5)

(check (quotient 10 3) => 3)
(check (quotient 10 -3) => -3)
(check (quotient -10 3) => -3)
(check (quotient -10 -3) => 3)

(check (quotient 0 5) => 0)
(check (quotient 0 -5) => 0)
(check (quotient 15 5) => 3)
(check (quotient -15 5) => -3)

(check (quotient 7 7) => 1)
(check (quotient 100 10) => 10)
(check (quotient 1 1) => 1)
(check (quotient -1 1) => -1)

(check (quotient 17 5) => 3)
(check (quotient -17 5) => -3)
(check (quotient 17 -5) => -3)
(check (quotient -17 -5) => 3)

(check-catch 'division-by-zero (quotient 11 0))
(check-catch 'division-by-zero (quotient 0 0))
(check (quotient 10.5 3.0) => 3)
(check (quotient 10.5 -3.0) => -3)
(check (quotient -10.5 3.0) => -3)
(check (quotient -10.5 -3.0) => 3)
(check-catch 'wrong-type-arg (quotient 1+i 2))
(check-catch 'wrong-type-arg (quotient 'hello 2))
(check-catch 'wrong-number-of-args (quotient 10))
(check-catch 'wrong-number-of-args (quotient 5 3 2))

#|
modulo
当除数不为零时，用于计算实数的取模运算（满足恒等式 dividend = quotient*divisor + remainder）。
当除数为零时，返回被除数本身

语法
----
(modulo dividend divisor)

参数
----
dividend : real? - 被除数
divisor : real? - 除数，不能为零

返回值
------
real?
返回 dividend 除以 divisor 的余数，该余数保持与 divisor 相同的符号。
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的。

错误
----
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为二时抛出错误。
|#

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
(check (modulo 1 1) => 0)
(check (modulo 5 5) => 0)
(check (modulo -1 5) => 4)
(check (modulo -5 5) => 0)
(check (modulo 20 7) => 6)
(check (modulo -20 7) => 1)
(check (modulo 20 -7) => -1)
(check (modulo 3 0) => 3)


(check-catch 'wrong-type-arg (modulo 1+i 2))
(check-catch 'wrong-type-arg (modulo 'hello 2))
(check-catch 'wrong-number-of-args (modulo 5))
(check-catch 'wrong-number-of-args (modulo 5 3 2))

#|
gcd
用于计算给定整数的最大公约数。

语法
----
(gcd integer ...)

参数
----
integer : integer? - 整数。接受零个、一个或多个参数。

返回值
------
integer?
返回所有参数的最大公约数。无参数时返回0，单参数时返回该参数的绝对值。

特殊规则
--------
- 无参数时返回0
- 参数中包含0时，忽略0值
- 负数会被取绝对值处理
- 多个参数按顺序计算最大值公约数

错误处理
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

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
(check (gcd 15 20 25) => 5)
(check (gcd 6 9 12 15) => 3)
(check (gcd 0 4 6) => 2)
(check (gcd 1 2 3 4 5) => 1)
(check (gcd 12 18) => 6)
(check (gcd 18 12) => 6)
(check (gcd 21 35) => 7)
(check (gcd 0 5) => 5)
(check (gcd 15 0) => 15)
(check (gcd -6 8) => 2)
(check (gcd 12 -9) => 3)

(check-catch 'wrong-type-arg (gcd 1.5))
(check-catch 'wrong-type-arg (gcd 2.3))
(check-catch 'wrong-type-arg (gcd 1+i))
(check-catch 'wrong-type-arg (gcd 'hello))
(check-catch 'wrong-type-arg (gcd 1 2+i 3))
(check-catch 'wrong-type-arg (gcd 1.5 2.5))

#|
lcm
计算给定有理数的最小公倍数。

语法
----
(lcm reals ...)

参数
----
reals : real?
实数。接受零个、一个或多个参数。

返回值
------
返回最小公倍数。
无参数时返回1，单参数时返回该参数本身的绝对值，参数中包含0时返回0。
如果参数中含有不精确值，返回值也是不精确的。

错误处理
--------
type-error
当参数不是实数时抛出错误。
|#

;; 基本测试
(check (lcm) => 1)
(check (lcm 1) => 1)
(check (lcm 0) => 0)
(check (lcm -1) => 1)

(check (lcm 2 3) => 6)
(check (lcm 4 6) => 12)
(check (lcm 12 18) => 36)
(check (lcm -6 8) => 24)
(check (lcm 0 5) => 0)

(check (lcm 2 4 5) => 20)
(check (lcm 6 8 9) => 72)

(check (lcm 5/2 4) => 20)  
(check (lcm 32.0 -36.0) => 288.0)
(check (lcm 32.0 -36) => 288.0)
(check-catch 'type-error (lcm 1+2i))


#|
numerator
返回有理数的分子部分。

语法
----
(numerator q)

参数
----
q : rational?
有理数。

返回值
------
integer?
返回有理数的分子部分。
对于整数，分子是整数本身；对于有理数a/b，返回a。

错误处理
--------
wrong-type-arg
当参数不是有理数时抛出错误。
|#

;; numerator测试
(check (numerator 1/2) => 1)
(check (numerator 4/5) => 4)
(check (numerator -3/7) => -3)
(check (numerator 5) => 5)
(check (numerator 0) => 0)
(check (numerator (inexact->exact 2.5)) => 5)

;; 补充numerator测试
(check (numerator 42) => 42)
(check (numerator -42) => -42)
(check (numerator 1/3) => 1)
(check (numerator 10/5) => 2)
(check (numerator -4/8) => -1)
(check (numerator 0) => 0)

#|
denominator
返回有理数的分母部分。

语法
----
(denominator q)

参数
----
q : rational?
有理数。

返回值
------
integer?
返回有理数的分母部分。
对于整数，分母是1；对于有理数a/b，返回b，b总是正整数。

错误处理
--------
wrong-type-arg
当参数不是有理数时抛出错误。
|#

;; denominator测试
(check (denominator 1/2) => 2)
(check (denominator 4/5) => 5)
(check (denominator -3/7) => 7)
(check (denominator 5) => 1)
(check (denominator 0) => 1)
(check (denominator (inexact->exact 2.5)) => 2)

;; 补充denominator测试  
(check (denominator 42) => 1)
(check (denominator -42) => 1)
(check (denominator 1/3) => 3)
(check (denominator 10/5) => 1)
(check (denominator -4/8) => 2)
(check (denominator (inexact->exact 5.5)) => 2)
(check (denominator (inexact->exact 0.25)) => 4)

#|
exp
计算指数函数 e^n。

语法
----
(exp n)

参数
----
n : number?
可选的数值参数，指数值。

返回值
------
number?
e的n次幂值。

说明
----
1. 计算自然指数函数
2. e ≈ 2.718281828459045
3. 支持整数、有理数、浮点数、复数等各种数值类型

错误处理
--------
wrong-type-arg
当参数不是数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; exp 基本测试
(check (exp 0) => 1)
(check (exp 1) => 2.718281828459045)
(check (exp -1) => 0.36787944117144233)
(check (exp 2) => 7.38905609893065)

;; exp 边界测试
(check (exp 10) => 22026.465794806718)
(check (exp -10) => 4.5399929762484854e-05)
(check (exp 0.5) => 1.6487212707001282)
(check (exp -0.5) => 0.6065306597126334)

;; 错误处理测试
(check (exp 1+2i) => -1.1312043837568135+2.4717266720048188i)
(check-catch 'wrong-type-arg (exp "hello"))
(check-catch 'wrong-number-of-args (exp))

#|
log
计算对数函数。单个参数时计算自然对数(log base e)，两个参数时计算以第二个参数为底的对数。

语法
----
(log z [base])

参数
----
z : number?
必须为数，计算对数值

base : number? 可选
必须为数，表示对数底

返回值
------
number?
对应的对数值

说明
----
1. 单个参数：计算自然对数ln(z) = log_e(z)
2. 两个参数：计算log_base(z) = log(z)/log(base)
3. 支持各种数值类型
4. 注意参数必须为正数且不等于1

错误处理
--------
out-of-range
当z <= 0或base <= 0时抛出错误。
wrong-type-arg
当参数类型错误时抛出错误。
wrong-number-of-args
当参数数量不为1或2个时抛出错误。
|#

;; log 基本自然对数测试
(check (log 1) => 0.0)
(check (log (exp 1)) => 1.0)
(check (log 2) => 0.6931471805599453)

;; log 双参数对数测试
(check (log 100 10) => 2)
(check (log 8 2) => 3)
(check (log 16 2) => 4)

;; log 通用对数测试
(check (log 10 10) => 1)
(check (log 100 10) => 2)
(check (log 1 10) => 0)

;; log 有理数测试
(check (log 2 4) => 1/2)
(check (log 1/2 2) => -1.0)
(check (log 9 3) => 2)

;; log 浮点数对数测试
(check (log 2.718281828459045) => 1.0)
(check (log 0.1 10) =>  -0.9999999999999998) ;返回值是个不精确数

;; 相互验证测试
(check (log (exp 3)) => 3.0)
(check (exp (log 5)) => 4.999999999999999)

;; 错误处理测试
(check (log 0) => -inf.0+3.141592653589793i) ; log(0) = -∞ + πi
(check (log -1) => 0+3.141592653589793i) ; log(-1) = πi
(check (log 3 1) => +inf.0) ; log(3, 1) = 0
(check-catch 'out-of-range (log 10 0))
(check-catch 'wrong-type-arg (log "a"))
(check-catch 'wrong-number-of-args (log))
(check-catch 'wrong-number-of-args (log 12 4 5))


#|
sin
计算给定角度的正弦值。

语法
----
(sin radians)

参数
----
radians : number?
以弧度为单位的角度值。

返回值
------
real?
返回弧度角度的正弦值，当输入值为实数时值域为[-1, 1]。

说明
----
1. 计算正弦函数sin(x)
2. 角度必须以弧度为单位
3. 支持整数、有理数、浮点数、复数等各种数值类型
4. 返回值精确度与输入值类型保持一致

错误处理
--------
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; sin 基本测试
(check (sin 0) => 0)
(check (sin (/ pi 2)) => 1.0)
(check (sin pi) => 1.2246467991473532e-16)
(check (sin (* 2 pi)) => -2.4492935982947064e-16)
(check (sin (/ pi 4)) => 0.7071067811865475)

;; 特殊角度测试
(check (sin (/ pi 6)) => 0.49999999999999994)
(check (sin (* -1 (/ pi 2))) => -1.0)
(check (sin (* 3 (/ pi 2))) => -1.0)

;; 边界测试
(check (sin 1000) => 0.8268795405320025)
(check (sin 0.001) => 9.999998333333417e-4)
(check (sin -0.001) => -9.999998333333417e-4)

;; 复数测试
(check (sin 1+2i) => 3.165778513216168+1.9596010414216063i)

;; 错误处理测试
(check-catch 'wrong-type-arg (sin "hello"))
(check-catch 'wrong-number-of-args (sin))
(check-catch 'wrong-number-of-args (sin 1 2))

#|
cos
计算给定角度的余弦值。

语法
----
(cos radians)

参数
----
radians : number?
以弧度为单位的角度值。

返回值
------
real?
返回弧度角度的余弦值，当输入值为实数时值域为[-1, 1]。

说明
----
1. 计算余弦函数cos(x)
2. 角度必须以弧度为单位
3. 支持整数、有理数、浮点数、复数等各种数值类型
4. 返回值精确度与输入值类型保持一致


错误处理
--------
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; cos 基本测试
(check (cos 0) => 1)
(check (cos (/ pi 2)) => 6.123233995736766e-17)
(check (cos pi) => -1.0)
(check (cos (* 2 pi)) => 1.0)
(check (cos (/ pi 4)) => 0.7071067811865476)

;; 特殊角度测试
(check (cos (/ pi 3)) => 0.5000000000000001)
(check (cos (/ pi 6)) => 0.8660254037844387)
(check (cos (* -1 (/ pi 3))) => 0.5000000000000001)

;; 边界测试
(check (cos 100) => 0.8623188722876839)


;; 有理数测试
(check (cos 3/4) => 0.7316888688738209)

;； 复数测试
(check (cos 1+2i) => 2.0327230070196656-3.0518977991518i)

;; 错误处理测试
(check-catch 'wrong-type-arg (cos "hello"))
(check-catch 'wrong-number-of-args (cos))
(check-catch 'wrong-number-of-args (cos 1 2))

#|
tan
计算给定角度的正切值。

语法
----
(tan radians)

参数
----
radians : number?
以弧度为单位的角度值。

返回值
------
real?
返回弧度角度的正切值。

说明
----
1. 计算正切函数tan(x) = sin(x)/cos(x)
2. 角度必须以弧度为单位
3. 支持整数、有理数、浮点、复数数等各种数值类型
4. 需要注意tan在π/2 + kπ处的奇点（无定义）


错误处理
--------
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; tan 基本测试
(check (tan 0) => 0)

;; 特殊角度测试
(check (tan (/ pi 3)) => 1.7320508075688767)


;; 有理数测试
(check (tan 1/2) => 0.5463024898437905)

;; 错误处理测试
(check-catch 'wrong-type-arg (tan "hello"))
(check-catch 'wrong-number-of-args (tan))
(check-catch 'wrong-number-of-args (tan 1 2))

#|
asin
计算给定值的反正弦值。

语法
----
(asin x)

参数
----
x : real?
必须在区间[-1, 1]内的实数，表示sin函数的值。

返回值
------
real?
返回x的反正弦值（arcsin），范围在[-π/2, π/2]内。

说明
----
1. 计算反正弦函数arcsin(x)
2. 支持整数、有理数、浮点数等各种数值类型
3. 当|x| > 1时，返回复数值
4. 返回值精确度与输入值类型保持一致

示例
----
(asin 0) => 0.0
(asin 1) => 1.5707963267948966 (π/2)
(asin -1) => -1.5707963267948966 (-π/2)

错误处理
--------
wrong-type-arg
当参数不是实数或超出范围时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; asin 基本测试
(check (asin 0) => 0)
(check (asin 1) => 1.5707963267948966)
(check (asin -1) => -1.5707963267948966)

;; 特殊值测试
(check (asin (/ (sqrt 2) 2)) => 0.7853981633974484)
(check (asin (/ (sqrt 3) 2)) => 1.0471975511965976)

;; 边界测试
(check (asin 0.000001) => 1.0000000000001666e-6)

;; 有理数测试
(check (asin 2/3) => 0.7297276562269664)


;; 错误处理测试
(check-catch 'wrong-type-arg (asin "hello"))
(check-catch 'wrong-number-of-args (asin))
(check-catch 'wrong-number-of-args (asin 1 2))

#|
acos
计算给定值的反余弦值。

语法
----
(acos x)

参数
----
x : real?
必须在区间[-1, 1]内的实数，表示cos函数的值。

返回值
------
real?
返回x的反余弦值（arccos），范围在[0, π]内。

说明
----
1. 计算反余弦函数arccos(x)
2. 支持整数、有理数、浮点数等各种数值类型
3. 当|x| > 1时，返回复数值
4. 返回值精确度与输入值类型保持一致

示例
----
(acos 0) => 1.5707963267948966 (π/2)
(acos 1) => 0.0
(acos -1) => 3.141592653589793 (π)

错误处理
--------
wrong-type-arg
当参数不是实数或超出范围时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; acos 基本测试
(check (acos 0) => 1.5707963267948966)
(check (acos 1) => 0)
(check (acos -1) => 3.141592653589793)
(check (acos -0.5) => 2.0943951023931957)

;; 特殊值测试
(check (acos (/ (sqrt 2) 2)) => 0.7853981633974483)
(check (acos (/ (sqrt 3) 2)) => 0.5235987755982989)

;; 边界测试
(check (acos 0.999999) => 0.0014142136802445852)
(check (acos 0.000001) => 1.5707953267948966)
(check (acos -0.999999) => 3.1401784399095485)

;; 有理数测试
(check (acos 3/4) => 0.7227342478134157)
(check (acos 2/3) => 0.8410686705679303)

;; 错误处理测试
(check-catch 'wrong-type-arg (acos "hello"))
(check-catch 'wrong-number-of-args (acos))
(check-catch 'wrong-number-of-args (acos 1 2))

#|
atan
计算给定值的反正切值，或计算两个值之比值的反正切值。

语法
----
(atan x [y])

参数
----
x : number?
当y未提供时，必须为实数，表示tan函数的值
当y提供时，必须为实数，表示纵坐标

y : real? 可选
表示横坐标的实数

返回值
------
real?
当只有x参数时，返回x的反正切值，范围在(-π/2, π/2)内
当提供x和y参数时，返回y/x的反正切值，范围在(-π, π]内

说明
----
1. 计算反正切函数arctan(x)或arctan(y/x)
2. 角度以弧度为单位返回
3. 双参数形式可以处理所有象限的角度
4. 支持各种数值类型

错误处理
--------
wrong-type-arg
当参数类型错误时抛出错误。
wrong-number-of-args
当参数数量不为1或2个时抛出错误。
|#

;; atan 基本单参数测试
(check (atan 0) => 0)
(check (atan 1) => 0.7853981633974483)
(check (atan -1) => -0.7853981633974483)

;; atan 双参数测试
(check (atan 1 1) => 0.7853981633974483)
(check (atan -1 1) => -0.7853981633974483)
(check (atan 1 -1) => 2.356194490192345)
(check (atan -1 -1) => -2.356194490192345)
(check (atan 0 1) => 0.0)
(check (atan 1 0) => 1.5707963267948966)
(check (atan -1 0) => -1.5707963267948966)

;; 特殊角度测试
(check (atan (/ 1 (sqrt 3))) => 0.5235987755982989)
(check (atan 2 3) => 0.5880026035475675)
(check (atan 3 2) => 0.982793723247329)

;; 有理数测试(atan)
(check (atan 2/3) => 0.5880026035475675)
(check (atan 3/4) => 0.6435011087932844)
(check (atan 4 3) => 0.9272952180016122)

;; 边界测试
(check (atan 1000) => 1.5697963271282298)
(check (atan 0.000001) => 9.999999999996666e-7)
(check (atan -0.000001) => -9.999999999996666e-7)

;; 复数测试
(check (atan 1+2i) => 1.3389725222944935+0.40235947810852507i)

;; 错误处理测试
(check-catch 'wrong-type-arg (atan "hello"))
(check-catch 'wrong-number-of-args (atan))
(check-catch 'wrong-number-of-args (atan 1 2 3))
(check-catch 'wrong-type-arg (atan 1 "hello"))

#|
square
计算给定数值的平方。

语法
----
(square x)

参数
----
x : number?
数值。支持整数、有理数、浮点数等各种数值类型。

返回值
------
返回x的平方值，保持与输入相同的数值类型精度。
对于整数，返回精确的平方值；对于浮点数，返回浮点数平方值。

错误处理
--------
wrong-type-arg
当参数不是数值时抛出错误。
|#

;; square测试
(check (square 2) => 4)
(check (square 0) => 0)
(check (square -2) => 4)
(check (square 5) => 25)
(check (square -5) => 25)
(check (square 1/2) => 1/4)
(check (square -1/3) => 1/9)
(check (square 2.5) => 6.25)
(check (square 0.0) => 0.0)
(check (square 10) => 100)
(check (square 1+2i) => -3+4i)
(check-catch 'wrong-type-arg (square "a"))


;; 补充square边界测试
(check (square 1) => 1)
(check (square -1) => 1)
(check (square 1000) => 1000000)
(check (square 1/100) => 1/10000)
(check (square 0.001) => 0.000001)

#|
exact-integer-sqrt
计算给定非负精确整数的精确平方根。

语法
----
(exact-integer-sqrt n)

参数
----
n : exact?
n是确切的非负整数。

返回值
------
values
返回两个值：
1. 整数r：满足r² ≤ n的最大整数
2. 整数remainder：n - r²，始终为非负

说明
----
该函数专为精确计算设计，要求参数必须是非负的准确整数。
对于完全平方数，remainder将为0；非完全平方数返回最大的整数根和余量。

错误处理
--------
type-error
当参数不是准确的整数时抛出错误。
value-error
当参数是负数时抛出错误。
|#

(check (list (exact-integer-sqrt 9)) => (list 3 0))
(check (list (exact-integer-sqrt 5)) => (list 2 1))
(check (list (exact-integer-sqrt 0)) => (list 0 0))
(check (list (exact-integer-sqrt 1)) => (list 1 0))
(check (list (exact-integer-sqrt 4)) => (list 2 0))
(check (list (exact-integer-sqrt 16)) => (list 4 0))
(check (list (exact-integer-sqrt 2)) => (list 1 1))
(check (list (exact-integer-sqrt 3)) => (list 1 2))
(check (list (exact-integer-sqrt 8)) => (list 2 4))
(check (list (exact-integer-sqrt 25)) => (list 5 0))
(check (list (exact-integer-sqrt 100)) => (list 10 0))
(check (list (exact-integer-sqrt 1000)) => (list 31 39))
(check (list (exact-integer-sqrt 1000000)) => (list 1000 0))
(check-catch 'type-error (exact-integer-sqrt "a"))
(check-catch 'value-error (exact-integer-sqrt -1))
(check-catch 'type-error (exact-integer-sqrt 1.1))
(check-catch 'type-error (exact-integer-sqrt 1+i)) 

#|
number->string
将数值转换为字符串表示。

语法
----
(number->string num)
(number->string num radix)

参数
----
num : number?
要转换为字符串的数值，支持整数、实数、有理数、复数等各种数值类型。

radix : exact?
可选参数，指定转换的进制。必须是精确的整数，范围在2到16之间（包含2和16）。
当不指定时，默认为10进制。

返回值
------
string?
返回给定数值的字符串表示形式。

说明
----
1. 对于整数，返回整数字符串表示
2. 对于实数，返回小数格式的字符串
3. 对于有理数，返回"分子/分母"格式的字符串
4. 对于复数，返回"实部+虚部i"格式的字符串
5. 指定进制时，返回指定进制的字符串表示（仅适用于有理的实数部分）

错误处理
--------
wrong-type-arg
当参数不是数值或进制不是精确的整数时抛出错误。
out-of-range
当进制不在2到16范围内时抛出错误。
wrong-number-of-args
当参数数量不为1或2时抛出错误。
|#

;; 基本整数转换测试
(check (number->string 123) => "123")
(check (number->string 0) => "0")
(check (number->string -456) => "-456")
(check (number->string 2147483647) => "2147483647")
(check (number->string -2147483648) => "-2147483648")

;; 基本进制转换测试
(check (number->string 123 2) => "1111011")
(check (number->string 123 8) => "173")
(check (number->string 255 16) => "ff")
(check (number->string 255 10) => "255")

;; 有理数转换测试
(check (number->string 1/2) => "1/2")
(check (number->string -1/3) => "-1/3")
(check (number->string 22/7) => "22/7")
(check (number->string 0/1) => "0")

;; 有理数进制转换测试
(check (number->string 1/2 2) => "1/10")
(check (number->string 3/4 2) => "11/100")
(check (number->string 1/3 16) => "1/3")

;; 浮点数转换测试
(check (number->string 123.456) => "123.456")
(check (number->string 0.0) => "0.0")
(check (number->string -0.123) => "-0.123")
(check (number->string 1.23e10) => "1.23e+10")
(check (number->string 1.23e-3) => "0.00123")

;; 复数转换测试
(check (number->string 1+2i) => "1.0+2.0i")
(check (number->string 0+2i) => "0.0+2.0i")
(check (number->string -3+4i) => "-3.0+4.0i")
(check (number->string 1.5-2.5i) => "1.5-2.5i")
(check (number->string 0+1i) => "0.0+1.0i")
(check (number->string 0+0i) => "0.0")
(check (number->string 1.0+0.0i) => "1.0")

;; 边界测试
(check (number->string 1 2) => "1")
(check (number->string 0 16) => "0")
(check (number->string -128 16) => "-80")
(check (number->string 1023 2) => "1111111111")

;; 错误处理测试
(check-catch 'wrong-type-arg (number->string 'not-a-number))
(check-catch 'wrong-type-arg (number->string 123 'not-a-number))
(check-catch 'out-of-range (number->string 123 1)) 
(check-catch 'out-of-range (number->string 123 37))
(check-catch 'wrong-type-arg (number->string 123 3.5))
(check-catch 'wrong-number-of-args (number->string))
(check-catch 'wrong-number-of-args (number->string 123 2 3))

#|
boolean=?
比较两个或多个布尔值是否相等。

语法
----
(boolean=? bool1 bool2 . more-bools)

参数
----
bool1, bool2, ... : boolean?
布尔值，可以是 #t (真) 或 #f (假)。

返回值
------
boolean?
如果所有给定的布尔值都相等，则返回 #t (真)，否则返回 #f (假)。

说明
----
1. 至少需要两个参数
2. 所有参数必须都是布尔值 (#t 或 #f)
3. 当所有布尔值相同时返回 #t，否则返回 #f
4. 支持比较两个或多个布尔值

错误处理
--------
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; boolean=? 基本测试
(check (boolean=? #t #t) => #t)
(check (boolean=? #f #f) => #t)
(check (boolean=? #t #f) => #f)
(check (boolean=? #f #t) => #f)
(check (boolean=? 1 #t) => #f)

;; 多参数测试
(check (boolean=? #t #t #t) => #t)
(check (boolean=? #f #f #f) => #t)
(check (boolean=? #t #t #f) => #f)
(check (boolean=? #t #f #t) => #f)
(check (boolean=? #f #t #t) => #f)

;; 边界测试
(check (boolean=? #t #t #t #t #t) => #t)
(check (boolean=? #f #f #f #f #f) => #t)
(check (boolean=? #t #t #f #t #t) => #f)

;; 错误处理测试
(check-catch 'wrong-number-of-args (boolean=?))
(check-catch 'wrong-number-of-args (boolean=? #t))


(check (char? #\A) => #t)
(check (char? 1) => #f)

#|
char=?
比较两个或多个字符是否相等。

语法
----
(char=? char1 char2 . more-chars)

参数
----
char1, char2, ... : char?
字符值。

返回值
------
boolean?
如果所有给定的字符都相等，则返回 #t (真)，否则返回 #f (假)。

说明
----
1. 至少需要两个参数
2. 所有参数必须都是字符
3. 当所有字符相等时返回 #t，否则返回 #f
4. 支持比较两个或多个字符
5. 区分大小写

错误处理
--------
wrong-type-arg
当参数不是字符时抛出错误。
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; char=? 基本测试
(check (char=? #\A #\A) => #t)
(check (char=? #\a #\a) => #t)
(check (char=? #\A #\a) => #f)
(check (char=? #\a #\A) => #f)
(check (char=? #\0 #\0) => #t)
(check (char=? #\9 #\9) => #t)
(check (char=? #\0 #\9) => #f)

;; 特殊字符测试
(check (char=? #\space #\space) => #t)
(check (char=? #\newline #\newline) => #t)
(check (char=? #\tab #\tab) => #t)
(check (char=? #\space #\newline) => #f)

;; 多参数测试
(check (char=? #\A #\A #\A) => #t)
(check (char=? #\a #\a #\a) => #t)
(check (char=? #\A #\A #\a) => #f)
(check (char=? #\a #\b #\c) => #f)

;; 边界测试
(check (char=? #\0 #\0 #\0 #\0 #\0) => #t)
(check (char=? #\A #\A #\A #\A #\a) => #f)
(check (char=? #\z #\z #\z) => #t)
(check (char=? #\! #\! #\!) => #t)

;; 数字字符测试
(check (char=? #\1 #\1) => #t)
(check (char=? #\1 #\! ) => #f)

;; 大小写混合测试
(check (char=? #\a #\b #\c #\d) => #f)
(check (char=? #\A #\B #\C) => #f)

;; 错误处理测试
(check-catch 'wrong-type-arg (char=? 1 #\A))
(check-catch 'wrong-type-arg (char=? #\A 'symbol))
(check-catch 'wrong-type-arg (char=? 123 #\a))
(check-catch 'wrong-number-of-args (char=?))
(check-catch 'wrong-number-of-args (char=? #\A))


#|
char->integer
将字符转换为其对应的码点值。

语法
----
(char->integer char)

参数
----
char : char?
字符。

返回值
------
integer?
字符对应的码点值

说明
----
将字符转换为对应的整数值

错误处理
--------
wrong-type-arg
当参数不是字符时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; char->integer 基本测试
(check (char->integer #\0) => 48)
(check (char->integer #\9) => 57)


;; 字符边界测试
(check (char->integer #\tab) => 9)
(check (char->integer #\newline) => 10)
(check (char->integer #\return) => 13)
(check (char->integer #\backspace) => 8)

;; 特殊字符测试
(check (char->integer #\!) => 33)
(check (char->integer #\@) => 64)
(check (char->integer #\#) => 35)
(check (char->integer #\$) => 36)
(check (char->integer #\%) => 37)

;; 扩展字符测试
(check (char->integer #\~) => 126)
(check (char->integer #\_) => 95)

;; 数字边界测试
(check (char->integer #\A) => 65)
(check (char->integer #\B) => 66)
(check (char->integer #\Z) => 90)
(check (char->integer #\a) => 97)
(check (char->integer #\z) => 122)

;; 错误处理测试
(check-catch 'wrong-type-arg (char->integer 65))
(check-catch 'wrong-type-arg (char->integer "A"))
(check-catch 'wrong-number-of-args (char->integer))
(check-catch 'wrong-number-of-args (char->integer #\A #\B))

#|
integer->char
将整数码点转换为对应的字符。

语法
----
(integer->char n)

参数
----
n : integer?
整数值，必须是有效的码点值，通常范围在0到255之间。
返回值
------
char?
对应的字符

说明
----
1. 将整数转换为对应的字符
4. 与char->integer互逆操作


错误处理
--------
out-of-range
当码点超出有效范围时抛出错误。
wrong-type-arg
当参数不是整数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; integer->char 基本测试
(check (integer->char 65) => #\A)
(check (integer->char 97) => #\a)
(check (integer->char 48) => #\0)
(check (integer->char 57) => #\9)
(check (integer->char 10) => #\newline)
(check (integer->char 32) => #\space)
(check (integer->char 9) => #\tab)

;; 大写和小写字符
(check (integer->char 65) => #\A)
(check (integer->char 90) => #\Z)
(check (integer->char 97) => #\a)
(check (integer->char 122) => #\z)

;; 数字字符
(check (integer->char 48) => #\0)
(check (integer->char 49) => #\1)
(check (integer->char 57) => #\9)

;; 特殊字符测试
(check (integer->char 33) => #\!)
(check (integer->char 64) => #\@)
(check (integer->char 35) => #\#)

;; 边界测试
(check (integer->char 0) => #\null)
(check (integer->char 126) => #\~)

;; 反向验证
(check (integer->char (char->integer #\A)) => #\A)
(check (integer->char (char->integer #\a)) => #\a)
(check (integer->char (char->integer #\0)) => #\0)
(check (char->integer (integer->char 65)) => 65)
(check (char->integer (integer->char 97)) => 97)

;; 错误处理测试
(check-catch 'out-of-range (integer->char -1))
(check-catch 'out-of-range (integer->char 256))
(check-catch 'wrong-type-arg (integer->char 65.0))
(check-catch 'wrong-number-of-args (integer->char))
(check-catch 'wrong-number-of-args (integer->char 65 66))  



#|
bytevector
返回一个新分配的字节向量，其元素包含传递给过程的所有参数。每个参数都必须是一个介于0到255之间的整数，表示字节向量中的一个字节。如果没有提供任何参数，将创建一个空的字节向量。

语法
----
(bytevector byte ...)

参数
----
byte... : integer?
零个或多个介于0到255之间的整数（包含边界），表示字节值。

返回值
------
bytevector?
新创建的字节向量，包含所有参数指定的字节值。

说明
----
1. 可以接受零个或多个参数
2. 每个参数必须在0-255的范围内
3. 无参数时创建空字节向量
4. 参数顺序就是字节向量中元素的顺序

错误处理
--------
wrong-type-arg
当任何参数不是在0-255范围内的整数时抛出错误。
|#

;; bytevector 基本测试
(check (bytevector) => #u8())
(check (bytevector 255) => #u8(255))
(check (bytevector 1 2 3 4) => #u8(1 2 3 4))
(check (bytevector 10 20 30 40 50) => #u8(10 20 30 40 50))

;; 边界测试
(check (bytevector 0) => #u8(0))
(check (bytevector 255) => #u8(255))
(check (bytevector 0 255) => #u8(0 255))

;; 不同长度测试
(check (bytevector) => #u8())
(check (bytevector 15) => #u8(15))
(check (bytevector 85 170) => #u8(85 170))
(check (bytevector 1 2 3 4 5 6 7 8 9 10) => #u8(1 2 3 4 5 6 7 8 9 10))

;; 错误处理测试
(check-catch 'wrong-type-arg (bytevector 256))
(check-catch 'wrong-type-arg (bytevector -1))
(check-catch 'wrong-type-arg (bytevector 123.0))
(check-catch 'wrong-type-arg (bytevector 123 #u8(1 2 3)))

#|
bytevector?
判断一个对象是否为字节向量类型的谓词。

语法
----
(bytevector? obj)

参数
----
obj : any?
任意对象。

返回值
------
boolean?
如果对象是一个字节向量，返回#t；否则返回#f。

说明
----
1. 用于检查对象是否为字节向量类型
2. #u8()形式创建的也是字节向量，即使为空
3. 能够正确识别所有类型的字节向量实例

错误处理
--------
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; bytevector? 基本测试
(check-true (bytevector? #u8()))
(check-true (bytevector? #u8(0)))
(check-true (bytevector? #u8(255)))
(check-true (bytevector? #u8(1 2 3 4 5)))
(check-true (bytevector? (bytevector)))
(check-true (bytevector? (bytevector 1 2 3)))

;; 类型判别测试
(check-true (bytevector? (bytevector 5 15 25)))
(check-false (bytevector? 123))
(check-false (bytevector? "hello"))
(check-false (bytevector? "list"))
(check-false (bytevector? 'symbol))

;; 错误处理测试
(check-catch 'wrong-number-of-args (bytevector?))
(check-catch 'wrong-number-of-args (bytevector? #u8(1 2 3) #u8(4 5 6)))

#|
make-bytevector
创建一个新的字节向量，指定长度和初始值为所有组成字节。

语法
----
(make-bytevector k [fill])

参数
----
k : integer?
必须是非负的精确整数，表示字节向量的长度。

fill : integer? 可选, 默认为0
0到255之间的整数，作为所有字节的初始值。

返回值
------
bytevector?
创建的字节向量，所有元素都设为指定的fill值。

说明
----
1. 可以指定长度和填充值
2. 填充值默认为0，如果提供必须在0-255范围内
3. 特殊字符如#等会被转换为对应的字节值

错误处理
--------
out-of-range
当k小于0时抛出错误。
wrong-type-arg
当任何参数不正确时抛出错误。
wrong-number-of-args
当参数数量不为1或2个时抛出错误。
|#

;; make-bytevector 基本测试
(check (make-bytevector 0) => #u8())
(check (make-bytevector 1) => #u8(0))
(check (make-bytevector 3) => #u8(0 0 0))
(check (make-bytevector 5 42) => #u8(42 42 42 42 42))
(check (make-bytevector 2 255) => #u8(255 255))

;; 不同长度测试
(check (make-bytevector 0 0) => #u8())
(check (make-bytevector 1 128) => #u8(128))
(check (make-bytevector 10 99) => #u8(99 99 99 99 99 99 99 99 99 99))

;; 边界条件测试
(check (make-bytevector 0) => #u8())
(check (make-bytevector 1 0) => #u8(0))
(check (make-bytevector 1 255) => #u8(255))

;; 特殊值测试
(check (make-bytevector 4 0) => #u8(0 0 0 0))
(check (make-bytevector 3 170) => #u8(170 170 170))
(check (make-bytevector 8 255) => #u8(255 255 255 255 255 255 255 255))

;; 错误处理测试
(check-catch 'out-of-range (make-bytevector -5))
(check-catch 'wrong-type-arg (make-bytevector 3 256))
(check-catch 'wrong-type-arg (make-bytevector 2 -1))
(check-catch 'wrong-type-arg (make-bytevector 3.5))
(check-catch 'wrong-type-arg (make-bytevector "hello"))
(check-catch 'wrong-number-of-args (make-bytevector))
(check-catch 'wrong-number-of-args (make-bytevector 1 2 3))



#|
bytevector-length
返回字节向量中的元素个数。

语法
----
(bytevector-length bv)

参数
----
bv : bytevector?
字节向量。

返回值
------
integer?
字节向量中的元素数量。

说明
----
1. 返回字节向量中的字节数
2. 空字节向量返回0
3. 结果是非负的精确整数

错误处理
--------
wrong-type-arg
当参数不是字节向量时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; bytevector-length 基本测试
(check (bytevector-length #u8()) => 0)
(check (bytevector-length #u8(1)) => 1)
(check (bytevector-length #u8(1 2 3)) => 3)
(check (bytevector-length #u8(255)) => 1)
(check (bytevector-length #u8(1 2 3 4 5 6 7 8 9 10)) => 10)

;; 使用不同类型创建的字节向量测试
(check (bytevector-length (bytevector)) => 0)
(check (bytevector-length (bytevector 50 150 250)) => 3)
(check (bytevector-length (make-bytevector 5 42)) => 5)
(check (bytevector-length (make-bytevector 10 0)) => 10)
(check (bytevector-length (make-bytevector 0)) => 0)
(check (bytevector-length "hello") => 5)
(check (bytevector-length 123) => #f)
(check (bytevector-length 'symbol) => #f)

;; 错误处理测试

(check-catch 'wrong-number-of-args (bytevector-length))
(check-catch 'wrong-number-of-args (bytevector-length #u8(1 2 3) #u8(4 5)))

#|
bytevector-u8-ref
返回字节向量中指定索引位置的字节值。

语法
----
(bytevector-u8-ref bv k)

参数
----
bv : bytevector?
字节向量。

k : integer?
非负的精确整数，表示字节索引位置，必须小于字节向量的长度。

返回值
------
integer?
位置k处的字节值，是一个0到255之间的整数。

说明
----
1. 用0基索引访问字节向量中的元素
2. 索引必须是从0到长度减1的非负整数
3. 返回对应位置的字节值

错误处理
--------
type-error
当bv不是字节向量时或k不是整数时抛出错误。
out-of-range
当k小于0或大于等于字节向量长度时抛出错误。
|#

;; bytevector-u8-ref 基本测试
(check (bytevector-u8-ref #u8(5 15 25) 0) => 5)
(check (bytevector-u8-ref #u8(5 15 25) 1) => 15)
(check (bytevector-u8-ref #u8(5 15 25) 2) => 25)
(check (bytevector-u8-ref #u8(255) 0) => 255)
(check (bytevector-u8-ref #u8(0) 0) => 0)


;; 使用其他函数创建的字节向量测试
(check (bytevector-u8-ref (bytevector 10 20 30 40) 0) => 10)
(check (bytevector-u8-ref (bytevector 10 20 30 40) 1) => 20)
(check (bytevector-u8-ref (bytevector 10 20 30 40) 3) => 40)
(check (bytevector-u8-ref (bytevector 200 150 100 50) 2) => 100)

(check (bytevector-u8-ref (make-bytevector 4 99) 0) => 99)
(check (bytevector-u8-ref (make-bytevector 4 99) 3) => 99)
(check (bytevector-u8-ref #u8(1) 0) => 1)  

;; 复杂字节向量测试
(check (bytevector-u8-ref #u8(10 20 30 40 50 60 70 80 90 100) 9) => 100)
(check (bytevector-u8-ref #u8(128 64 32 16 8 4 2 1) 4) => 8)

;; UTF-8转换测试
(check (bytevector-u8-ref (string->utf8 "XYZ") 0) => 88) ;; ASCII 'X'
(check (bytevector-u8-ref (string->utf8 "XYZ") 1) => 89) ;; ASCII 'Y'
(check (bytevector-u8-ref (string->utf8 "A") 0) => 65)

;; 错误处理测试

(check-catch 'wrong-type-arg (bytevector-u8-ref 123 0))
(check-catch 'wrong-type-arg (bytevector-u8-ref "hello" 0))
(check-catch 'wrong-type-arg (bytevector-u8-ref #u8(1 2 3) 1.5))
(check-catch 'out-of-range (bytevector-u8-ref #u8() 0)) ;; empty case
(check-catch 'out-of-range (bytevector-u8-ref #u8(1 2 3) -1))
(check-catch 'out-of-range (bytevector-u8-ref #u8(1 2 3) 3))
(check-catch 'out-of-range (bytevector-u8-ref #u8(1 2 3) 1 3))
(check-catch 'out-of-range (bytevector-u8-ref #u8() 0))
(check-catch 'wrong-number-of-args (bytevector-u8-ref #u8(1 2 3)))


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


#|
symbol?
判断给定的对象是否为符号(symbol)类型

语法
----
(symbol? obj)

参数
----
obj : any
任意类型的对象

返回值
-----
boolean?
如果obj是符号类型则返回#t，否则返回#f

说明
----
符号是Scheme中的基本数据类型之一，用单引号(')前缀表示。
符号在Scheme中通常用作标识符、关键字或枚举值。

|#

(check-true (symbol? 'foo))
(check-true (symbol? (car '(foo bar))))
(check-true (symbol? 'nil))

(check-false (symbol? "bar"))
(check-false (symbol? #f))
(check-false (symbol? '()))
(check-false (symbol? '123))

;; 边界情况测试
(check-true (symbol? '+))
(check-true (symbol? '-))
(check-true (symbol? '*))
(check-true (symbol? '/))
(check-true (symbol? '==))
(check-true (symbol? '=>))

;; 数字开头符号测试
(check-true (symbol? '123abc))
(check-true (symbol? '1a2b3c))

;; 空符号名称测试 (注意：某些scheme系统可能不支持空符号)
(check-true (symbol? (string->symbol "empty-symbol")))

;; 特殊符号测试
(check-true (symbol? 'if))
(check-true (symbol? 'lambda))
(check-true (symbol? 'define))
(check-true (symbol? 'let))
(check-true (symbol? 'begin))

;; 特殊符号格式测试
(check-true (symbol? 'complex_name))
(check-true (symbol? 'symbol_with_underscore))
(check-true (symbol? 'symbol-with-dash))

;; 非符号类型测试
(check-false (symbol? 123))
(check-false (symbol? 123.456))
(check-false (symbol? #\a))
(check-false (symbol? '()))
(check-false (symbol? (list 'a 'b 'c)))
(check-false (symbol? (vector 'a 'b 'c)))

;; 字符串转换测试
(check-true (symbol? (string->symbol "test")))
(check-true (symbol? (string->symbol "complex-symbol-with-numbers")))

#|
symbol=?
判断给定符号是否相等

语法
----
(symbol=? symbol1 symbol2 ...)

参数
----
symbol1, symbol2, ... : symbol?
一个或多个符号参数

返回值
-----
boolean?
如果所有符号相等则返回#t，否则返回#f

说明
----
符号比较是基于符号的标识符名称进行的。
R7RS中规定symbol=?需要至少两个参数。

|#

;; 基本测试
(check-catch 'wrong-number-of-args (symbol=? 'a))
(check-catch 'wrong-number-of-args (symbol=? 1))

(check-true (symbol=? 'a 'a))
(check-true (symbol=? 'foo 'foo))
(check-false (symbol=? 'a 'b))
(check-false (symbol=? 'foo 'bar))

;; 多参数测试
(check-true (symbol=? 'bar 'bar 'bar))
(check-true (symbol=? 'x 'x 'x 'x))
(check-false (symbol=? 'a 'a 'b))

;; 边界测试
(check-true (symbol=? 'a (string->symbol "a")))
(check-false (symbol=? 'a (string->symbol "A")))

;; 类型错误测试
(check-false (symbol=? 1 1))
(check-false (symbol=? 'a 1))
(check-false (symbol=? (string->symbol "foo") 1))
(check-false (symbol=? 'a 'b '()))

#|
symbol->string
将符号转换为字符串形式

语法
----
(symbol->string symbol)

参数
----
symbol : symbol?
要转换的符号

返回值
-----
string?
符号对应的字符串表示

错误
----
wrong-type-arg
如果参数不是符号类型，抛出错误。

说明
----
symbol->string将符号的标识符转换为等效的字符串表示。
注意区分大小写：符号'abc和'ABC会转换为"abc"和"ABC"的不同字符串。

|#

;; 基本测试
(check (symbol->string 'MathAgape) => "MathAgape")
(check (symbol->string 'goldfish-scheme) => "goldfish-scheme")
(check (symbol->string (string->symbol "Hello World")) => "Hello World")

;; 特殊符号测试
(check (symbol->string '+) => "+")
(check (symbol->string '-) => "-")
(check (symbol->string '*) => "*")
(check (symbol->string '/) => "/")
(check (symbol->string '=>) => "=>")
(check (symbol->string '<=) => "<=")

;; 大小写敏感测试
(check (symbol->string 'ABC) => "ABC")
(check (symbol->string 'abc) => "abc")
(check (symbol->string 'LispCase) => "LispCase")
(check (symbol->string 'camelCase) => "camelCase")

;; 边界测试
(check (symbol->string 'a) => "a")
(check (symbol->string 'x) => "x")
(check (symbol->string 'empty) => "empty")

;; 数字和特殊字符测试
(check (symbol->string (string->symbol "123")) => "123")
(check (symbol->string (string->symbol "123abc")) => "123abc")
(check (symbol->string (string->symbol "symbol_with_underscore")) => "symbol_with_underscore")
(check (symbol->string (string->symbol "symbol-with-dash")) => "symbol-with-dash")
(check (symbol->string (string->symbol "sym$bol")) => "sym$bol")

;; 错误测试
(check-catch 'wrong-type-arg (symbol->string 123))
(check-catch 'wrong-type-arg (symbol->string "symbol"))
(check-catch 'wrong-type-arg (symbol->string #f))
(check-catch 'wrong-type-arg (symbol->string '()))
(check-catch 'wrong-number-of-args (symbol->string 'a 'b))
(check-catch 'wrong-number-of-args (symbol->string))

;; 往返转换测试
(let ((test-symbols '(hello world scheme-prog example complex-identifier my-symbol)))
  (for-each
    (lambda (sym)
      (let ((str (symbol->string sym)))
        (check (string->symbol str) => sym)))
    test-symbols))

#|
string->symbol
将字符串转换为对应的符号

语法
----
(string->symbol string)

参数
----
string : string?
要转换的字符串。可以是空字符串、包含数字、特殊字符的任何字符串内容。

返回值
-----
symbol?
字符串对应的符号标识符。

错误
----
wrong-type-arg
如果参数不是字符串类型，抛出错误。
wrong-number-of-args
如果没有参数或参数数量超过一个，抛出错误。

行为特性
--------
1. 名称转换：字符串内容会原样转换为符号标识符，保持大小写敏感
2. 数字处理：数字字符串（如"123"）转换为数字名称符号，而非数值123
3. 重入一致性：相同字符串多次转换返回同一个符号对象

|#

;; 基本转换测试
(check (string->symbol "MathAgape") => `MathAgape)
(check (string->symbol "hello") => 'hello)
(check (string->symbol "scheme-prog") => 'scheme-prog)

;; 特殊字符转换测试
(check (string->symbol "+") => '+)
(check (string->symbol "-") => '-)
(check (string->symbol "*") => '*)
(check (string->symbol "lambda") => 'lambda)

;; 大小写敏感测试
(check (string->symbol "ABC") => 'ABC)
(check (string->symbol "abc") => 'abc)

;; 数字符号化处理（重要区别）
(check-false (equal? (string->symbol "123") 123))   ; 不是数值

;; 混合字符测试
(check (string->symbol "123abc") => (string->symbol "123abc"))
(check (string->symbol "symbol-with-dash") => (string->symbol "symbol-with-dash"))
(check (string->symbol "symbol_with_underscore") => (string->symbol "symbol_with_underscore"))

;; 错误处理测试  
(check-catch 'wrong-type-arg (string->symbol 123))
(check-catch 'wrong-type-arg (string->symbol 'symbol))
(check-catch 'wrong-number-of-args (string->symbol "a" "b"))
(check-catch 'wrong-number-of-args (string->symbol))

;; 保留字符号化
(check (string->symbol "if") => 'if)
(check (string->symbol "define") => 'define)
(check (string->symbol "let") => 'let)

;; 保持原始往返测试
(check (string->symbol (symbol->string `MathAgape)) => `MathAgape)


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

;; R7RS Equivalence Predicates Tests
;; According to 201_2 task: eqv?, eq?, equal?

#|
eqv?
判断两个对象是否值相等，根据R7RS规范，eqv?在不同类型的数据上表现不同。

语法
----
(eqv? obj1 obj2)

参数
----
obj1, obj2 : any
任意类型的对象

返回值
-----
boolean?
如果两个对象值相等则返回 #t，否则返回 #f。

|#

;; Test eqv? for boolean values
(check-true (eqv? #t #t))
(check-true (eqv? #f #f))
(check-false (eqv? #t #f))

;; Test eqv? for exact numbers
(check-true (eqv? 42 42))
(check-false (eqv? 42 43))

;; Test eqv? for inexact numbers
(check-true (eqv? 3.14 3.14))
(check-false (eqv? 3.14 2.71))

;; Test eqv? for characters
(check-true (eqv? #\a #\a))
(check-false (eqv? #\a #\b))

;; Test eqv? for symbols
(check-true (eqv? 'abc 'abc))
(check-false (eqv? 'abc 'def))

;; Test eqv? for lists (same instance)
(check-true (let ((lst (list 1 2 3)))
              (eqv? lst lst)))

;; Test eqv? for lists (different instances)
(check-false (eqv? (list 1 2 3) (list 1 2 3)))

;; Test eqv? for strings (always #f due to different instances)
(check-false (eqv? "hello" "hello"))
(check-false (eqv? "hello" "world"))

;; Test eqv? for procedures
(check-true (eqv? car car))
(check-false (eqv? car cdr))

;;; eq?

#|
eq?
判断两个对象是否引用相同（对象为同一），即判断对象标识。

语法
----
(eq? obj1 obj2)

参数
----
obj1, obj2 : any
任意类型的对象

返回值
-----
boolean?
如果两个对象是同一对象则返回 #t，否则返回 #f。
|#

;; Test eq? for boolean values
(check-true (eq? #t #t))
(check-true (eq? #f #f))
(check-false (eq? #t #f))

;; Test eq? for exact numbers (may return #f for different instances)
(check-true (eq? 42 42))
(check-false (eq? 42 43))

;; Test eq? for symbols
(check-true (eq? 'abc 'abc))
(check-false (eq? 'abc 'def))

;; Test eq? for lists (not the same instance)
(check-false (eq? (list 1 2 3) (list 1 2 3)))
(check-true (let ((lst (list 1 2 3)))
              (eq? lst lst)))

;; Test eq? for strings (always #f due to different instances)
(check-false (eq? "hello" "hello"))

;; Test eq? for procedures
(check-true (eq? car car))
(check-false (eq? car cdr))

;;; equal?

#|
equal?
判断两个对象结构是否相等，根据R7RS规范，equal?对复杂数据结构进行深比较。

语法
----
(equal? obj1 obj2)

参数
----
obj1, obj2 : any
任意类型的对象

返回值
-----
boolean?
如果两个对象结构相等则返回 #t，否则返回 #f。
|#

;; Test equal? for simple types
(check-true (equal? #t #t))
(check-true (equal? 42 42))
(check-true (equal? 3.14 3.14))
(check-true (equal? "hello" "hello"))
(check-true (equal? 'abc 'abc))

;; Test equal? for lists
(check-true (equal? (list 1 2 3) (list 1 2 3)))
(check-false (equal? (list 1 2 3) (list 1 2 4)))

;; Test equal? for nested lists
(check-true (equal? (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4))))
(check-false (equal? (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 5))))

;; Test equal? for vectors
(check-true (equal? (vector 1 2 3) (vector 1 2 3)))
(check-false (equal? (vector 1 2 3) (vector 1 2 4)))

;; Test equal? for nested vectors
(check-true (equal? (vector (vector 1 2) (vector 3 4)) (vector (vector 1 2) (vector 3 4))))
(check-false (equal? (vector (vector 1 2) (vector 3 4)) (vector (vector 1 2) (vector 3 5))))

;; Test equal? for mixed structures
(check-true (equal? (list 1 (vector 2 3) 4) (list 1 (vector 2 3) 4)))
(check-false (equal? (list 1 (vector 2 3) 4) (list 1 (vector 2 4) 4)))

;; Test equal? for empty structures
(check-true (equal? (list) (list)))
(check-true (equal? (vector) (vector)))

;; Test equal? for different types
(check-false (equal? 42 "hello"))
(check-false (equal? #\a "a"))

(check-report)
