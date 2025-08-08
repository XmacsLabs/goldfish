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
        (scheme base))
      
#|
car
car 是 Scheme 内置函数，用于获取序对的第一个元素（第一个分量）。该函数是 R7RS 标准的基本列表操作函数之一。

语法
----
(car pair)

参数
----
pair : pair?
可以是序对（即非空列表或显式点对），不能是空列表或其他对象。

返回值
------
任意类型
返回序对的第一个元素（car部分）。根据不同的序对内容，返回类型可以是
符号、数字、列表、点对等任何对象。

说明
----
1. car 是 pair? 谓词的基本操作之一，与 cdr 配合使用处理序对数据
2. 当应用于列表时，返回列表的第一个元素
3. 适用于所有序对数据：不论是点对 (a . b) 还是非空列表 (a b c ...)

错误处理
--------
wrong-type-arg
当参数不是序对（如空列表 '()、数字、字符串等）时抛出错误。

|#

(check (car '(a b c . d)) => 'a)
(check (car '(a b c)) => 'a)
;; car 边界条件测试
(check (car '(a)) => 'a)                       ; 单元素列表测试
(check (car '(1)) => 1)                        ; 单元素数字测试
(check (car '(#t)) => #t)                      ; 单元素布尔值测试  
(check (car '("hello")) => "hello")            ; 单元素字符串测试
(check (car '(42)) => 42)                      ; 单元素整数测试

;; 各种数据类型作为car值测试
(check (car '(123 "text" symbol)) => 123)                     ; 多类型混合，car是整数
(check (car '(#\a #\b #\c)) => #\a)                         ; 字符列表
(check (car '((a b) c d)) => '(a b))                          ; 子列表作为car
(check (car '((((a))))) => '(((a))))                          ; 深度嵌套列表
(check (car '("nested" (list) "structure")) => "nested")      ; 字符串嵌套结构

;; 点对结构boundary测试
(check (car '(a . b)) => 'a)                   ; 普通点对
(check (car (cons 1 2)) => 1)                  ; cons结构
(check (car (cons 'a (cons 'b 'c))) => 'a)     ; 嵌套cons结构

(check-catch 'wrong-type-arg (car '()))

;; car 异常边界测试
(check-catch 'wrong-type-arg (car 123))                    ; 数字不是pair
(check-catch 'wrong-type-arg (car "hello"))                ; 字符串不是pair
(check-catch 'wrong-type-arg (car #t))                     ; 布尔值不是pair
(check-catch 'wrong-type-arg (car #\a))                   ; 字符不是pair
(check-catch 'wrong-type-arg (car #(a b c)))               ; 向量不是pair
(check-catch 'wrong-number-of-args (car))                  ; 参数不足
(check-catch 'wrong-number-of-args (car '(1 2) '(3 4)))    ; 参数过多

;; 补充边界条件测试 - 完善car边界条件

;; 各种数据结构边界测试
(check (car '(symbol)) => 'symbol)
(check (car '(#t #f)) => #t)
(check (car '(42 24)) => 42)
(check (car '(3.14 2.71)) => 3.14)
(check (car '(1/2 2/3)) => 1/2)
(check (car '(1+2i 3+4i)) => 1+2i)
(check (car '(#	ab #\newline)) => #	ab)

;; 嵌套结构和特殊边界值测试
(check (car '((a (b (c))))) => '(a (b (c))))
(check (car '(((1 2) 3) 4)) => '((1 2) 3))
(check (car '(() b c)) => '())
(check (car '('(a b) '(c d))) => ''(a b))
(check (car '(`(a b) `(c d))) => ''(a b))

;; 向量和字节向量作为car值测试
(check (car '(#(1 2 3) #(4 5))) => #(1 2 3))
(check (car '(#u8(255 128) #u8(1 2))) => #u8(255 128))

;; Scheme符号和过程边界测试
(check (car '(procedure? symbol?)) => `procedure?)
(check (car '(+ - * /)) => '+)
(check (car '(sqrt abs) ) => `sqrt)

;; 连续空列表嵌套边界测试
(check (car '((((()))))) => '(((()))))
(check (car '((a (()) b) c)) => '(a (()) b))

;; Unicode和特殊字符边界测试
(check (car '("中文" "world")) => "中文")
(check (car '("🙂" "🚀")) => "🙂")
(check (car '((list 'a 'b) 'c)) => '(list 'a 'b))

;; 函数和过程对象作为car值测试
(check (car '((lambda (x) (* x x)) (lambda (y) (+ y 1)))) => 
       `(lambda (x) (* x x)))

;; 极端边界：现存表达式嵌套
(check (car '((begin 1 2 3) (begin 4 5))) => '(begin 1 2 3))
(check (car '((let ((x 10)) x) (let ((y 20)) y))) => '(let ((x 10)) x))

;; 确保对car函数的精确数据类型边界验证
(check (car '((define f (lambda (x) x)) (define g (lambda (x) x)))) => 
       '(define f (lambda (x) x)))

(check-catch 'wrong-type-arg (car #f))
(check-catch 'wrong-type-arg (car '[]))
(check-catch 'wrong-type-arg (car '()))
(check-catch 'wrong-number-of-args (car 42 84))
(check-catch 'wrong-type-arg (car '*))

(check-report)
