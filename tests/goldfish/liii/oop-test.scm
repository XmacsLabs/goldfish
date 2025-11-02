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

(import (liii oop) (liii check))

#|
@
创建一个部分应用函数，允许指定部分参数，使用下划线 `_` 作为占位符。

语法
----
(@ func arg1 arg2 ...)

参数
----
func : procedure
要部分应用的函数，可以是任何可调用的过程。

args : any
参数列表，可以包含任意数量的下划线 `_` 作为占位符。

返回值
----
procedure
返回一个新的函数，该函数接受与占位符数量相同的参数，
并将这些参数填充到原函数对应的位置。

描述
----
@ 是 (liii oop) 模块中用于函数式编程的核心宏，它实现了部分应用(partial application)
的功能。通过指定部分参数和占位符，可以创建新的函数，这些函数在调用时会自动将
提供的参数填充到占位符位置。

该宏在定义时计算所有非占位符参数的值，这意味着如果这些参数涉及变量引用，
它们会在定义时被捕获，而不是在调用时重新计算。

特点
----
- 支持任意数量的占位符
- 占位符可以出现在参数列表的任意位置
- 支持嵌套使用，可以组合多个 @ 表达式
- 在定义时计算非占位符参数的值
- 保持原函数的语义和行为

注意事项
----
- 占位符必须使用下划线 `_` 符号
- 返回的函数参数数量必须与占位符数量一致
- 非占位符参数在定义时求值，可能捕获当前环境中的变量值
- 支持任意类型的参数，包括过程、列表、符号等
|#

;;; 基础用法测试
(check ((@ + _ 2) 1) => 3)
(check ((@ list 1 _ 3 _ 5) 2 4) => (list 1 2 3 4 5))
(check ((@ list _ _) 'a 'b) => (list 'a 'b))

(check (let ((a 10))
         (define add (@ + (* a 2) _))
         (set! a 100)
         (add 5)) => 25)

(let ((x 5))
  (check ((@ cons (+ x 1) _) 'y) => (cons 6 'y)))

(check (procedure? (@ list 1 2)) => #t)
(check ((@ list 1 2)) => '(1 2))

(check ((@ _ 'a 'b) list) => (list 'a 'b))
(check ((@ map _ '(1 2 3)) (lambda (x) (+ x 1))) => '(2 3 4))
(check ((@ apply _ '(1 2 3)) +) => 6)

(check ((@ (@ + _ 1) _) 2) => 3)
(check ((@ _ _) (@ * _ 2) 3) => 6)
