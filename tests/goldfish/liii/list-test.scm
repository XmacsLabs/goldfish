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
        (liii cut)
        (liii base)
        (only (srfi srfi-1) delete-duplicates))

(check-set-mode! 'report-failed)

#|
xcons
交换参数顺序的cons操作。

语法
----
(xcons obj1 obj2)

参数
----
obj1 : any
任意对象。
obj2 : any
任意对象。

返回值
----
pair
返回 (obj2 . obj1) 组成的对。

注意
----
xcons 是SRFI-1中的一个实用工具函数，便于从右向左构建列表。
当与函数组合使用时特别有用。

错误处理
----
wrong-number-of-args 如果参数数量不为2。
|#
(check (xcons 1 2) => '(2 . 1))
(check (xcons 1 '(2 3)) => '((2 3) . 1))
(check (xcons '(1 2) 3) => '(3 1 2))
(check (xcons '(1 2) '(3 4)) => '((3 4) 1 2))
(check (xcons 1 '()) => '(() . 1))
(check (xcons '() 2) => '(2))
(check (xcons (xcons 1 2) 3) => '(3 2 . 1))

(check-catch 'wrong-number-of-args (xcons 1))
(check-catch 'wrong-number-of-args (xcons 1 2 3))


#|
cons*
以线性更新方式构造列表，支持可变参数。

语法
----
(cons* obj ...)

参数
----
obj : any
任意数量的参数，最少需要一个。

返回值
----
list | pair
- 如果只有一个参数obj，返回该obj本身
- 如果有两个参数obj1 obj2，返回 (obj1 . obj2)
- 如果有三个或以上参数，返回一个列表，其中最后一个参数作为cdr

特性
----
cons* 是一个灵活的列表构造函数，可以处理以下情况：
1. 构造带点对的列表： (cons* 1 2 3) -> (1 2 . 3)
2. 在列表前添加元素： (cons* 1 2 '(3 4)) -> (1 2 3 4)
3. 处理嵌套结构： (cons* 1 '(2 3) 4) -> (1 (2 3) . 4)
4. 处理空列表： (cons* 1 '() 2) -> (1 () . 2)

注意事项
----
- cons* 单次使用时至少需要提供一个参数
- 当最后一个参数是列表时，结果是一个合法的列表
- 当最后一个参数不是列表时，结果是一个点对
- cons* 可以递归使用以构建复杂结构

示例
----
(cons* 1) => 1
(cons* 1 2) => (1 . 2)
(cons* 1 2 3) => (1 2 . 3)
(cons* 1 2 3 4) => (1 2 3 . 4)
(cons* 1 '(2 3)) => (1 2 3)
(cons* 1 2 '(3 4)) => (1 2 3 4)
(cons* 1 2 3 '(4 5)) => (1 2 3 4 5)
(cons* 'a 'b 'c) => '(a b . c)
(cons* 'a () 'b) => '(a () . b)

错误处理
----
wrong-number-of-args 如果没有提供任何参数
|#
(check (cons* 1 2) => '(1 . 2))
(check (cons* 1 2 3) => '(1 2 . 3))
(check (cons* 'a 'b 'c 'd) => '(a b c . d))
(check (cons* '(1 2 3)) => '(1 2 3))
(check (cons* '(1 2) 3 4) => '((1 2) 3 . 4))
(check (cons* 1 2 '(3 4)) => '(1 2 3 4))
(check (cons* '(1) '(2) '(3)) => '((1) (2) . (3)))
(check (cons* 1 '() 3) => '(1 () . 3))
(check (cons* 1 (cons* 2 3)) => '(1 2 . 3))

; More comprehensive cons* tests
(check (cons* 1) => 1)
(check (cons* 'a) => 'a)
(check (cons* '()) => '())
(check (cons* '(1 2 3)) => '(1 2 3))
(check (cons* '(a b) '(c d)) => '((a b) c d))

; Edge cases with lists and atoms
(check (cons* 1 2 '()) => '(1 2))
(check (cons* '() '() '()) => '(() ()))
(check (cons* 1 2 3 4 '()) => '(1 2 3 4))
(check (cons* 1 2 3 4 5) => '(1 2 3 4 . 5))

; Complex nested cases
(check (cons* 1 2 (cons* 3 4 5)) => '(1 2 3 4 . 5))
(check (cons* (cons* 1 2) 3 4) => '((1 . 2) 3 . 4))
(check (cons* 1 (list 2 3) (cons* 4 5)) => '(1 (2 3) 4 . 5))

; Symbol and number combinations
(check (cons* 'a 'b 'c) => '(a b . c))
(check (cons* 1 2 3 4 5 6) => '(1 2 3 4 5 . 6))

; Mixed types
(check (cons* 1 'a 2 'b) => '(1 a 2 . b))
(check (cons* 'hello 42 'world) => '(hello 42 . world))

(check-catch 'wrong-number-of-args (cons*))

#|
iota
生成一个等差数列列表。

语法
----
(iota count)
(iota count start)
(iota count start step)

参数
----
count : exact-nonnegative-integer?
需要生成的元素个数，必须是非负整数。
start : integer?
数列的起始值，默认为0。
step : integer?
数列的步长，默认为1。

返回值
----
list?
返回一个由连续整数组成的列表。

说明
----
iota函数按照SRFI-1规范实现，用于生成等差数列。
- 当只提供一个参数count时，生成从0开始的连续整数序列。
- 当提供count和start参数时，生成从start开始的连续整数序列。
- 当提供count、start和step参数时，生成从start开始，步长为step的整数序列。

错误处理
----
value-error 当count为负数时抛出。
type-error 当任何参数不是整数时抛出。
|#

; Basic iota tests
(check (iota 3) => (list 0 1 2))
(check (iota 3 7) => (list 7 8 9))
(check (iota 2 7 2) => (list 7 9))

; Additional iota edge case tests
(check (iota 0) => '())
(check (iota 1) => '(0))
(check (iota 1 5) => '(5))
(check (iota 1 5 2) => '(5))

; Large count tests
(check (iota 5) => (list 0 1 2 3 4))
(check (iota 5 1 2) => (list 1 3 5 7 9))
(check (iota 3 0 -1) => (list 0 -1 -2))
(check (iota 4 10 -2) => (list 10 8 6 4))

; Zero step (edge case)
(check (iota 3 7 0) => (list 7 7 7))

; Negative start tests
(check (iota 3 -5) => (list -5 -4 -3))
(check (iota 4 -10 2) => (list -10 -8 -6 -4))

; Error handling tests
(check-catch 'value-error (iota -1))
(check-catch 'value-error (iota -5))
(check-catch 'type-error (iota 'a))
(check-catch 'type-error (iota 3 'a))
(check-catch 'type-error (iota 3 5 'a))
(check-catch 'type-error (iota 3.5))
(check-catch 'type-error (iota 3 5.5))
(check-catch 'type-error (iota 3 2 0.5))

#|
circular-list
构造一个包含给定元素的循环列表。

语法
----
(circular-list obj1 obj2 ...)

参数
----
obj1, obj2, ... : any
任意数量的元素，至少需要一个元素。

返回值
----
pair
返回一个循环列表（circular list）。循环列表的最后一个元素的cdr指向列表的第一个元素，形成一个无限循环。

说明
----
circular-list是SRFI-1中定义的构造器函数，用于创建循环列表。
- 函数接受任意数量的元素作为参数，但至少需要一个元素
- 创建的列表是循环的，即最后一个元素的cdr指向第一个元素
- 循环列表可以通过circular-list?进行检测
- 循环列表在结构上是无限长度的，使用中需要特别注意避免无限循环

使用场景
--------
- 重复数据流的生成
- 环形缓冲区的实现
- 循环模式的模拟
- 算法中的周期性结构表示

边界情况
--------
- 当提供单个元素时，创建单个元素的循环列表
- 当提供多个元素时，元素按提供的顺序排列
- 空参数列表会抛出wrong-number-of-args错误

例子
----
(circular-list 'a)        => 循环列表 (a a a ...)
(circular-list 'a 'b 'c)  => 循环列表 (a b c a b c ...)
(circular-list 1 2 3)     => 循环列表 (1 2 3 1 2 3 ...)

错误处理
--------
wrong-number-of-args 当没有提供参数时抛出。
|#

; Basic circular-list tests
(check-true (circular-list? (circular-list 1)))
(check-true (circular-list? (circular-list 1 2)))
(check-true (circular-list? (circular-list 1 2 3)))

; Test element access in circular list
(let ((cl (circular-list 1 2 3)))
  (check (cl 0) => 1)
  (check (cl 1) => 2)
  (check (cl 2) => 3)
  (check (cl 3) => 1)  ; Should cycle back to first element
  (check (cl 4) => 2)
  (check (cl 5) => 3)
  (check (cl 6) => 1))

; Test with different data types
(check-true (circular-list? (circular-list 'a)))
(check-true (circular-list? (circular-list 'a 'b 'c)))
(check-true (circular-list? (circular-list "hello" "world")))
(check-true (circular-list? (circular-list '(1 2) '(3 4))))

; Test edge case with single element
(let ((single (circular-list 'x)))
  (check (single 0) => 'x)
  (check (single 1) => 'x)
  (check (single 100) => 'x))  ; Always return the same element

; Test nested structures
(let ((nested (circular-list '(1 2) '(3) '(4 5 6))))
  (check (nested 0) => '(1 2))
  (check (nested 1) => '(3))
  (check (nested 2) => '(4 5 6))
  (check (nested 3) => '(1 2)))

; Error handling tests
(check-catch 'wrong-number-of-args (circular-list))

#|
list-copy

创建一个新列表，它是输入列表的浅拷贝。

语法
----
(list-copy list)

参数
----
list - 要复制的列表

返回值
------
返回一个新的列表，具有与输入列表相同的元素，但这是一个不同的对象。

描述
----
list-copy 函数创建输入列表的一个浅拷贝。新列表的顶层节点是新的，
但列表中的元素本身不会被复制（浅拷贝）。这使得修改原始列表不会影响拷贝的列表，
但需要注意嵌套列表的深层结构不会被复制。

该函数在 (srfi srfi-1) 模块中实现，并由 (liii list) 重新导出。

示例
----
; 基本列表复制
(list-copy '(1 2 3))      => (1 2 3)
(list-copy '())           => ()

边界条件
--------
- 空列表参数返回空列表
- 非列表参数会触发类型错误异常
- 嵌套列表的子列表为同一引用（浅拷贝特性）

时间和空间复杂度
----------------
- 时间复杂度：O(n)，其中 n 是列表长度
- 空间复杂度：O(n)，需要创建新的列表节点

|#

;; list-copy tests

;; 基本功能测试
(check (list-copy '()) => '())
(check (list-copy '(1 2 3 4 5)) => '(1 2 3 4 5))
(check (list-copy '(a b c d)) => '(a b c d))
(check (list-copy '((1 2) (3 4) (5 6))) => '((1 2) (3 4) (5 6)))

;; 空列表边界条件
(check (list-copy '()) => '())

;; 对象独立性验证 - 确保是浅拷贝
(check-false (eq? (list-copy '(1 2 3)) '(1 2 3)))

;; 突变隔离测试 - 验证列表节点独立性
(let ((orig '(a b c))
      (copy (list-copy '(a b c))))
  (check orig => copy)
  (check-false (eq? orig copy))
  ;; 验证浅拷贝特性
  (let ((mut-copy (list-copy orig)))
    (set-car! mut-copy 'x)
    (check orig => '(a b c))      ; 原始列表不受影响
    (check mut-copy => '(x b c)))) ; 拷贝列表已改变

#|
proper-list?
判断一个对象是否为proper list。

语法
----
(proper-list? obj)

参数
----
obj : any
任意对象

返回值
----
boolean?
如果obj是proper list返回#t，否则返回#f。

说明
----
proper list是指一个符合R7RS规范的传统列表结构，满足以下条件：
1. 空列表'()是proper list
2. 通过cons操作递归构建的以空列表结尾的列表是proper list
3. 不包含循环引用的列表

非proper list的情况包括：
- 点对 (a . b)
- dotted list (a b . c)
- 循环列表
- 非pair和null的对象

使用场景
--------
该函数常用于类型检查，确保输入参数符合列表操作的要求，避免在列表操作函数中发生类型错误。


错误处理
--------
无特殊错误处理，任何类型的对象都能接受。
|#

; 基本功能测试
(check-true (proper-list? (list 1 2)))
(check-true (proper-list? '()))
(check-true (proper-list? '(1 2 3)))

; 非proper list测试
(check-false (proper-list? '(a . b)))
(check-false (proper-list? '(a b . c)))
(check-false (proper-list? (circular-list 1 2 3)))

; 边界条件测试
(check-true (proper-list? '(() ()) )) ; 嵌套列表
(check-true (proper-list? '(a))) ; 单元素列表
(check-false (proper-list? 1)) ; 非列表对象
(check-false (proper-list? 'hello)) ; 符号
(check-false (proper-list? "hello")) ; 字符串

; 复杂结构测试
(check-false (proper-list? '(a b . c))) ; dotted list
(check-true (proper-list? '(a b (c d)))) ; 嵌套proper list
(check-true (proper-list? '(() a b))) ; 前导空列表
(check-true (proper-list? '(a b ()))) ; 尾随空列表元素

; 点和dotted list测试
(check-false (proper-list? '(a . b)))
(check-false (proper-list? '(a b . c)))
(check-false (proper-list? '(a b c . d)))

; 循环列表测试
(let ((lst (list 1 2 3)))
  (set-cdr! (last-pair lst) lst)
  (check-false (proper-list? lst))) ; 手动创建循环列表

#|
dotted-list?
判断一个对象是否为dotted list。

语法
----
(dotted-list? obj)

参数
----
obj : any
任意对象

返回值
----
boolean?
如果obj是dotted list返回#t，否则返回#f。

说明
----
dotted list是指不符合proper list规范但也不是循环列表的列表结构，主要特征包括：
1. 以非空列表结尾的cons结构，如 (a . b) 或 (a b . c)
2. 最终cdr部分不是空列表的列表
3. 单个非pair/非null对象也被视为dotted list
4. 空列表和proper list不是dotted list

使用场景
--------
该函数用于区分proper list与其他非列表结构，特别是在处理列表输入验证时非常有用。

注意
----
- 循环列表不被视为dotted list
- 该函数是proper-list?的补集（对于非循环列表而言）
- 可以用于检测输入是否应该被当作列表处理

错误处理
--------
无特殊错误处理，任何类型的对象都能接受。
|#

; 基本功能测试
(check-true (dotted-list? 1))
(check-true (dotted-list? '(1 . 2)))
(check-true (dotted-list? '(1 2 . 3)))

; 非dotted list测试
(check-false (dotted-list? (circular-list 1 2 3)))
(check-false (dotted-list? '()))
(check-false (dotted-list? '(a)))
(check-false (dotted-list? '(a b)))
(check-false (dotted-list? '(a b (c d))))

; 各种dotted list测试
(check-true (dotted-list? 'a))
(check-true (dotted-list? 'symbol))
(check-true (dotted-list? "string"))
(check-true (dotted-list? 42))
(check-true (dotted-list? '(a . b)))
(check-true (dotted-list? '(a b . c)))
(check-true (dotted-list? '(a b c . d)))
(check-true (dotted-list? '(a b c d . e)))
(check-true (dotted-list? '(() . a)))
(check-true (dotted-list? '(a () . b)))
(check-true (dotted-list? '(a b () . c)))

; 嵌套dotted list测试
(check-true (dotted-list? '((a . b) c . d)))
(check-true (dotted-list? '(a (b . c) . d)))
(check-true (dotted-list? '(a . (b . c))))

; 边界条件测试
(check-false (dotted-list? '(())))
(check-false (dotted-list? '(a ())))
(check-false (dotted-list? '(() a b)))
(check-true (dotted-list? '(a () . b)))

; 复杂结构测试
(check-true (dotted-list? '(a b c . d)))
(check-false (dotted-list? '(a b (c . d)))) ; 嵌套的dotted list但整体是proper

; 与proper-list?的互补性测试
(check-false (dotted-list? '()))
(check-false (dotted-list? '(a)))
(check-false (dotted-list? '(a b)))
(check-false (dotted-list? '(a b c)))
(check-true (dotted-list? '(a . b)))
(check-true (dotted-list? '(a b . c)))
(check-true (dotted-list? 'a))

; 循环列表测试（应返回#f）
(let ((lst (list 1 2 3)))
  (set-cdr! (last-pair lst) lst)
  (check-false (dotted-list? lst))) ; 手动创建循环列表

; 深层嵌套测试
(check-true (dotted-list? '(a (b . c) . d)))
(check-false (dotted-list? '((a b) (c d))))
(check-true (dotted-list? '((a b) . c)))

(check (null-list? '()) => #t)

(check (null-list? '(1 . 2)) => #f)

(check (null-list? '(1 2)) => #f)

(check (null? 1) => #f)

#|
first
获取列表的第一个元素。

语法
----
(first list)

参数
----
list : pair?
非空列表或点对结构。

返回值
----
any
返回列表的第一个元素(car部分)。

说明
----
first函数作为SRFI-1中的选择器函数，相当于(car list)。当应用于非空列表时返回第一个元素，
当应用于点对时返回左侧元素。如果试图在空列表'()上使用first，会抛出错误。

使用场景
--------
常与second至tenth等其他选择器函数配合使用，快速访问列表中特定位置的元素。

错误处理
--------
wrong-type-arg 当应用于空列表时抛出。

|#
(check (first '(1 2 3 4 5 6 7 8 9 10)) => 1)
(check (first '(left . right)) => 'left)
(check (first '(a b c)) => 'a)
(check (first '( 42)) => 42)

(check-catch 'wrong-type-arg (first '()))

; 基本功能测试
(check (first '(a)) => 'a)
(check (first '(a b)) => 'a)
(check (first '(a b c d e)) => 'a)

; 点对结构测试
(check (first '(a . b)) => 'a)
(check (first '((1 2) 3 4)) => '(1 2))

; 嵌套结构测试
(check (first '((a b) (c d))) => '(a b))
(check (first '(() a b)) => '())

; 各种数据类型测试
(check (first '((1 2 3) 4 5)) => '(1 2 3))
(check (first '("a" "b" "c")) => "a")
(check (first '(42 43 44)) => 42)
(check (first '(#t #f #t)) => #t)

; 混合类型测试
(check (first '(1 "hello" a)) => 1)
(check (first '(a 1 #t)) => 'a)

(check (second '(1 2 3 4 5 6 7 8 9 10)) => 2)

(check-catch 'wrong-type-arg (second '(left . right)))
(check-catch 'wrong-type-arg (second '(1)))

(check (third '(1 2 3 4 5 6 7 8 9 10)) => 3)

(check-catch 'wrong-type-arg (third '(1 2)))

(check (fourth '(1 2 3 4 5 6)) => 4)

(check (fifth '(1 2 3 4 5 6 7 8 9 10)) => 5)

(check (sixth '(1 2 3 4 5 6 7 8 9 10)) => 6)

(check (seventh '(1 2 3 4 5 6 7 8 9 10)) => 7)

(check (eighth '(1 2 3 4 5 6 7 8 9 10)) => 8)

(check (ninth '(1 2 3 4 5 6 7 8 9 10)) => 9)

(check (tenth '(1 2 3 4 5 6 7 8 9 10)) => 10)

#| take
从列表开头提取指定数量的元素。

语法
-----
(take list k)

参数
-----
list : list?
源列表，从中提取元素。

k : integer?
要提取的元素数量，必须是非负整数且不超过列表长度。

返回值
------
list
包含指定数量元素的新列表，从原列表的开头开始计数。

注意
-----
take函数会创建新的列表结构，原列表不会被修改。当k等于列表长度时，返回完整列表的拷贝。对于点结尾的列表，行为与proper列表相同，直到遇到dot才停止。

示例
-----
(take '(1 2 3 4) 2) => '(1 2)
(take '(a b c) 0) => '()
(take '(1 (2 3) 4) 2) => '(1 (2 3))
(take '((a b) (c d)) 1) => '((a b))

边界条件
-----
空列表返回空列表
(take '() 0) => '()

错误处理
-----
wrong-type-arg 当list不是列表或k不是整数类型时
out-of-range 当k超过列表长度或k为负数时
|#
(check (take '(1 2 3 4) 3) => '(1 2 3))
(check (take '(1 2 3 4) 4) => '(1 2 3 4))
(check (take '(1 2 3 . 4) 3) => '(1 2 3))

(check-catch 'wrong-type-arg (take '(1 2 3 4) 5))
(check-catch 'wrong-type-arg (take '(1 2 3 . 4) 4))

; 更多边界条件测试
(check (take '() 0) => '())
(check (take '(a) 1) => '(a))
(check (take '(a) 0) => '())
(check (take '((a) (b c) d) 2) => '((a) (b c)))
(check (take '(1 2 3 4 5 6) 3) => '(1 2 3))

; 链式操作测试
(check (take (drop '(1 2 3 4 5) 1) 3) => '(2 3 4))
(check (take (take '(1 2 3 4 5) 4) 2) => '(1 2))

; 大列表测试
(check (take (iota 10) 5) => '(0 1 2 3 4))

; 错误条件测试
(check-catch 'wrong-type-arg (take '(1 2 3) -1))
(check-catch 'wrong-type-arg (take "not a list" 2))
(check-catch 'wrong-type-arg (take '(1 2 3) "not a number"))

#| drop
从列表开头删除指定数量的元素。

语法
----
(drop list k)

参数
----
list : list?
源列表，从中删除元素。

k : integer?
要从开头删除的元素数量，必须是非负整数且不超过列表长度。

返回值
------
list
删除k个元素后的新列表。当k等于列表长度时，返回空列表。

说明
----
drop函数与take函数功能相反，从列表前端删除元素而不是提取。
对于proper list，返回的是剩余部分的列表；
对于dotted list，如果k等于列表长度减一，返回的是最后的非列表元素。

drop和take互为补操作：(take lst k) + (drop lst k) = lst

示例
----
(drop '(1 2 3 4) 2) => '(3 4)
(drop '(a b c) 1) => '(b c)
(drop '(1 (2 3) 4) 2) => '(4)
(drop '() 0) => '()
(drop '(a) 0) => '(a)

与dotted list交互:
(drop '(1 2 . 3) 1) => '(2 . 3)
(drop '(1 2 . 3) 2) => 3

边界条件
--------
- 空列表：返回空列表
- 零个元素删除：返回原列表
- 删除所有元素：返回空列表

错误处理
--------
- out-of-range：当k超过列表长度时
- wrong-type-arg：当list不是列表或k不是整数类型时
|#
(check (drop '(1 2 3 4) 2) => '(3 4))
(check (drop '(1 2 3 4) 4) => '())
(check (drop '(1 2 3 . 4) 3) => 4)

; 基本功能测试
(check (drop '(1 2 3 4 5) 0) => '(1 2 3 4 5))
(check (drop '(1 2 3 4 5) 1) => '(2 3 4 5))
(check (drop '(1 2 3 4 5) 3) => '(4 5))
(check (drop '(1 2 3 4 5) 5) => '())

; 空列表边界条件
(check (drop '() 0) => '())

; 单个元素列表测试
(check (drop '(a) 0) => '(a))
(check (drop '(a) 1) => '())

; 嵌套列表测试
(check (drop '((a b) (c d) (e f)) 1) => '((c d) (e f)))
(check (drop '((a b) (c d) (e f)) 2) => '((e f)))
(check (drop '((a b) (c d) (e f)) 3) => '())

; dotted list边界条件测试
(check (drop '(1 2 . 3) 0) => '(1 2 . 3))
(check (drop '(1 2 . 3) 1) => '(2 . 3))
(check (drop '(1 2 . 3) 2) => 3)
(check (drop '(a b c . d) 1) => '(b c . d))
(check (drop '(a b c . d) 2) => '(c . d))
(check (drop '(a b c . d) 3) => 'd)

; 链式操作测试
(check (drop (drop '(1 2 3 4 5) 1) 2) => '(4 5))
(check (drop (take '(1 2 3 4 5) 4) 2) => '(3 4))
(check (take (drop '(1 2 3 4 5) 2) 2) => '(3 4))

; 大列表测试
(let ((lst (iota 10)))
  (check (drop lst 0) => '(0 1 2 3 4 5 6 7 8 9))
  (check (drop lst 5) => '(5 6 7 8 9))
  (check (drop lst 10) => '()))

; 与take的对称性测试
(let ((lst '(1 2 3 4 5 6 7 8 9 10)))
  (define (symmetry-test lst k)
    (let ((take-part (take lst k))
          (drop-part (drop lst k)))
      (append take-part drop-part)))
  
  (check (symmetry-test lst 0) => lst)
  (check (symmetry-test lst 3) => lst)
  (check (symmetry-test lst 10) => lst)
  (check (symmetry-test lst 5) => lst))

; 错误条件测试  
(check-catch 'out-of-range (drop '(1 2 3 4) 5))
(check-catch 'out-of-range (drop '(1 2 3 . 4) 4))
(check-catch 'out-of-range (drop '(1 2 3 4) -1))
(check-catch 'wrong-type-arg (drop "not a list" 2))
(check-catch 'wrong-type-arg (drop '(1 2 3) "not a number"))

#|
take-right
从列表尾部提取指定数量的元素。

语法
-----
(take-right list k)

参数
-----
list : list?
源列表，从中从尾部提取元素。

k : integer?
要从尾部提取的元素数量，必须是非负整数且不超过列表长度。

返回值
------
list
包含从尾部开始指定数量元素的新列表。

注意
-----
take-right函数与take功能相反，从列表右侧（尾部）提取元素。

示例
-----
(take-right '(1 2 3 4) 2) => '(3 4)
(take-right '(a b c) 1) => '(c)
(take-right '((a b) (c d) (e f)) 2) => '((c d) (e f))

边界条件
-----
空列表返回空列表
(take-right '() 0) => '()

错误处理
-----
out-of-range 当k超过列表长度或k为负数时
|#
(check (take-right '(1 2 3 4) 3) => '(2 3 4))
(check (take-right '(1 2 3 4) 4) => '(1 2 3 4))
(check (take-right '(1 2 3 . 4) 3) => '(1 2 3 . 4))

; 更多边界条件测试
(check (take-right '() 0) => '())
(check (take-right '(a) 1) => '(a))
(check (take-right '(a) 0) => '())
(check (take-right '((a) (b c) d) 2) => '((b c) d))
(check (take-right '(1 2 3 4 5 6) 3) => '(4 5 6))

; 链式操作测试
(check (take-right (drop '(1 2 3 4 5) 1) 3) => '(3 4 5))
(check (take-right (take '(1 2 3 4 5) 4) 2) => '(3 4))

; 大列表测试
(check (take-right (iota 10) 5) => '(5 6 7 8 9))

; 边缘情况测试
(check (take-right '(1) 1) => '(1))
(check (take-right '(1 2) 1) => '(2))

; 错误条件测试
(check-catch 'out-of-range (take-right '(1 2 3 4) 5))
(check-catch 'out-of-range (take-right '(1 2 3 . 4) 4))
(check-catch 'out-of-range (take-right '(1 2 3) -1))
(check-catch 'wrong-type-arg (take-right "not a list" 2))
(check-catch 'wrong-type-arg (take-right '(1 2 3) "not a number"))

#| drop-right
从列表末尾删除指定数量的元素。

语法
----
(drop-right list k)

参数
----
list : list?
源列表，从中从末尾删除元素。

k : integer?
要从末尾删除的元素数量，必须是非负整数且不超过列表长度。

返回值
------
list
删除k个尾部元素后的新列表。当k等于列表长度时，返回空列表。

说明
----
drop-right函数与drop功能相反，从列表右侧（尾部）删除元素而不是前端。对于proper list，返回的是列表前端部分；对于dotted list，如果k等于列表长度减一，返回的是空列表'()。

drop-right和take-right互为补操作：(append (drop-right lst k) (take-right lst k)) = lst

示例
----
(drop-right '(1 2 3 4) 2) => '(1 2)
(drop-right '(a b c) 1) => '(a b)
(drop-right '(a b c) 0) => '(a b c)
(drop-right '(1 (2 3) 4) 2) => '(1 (2 3))

与dotted list交互:
(drop-right '(a b c . d) 1) => '(a b)
(drop-right '(a b . d) 2) => '()

边界条件
--------
- 空列表：返回空列表
- 零个元素删除：返回原列表
- 删除所有元素：返回空列表

错误处理
--------
- out-of-range：当k超过列表长度时
- wrong-type-arg：当list不是列表或k不是整数类型时

对应的SRFI-1规范
----------------
drop-right 严格按照SRFI-1规范实现，处理各种边界情况和错误条件。
|#
(check (drop-right '(1 2 3 4) 2) => '(1 2))
(check (drop-right '(1 2 3 4) 4) => '())
(check (drop-right '(1 2 3 . 4) 3) => '())

; 基本功能测试
(check (drop-right '(1 2 3 4 5) 0) => '(1 2 3 4 5))
(check (drop-right '(1 2 3 4 5) 1) => '(1 2 3 4))
(check (drop-right '(1 2 3 4 5) 3) => '(1 2))
(check (drop-right '(1 2 3 4 5) 5) => '())

; 空列表边界条件
(check (drop-right '() 0) => '())

; 单元素列表测试
(check (drop-right '(a) 0) => '(a))
(check (drop-right '(a) 1) => '())

; 嵌套列表测试
(check (drop-right '((a b) (c d) (e f)) 1) => '((a b) (c d)))
(check (drop-right '((a b) (c d) (e f)) 2) => '((a b)))
(check (drop-right '((a b) (c d) (e f)) 3) => '())

; dotted list边界条件测试
(check (drop-right '(1 2 . 3) 0) => '(1 2))
(check (drop-right '(1 2 . 3) 1) => '(1))
(check (drop-right '(1 2 . 3) 2) => '())
(check (drop-right '(a b c . d) 1) => '(a b))
(check (drop-right '(a b c . d) 2) => '(a))
(check (drop-right '(a b c . d) 3) => '())

; 链式操作测试
(check (drop-right (drop-right '(1 2 3 4 5) 1) 1) => '(1 2 3))
(check (drop-right (take-right '(1 2 3 4 5) 4) 2) => '(2 3))
(check (take-right (drop-right '(1 2 3 4 5) 2) 2) => '(2 3))

; 大列表测试
(check (drop-right (iota 10) 5) => '(0 1 2 3 4))
(check (drop-right (iota 10) 0) => '(0 1 2 3 4 5 6 7 8 9))
(check (drop-right (iota 10) 10) => '())

; 与take-right的对称性测试
(let ((lst '(1 2 3 4 5 6 7 8 9 10)))
  (define (symmetry-test lst k)
    (let ((drop-part (drop-right lst k))
          (take-part (take-right lst k)))
      (append drop-part take-part)))
  
  (check (symmetry-test lst 0) => lst)
  (check (symmetry-test lst 3) => lst)
  (check (symmetry-test lst 10) => lst)
  (check (symmetry-test lst 5) => lst))

; 各种数据类型测试
(check (drop-right '("a" "b" "c" "d") 2) => '("a" "b"))
(check (drop-right '(42 43 44 45) 2) => '(42 43))
(check (drop-right '(#t #f #t #f) 2) => '(#t #f))
(check (drop-right '(a 1 "hello") 1) => '(a 1))

; 错误条件测试
(check-catch 'out-of-range (drop-right '(1 2 3 4) 5))
(check-catch 'out-of-range (drop-right '(1 2 3 4) -1))
(check-catch 'out-of-range (drop-right '(1 2 3 . 4) 4))
(check-catch 'wrong-type-arg (drop-right "not a list" 2))
(check-catch 'wrong-type-arg (drop-right '(1 2 3) "not a number"))

(check (list (split-at '(1 2 3 4 5) 3)) => '((1 2 3) (4 5)))
(check (list (split-at '(1 2 3 4 5) 0)) => '(() (1 2 3 4 5)))

(check-catch 'value-error (split-at '(1 2 3 4 5) 10))
(check-catch 'value-error (split-at '(1 2 3 4 5) -1))

(check (list (split-at '(1 2 3 4 . 5) 0)) => '(() (1 2 3 4 . 5)))
(check (list (split-at '(1 2 3 4 . 5) 3)) => '((1 2 3) (4 . 5)))
(check (list (split-at '(1 2 3 4 . 5) 4)) => '((1 2 3 4) 5))

(check-catch 'value-error (split-at '(1 2 3 4 . 5) 10))
(check-catch 'value-error (split-at '(1 2 3 4 . 5) -1))

(check (list (split-at '() 0)) => '(() ()))
(check-catch 'value-error (split-at '() 10))
(check-catch 'value-error (split-at '() -1))

(check (last-pair '(a b c)) => '(c))
(check (last-pair '(c)) => '(c))

(check (last-pair '(a b . c)) => '(b . c))
(check (last-pair '(b . c)) => '(b . c))

(check-catch 'wrong-type-arg (last-pair '()))

(check (last '(a b c)) => 'c)
(check (last '(c)) => 'c)

(check (last '(a b . c)) => 'b)
(check (last '(b . c)) => 'b)

(check-catch 'wrong-type-arg (last '()))

(check (count even? '(3 1 4 1 5 9 2 5 6)) => 3)

(check (zip '(1 2 3) '(a b c)) => '((1 a) (2 b) (3 c)))
(check (zip '(1 2) '(a b c)) => '((1 a) (2 b)))
(check (zip '(1 2 3) '(a b)) => '((1 a) (2 b)))
(check (zip '(1) '(a) '(x)) => '((1 a x)))
(check (zip '() '(a b)) => '())

(check (zip '(1 2) '(a b)) => '((1 a) (2 b)))

(check (fold + 0 '(1 2 3 4)) => 10)
(check (fold + 0 '()) => 0)

(check-catch 'type-error (fold 0 + '(1 2 3 4)))

(check (fold cons () '(1 2 3 4)) => '(4 3 2 1))

(check
  (fold (lambda (x count) (if (symbol? x) (+ count 1) count))
        0
        '(a b 1 2 3 4))
  => 2)

(check (fold + 0 '(1 2 3) '(4 5 6)) => 21)
(check (fold + 0 '(1 2 3 4) '(10 20 30)) => 66)
(check (fold list '() '(1 2 3) '(a b c)) => '(3 c (2 b (1 a ()))))
(check-catch 'type-error (fold 0 + '(1 2 3) 'a))

(check (fold-right + 0 '(1 2 3 4)) => 10)

(check (fold-right + 0 '()) => 0)

(check
  (fold-right (lambda (x count) (if (symbol? x) (+ count 1) count))
    0
    '(a b 1 2 3 4))
  =>
  2)

(check (fold-right cons () '(1 2 3 4)) => '(1 2 3 4))

(check (fold-right + 0 '(1 2 3) '(4 5 6)) => 21)
(check (fold-right + 0 '(1 2 3 4) '(10 20 30)) => 66)
(check (fold-right list '() '(1 2 3) '(a b c)) => '(1 a (2 b (3 c ()))))
(check-catch 'type-error (fold-right 0 + '(1 2 3) 'a))

(check (reduce + 0 '(1 2 3 4)) => 10)
(check (reduce + 0 '()) => 0)

(check (reduce cons () '(1 2 3 4)) => '(4 3 2 . 1))

(check-catch 'wrong-type-arg 
  (reduce (lambda (x count) (if (symbol? x) (+ count 1) count))
          0
          '(a b 1 2 3 4)))

(check (reduce-right + 0 '(1 2 3 4)) => 10)

(check (reduce-right + 0 '()) => 0)

(check (reduce-right cons () '(1 2 3 4))
       => '(1 2 3 . 4) )

(check
  (reduce-right (lambda (x count) (if (symbol? x) (+ count 1) count))
    0
    '(a b 1 2 3 4))
  => 6)

(let* ((proc (lambda (x) (list x (* x 2))))
       (input '(1 2 3))
       (expected '(1 2 2 4 3 6)))
  (check (append-map proc input) => expected))

(let* ((proc (lambda (x y) (list (+ x y) (- x y))))
       (list1 '(5 8 10))
       (list2 '(3 2 7))
       (expected '(8 2 10 6 17 3)))
  (check (append-map proc list1 list2) => expected))

(check (append-map (lambda (x y) (list x y)) '(1) '()) => '())

(let* ((proc (lambda (x) (if (even? x) (list x) '())))
       (input '(1 2 3 4))
       (expected '(2 4)))
  (check (append-map proc input) => expected))

(let* ((proc (lambda (x y) (list (cons x y))))
       (list1 '(a b c))
       (list2 '(1 2))
       (expected '((a . 1) (b . 2))))
  (check (append-map proc list1 list2) => expected))

(let* ((proc (lambda (x) (list (list x) (list (* x 2)))))
       (input '(5))
       (expected '( (5) (10) )))
  (check (append-map proc input) => expected))

(check (filter even? '(-2 -1 0 1 2)) => '(-2 0 2))

; 下面的代码如果使用旧版的S7 Scheme，会Crash
(filter (lambda (x) #t) (make-list 100000 1))

(check
  (partition symbol? '(one 2 3 four five 6))
  => (cons '(five four one) '(6 3 2)))

(check (remove even? '(-2 -1 0 1 2)) => '(-1 1))

#|
find
在列表中查找第一个满足谓词的元素。

语法
----
(find pred clist)

参数
----
pred : procedure?
一个谓词过程，接受列表中的每个元素作为参数，返回布尔值。
clist : list?
要查找的列表。

返回值
----
value
返回 clist 中第一个使 pred 返回 #t 的元素。如果没有找到，返回 #f。

注意
----
find 返回 #f 时有语义上的歧义：无法区分是找到一个值为 #f 的元素，还是没有任何元素满足谓词。
在大多数情况下，这种歧义不会出现。如果需要消除这种歧义，建议使用 find-tail。

错误处理
----
wrong-type-arg 如果 clist 不是列表类型。

|#

(check (find even? '(3 1 4 1 5 9)) => 4)

(check (find even? '()) => #f)

(check (find even? '(1 3 5 7 9)) => #f)

(check (take-while even? '()) => '())

(check (take-while (lambda (x) #t) '(1 2 3))
  => '(1 2 3))

(check
  (take-while (lambda (x) #f) '(1 2 3))
  => '())

(check
  (take-while (lambda (x) (not (= x 1))) '(1 2 3))
  => '())

(check
  (take-while (lambda (x) (< x 3)) '(1 2 3 0))
  => '(1 2))

(check (drop-while even? '()) => '())

(check (drop-while (lambda (x) #t) '(1 2 3)) => '())

(check (drop-while (lambda (x) #f) '(1 2 3)) => '(1 2 3))

(check
  (drop-while (lambda (x) (not (= x 1))) '(1 2 3))
  => '(1 2 3))

(check (list-index even? '(3 1 4 1 5 9)) => 2)
(check (list-index even? '()) => #f)
(check (list-index even? '(1 3 5 7 9)) => #f)

(check (any integer? '()) => #f)
(check (any integer? '(a 3.14 "3")) => #f)
(check (any integer? '(a 3.14 3)) => #t)

(check (every integer? '()) => #t)
(check (every integer? '(a 3.14 3)) => #f)
(check (every integer? '(1 2 3)) => #t)

(check (delete 1 (list 1 2 3 4)) => (list 2 3 4))

(check (delete 0 (list 1 2 3 4)) => (list 1 2 3 4))

(check (delete #\a (list #\a #\b #\c) char=?)
       => (list #\b #\c))

(check (delete #\a (list #\a #\b #\c) (lambda (x y) #f))
       => (list #\a #\b #\c))

(check (delete 1 (list )) => (list ))

(check
  (catch 'wrong-type-arg
    (lambda ()
      (check (delete 1 (list 1 2 3 4) 'not-pred) => 1))
    (lambda args #t))
  => #t)

(check (delete-duplicates (list 1 1 2 3)) => (list 1 2 3))
(check (delete-duplicates (list 1 2 3)) => (list 1 2 3))
(check (delete-duplicates (list 1 1 1)) => (list 1))

(check (delete-duplicates (list )) => (list ))

(check (delete-duplicates (list 1 1 2 3) (lambda (x y) #f))
       => (list 1 1 2 3))

(check (delete-duplicates '(1 -2 3 2 -1) (lambda (x y) (= (abs x) (abs y))))
       => (list 1 -2 3))

(check
  (catch 'wrong-type-arg
    (lambda
      ()
      (check (delete-duplicates (list 1 1 2 3) 'not-pred) => 1))
    (lambda args #t))
  => #t)

(let1 l '((a 1) (b 2) (c . 3))
  (check (assq 'a l) => `(a 1))
  (check-true (eq? (assq 'a l) (l 0)))
  (check (assq 'b l) => `(b 2))
  (check (assq 'c l) => `(c . 3))
  (check (assq 'd l) => #f))

#|
assq
在关联列表中查找键，使用 eq? 进行比较。

语法
----
(assq key alist)

参数
----
key : any
要查找的键值，通常是一个符号。
alist : list
关联列表，每个元素都是一个配对（pair），其中 car 是键，cdr 是值。

返回值
----
alist-element | #f
如果在关联列表中找到匹配的键，返回对应的配对；如果未找到匹配项，返回 #f。

描述
----
assq 在关联列表中查找第一个键与给定 key 匹配的配对。键的比较使用 eq? 操作符，
这意味着键必须是同一个对象（通常用于符号、数字或其他不可变对象）。

assq 是 SRFI-1 规范中定义的关联列表操作函数，适用于符号键或其他.eq? 可比较的对象。

注意
----
- 返回的是找到的第一个配对，不是值本身
- 使用 eq? 进行比较，因此最适合符号键
- 对于字符串键等需要使用 equal? 的情况，请使用 assoc
|#

(let1 l '((a 1) (b 2) (c . 3))
  (check (assq 'a l) => `(a 1))
  (check-true (eq? (assq 'a l) (l 0)))
  (check (assq 'b l) => `(b 2))
  (check (assq 'c l) => `(c . 3))
  (check (assq 'd l) => #f))

; Additional comprehensive assq tests
(check (assq 'a '()) => #f)
(check (assq 'a '((a . 1))) => '(a . 1))
(check (assq 'a '((a) (b))) => '(a))
(check (assq 'b '((a 1) (b 2) (a 3))) => '(b 2))
(check (assq 'a '((a 1) (b 2) (a 3))) => '(a 1))
(check (assq 1 '((1 "one") (2 "two"))) => '(1 "one"))
(check (assq 3 '((1 "one") (2 "two"))) => #f)

; Test with different value types
(check (assq 'x '((x . 10) (y . 20))) => '(x . 10))
(check (assq 'vector '((symbol . 1) (vector . #(1 2 3)) (list . (a b c)))) => '(vector . #(1 2 3)))
(check (assq 'list '((symbol . 1) (vector . #(1 2 3)) (list . (a b c)))) => '(list a b c))

; Test with dotted pairs as values
(check (assq 'key '((key . value) (other . something))) => '(key . value))
(check (assq 'missing '((key . value) (other . something))) => #f)

(let1 l '((2 3) (5 7) (11 . 13))
  (check (assv 5 l) => '(5 7))
  (check (assv 11 l) => '(11 . 13)))

(let1 l '(((a)) ((b)) ((c)))
  (check (assoc '(a) l) => '((a)))
  (check (assq '(a) l) => #f)
  (check (assv '(a) l) => #f))

(check (alist-cons 'a 1 '()) => '((a . 1)))
(check (alist-cons 'a 1 '((b . 2))) => '((a . 1) (b . 2)))

(let1 cl (circular-list 1 2 3)
  (check (cl 3) => 1)
  (check (cl 4) => 2)
  (check (cl 5) => 3)
  (check (cl 6) => 1))

(check-true (circular-list? (circular-list 1 2)))
(check-true (circular-list? (circular-list 1)))

(let* ((l (list 1 2 3))
       (end (last-pair l)))
  (set-cdr! end (cdr l))
  (check-true (circular-list? l)))

(check-false (circular-list? (list 1 2)))

(check-true (length=? 3 (list 1 2 3)))
(check-false (length=? 2 (list 1 2 3)))
(check-false (length=? 4 (list 1 2 3)))

(check-true (length=? 0 (list )))
(check-catch 'value-error (length=? -1 (list )))

(check-true (length>? '(1 2 3 4 5) 3))
(check-false (length>? '(1 2) 3))
(check-false (length>? '() 0))

(check-true (length>? '(1) 0))
(check-false (length>? '() 1))

(check-false (length>? '(1 2 . 3) 2))
(check-true (length>? '(1 2 . 3) 1))

(check-true (length>=? '(1 2 3 4 5) 3))
(check-false (length>=? '(1 2) 3))
(check-true (length>=? '() 0))

(check-true (length>=? '(1) 0))
(check-false (length>=? '() 1))

(check-false (length>=? '(1 2 . 3) 3))
(check-true (length>=? '(1 2 . 3) 2))

(check (flat-map (lambda (x) (list x x))
                 (list 1 2 3))
  => (list 1 1 2 2 3 3))

(check-catch 'type-error (flat-map 1 (list 1 2 3)))

(check (not-null-list? (list 1)) => #t)
(check (list-not-null? (list 1)) => #t)
(check (list-null? (list 1)) => #f)

(check (not-null-list? (list 1 2 3)) => #t)
(check (list-not-null? (list 1 2 3)) => #t)
(check (list-null? (list 1 2 3)) => #f)

(check (not-null-list? '(a)) => #t)
(check (list-not-null? '(a)) => #t)
(check (list-null? '(a)) => #f)

(check (not-null-list? '(a b c)) => #t)
(check (list-not-null? '(a b c)) => #t)
(check (list-null? '(a b c)) => #f)

(check (not-null-list? ()) => #f)
(check (list-not-null? ()) => #f)
(check (list-null? ()) => #t)

; '(a) is a pair and a list
; '(a . b) is a pair but not a list
(check (not-null-list? '(a . b)) => #f)
(check (list-not-null? '(a . b)) => #f)
(check (list-null? '(a . b)) => #f)

(check-catch 'type-error (not-null-list? 1))
(check (list-not-null? 1) => #f)
(check (list-null? 1) => #f)

#|
flatten
展平嵌套列表到指定深度，或者展平到最深层级。

语法
----
(flatten lst)(flatten lst depth)

参数
----
lst : list被展平的列表。depth : integer 或 'deepest展平深度。当为整数时表示展平的层数，当为 'deepest 时表示展平到最深层级。

返回值
----
list展平后的列表。如果指定 depth，将按指定深度展平嵌套结构。如果指定 'deepest，将完全展平所有嵌套层级。

说明
----
flatten 是 SRFI-1 中的一个实用工具函数，用于将嵌套列表结构展平到单一维度的列表。它提供了灵活的展平选项：- 不带 depth 参数时，默认为深度 1，即展平第一层嵌套 - 提供整数 depth 参数时，按指定深度展平多级嵌套结构 - 提供符号 'deepest 时，完全展平所有嵌套层级，无论多深

使用场景
--------
- 处理树形结构数据，将其转换为线性序列- 清理嵌套的输入数据- 字符串处理，展平嵌套字符列表

错误处理
--------
type-error 当 depth 参数既不是整数也不是 'deepest 符号时抛出。

例子
----
(flatten '((a) () (b ()) () (c)))       => '(a b () c)
(flatten '((a) () (b ()) () (c)) 2)     => '(a b c)
(flatten '((a) () (b ()) () (c)) 'deepest) => '(a b c)
(flatten '((a b) c (((d)) e)) 1)        => '(a b c ((d)) e)
(flatten '((a b) c (((d)) e)) 3)        => '(a b c d e)

|#

; deepest flatten
(check (flatten '((a) () (b ()) () (c)) 'deepest) => '(a b c))
(check (flatten '((a b) c (((d)) e)) 'deepest) => '(a b c d e))
(check (flatten '(a b (() (c))) 'deepest) => '(a b c))
; depth flatten
(check (flatten '((a) () (b ()) () (c)) 0) => '((a) () (b ()) () (c)))
(check (flatten '((a) () (b ()) () (c)) 1) => '(a b () c))
(check (flatten '((a) () (b ()) () (c))) => '(a b () c))
(check (flatten '((a) () (b ()) () (c)) 2) => '(a b c))
(check (flatten '((a) () (b ()) () (c)) -1) => '(a b c))
(check (flatten '((a b) c (((d)) e)) 0) => '((a b) c (((d)) e)))
(check (flatten '((a b) c (((d)) e)) 1) => '(a b c ((d)) e))
(check (flatten '((a b) c (((d)) e))) => '(a b c ((d)) e))
(check (flatten '((a b) c (((d)) e)) 2) => '(a b c (d) e))
(check (flatten '((a b) c (((d)) e)) 3) => '(a b c d e))
(check (flatten '((a b) c (((d)) e)) -1) => '(a b c d e))
(check (flatten '(a b (() (c))) 0) => '(a b (() (c))))
(check (flatten '(a b (() (c))) 1) => '(a b () (c)))
(check (flatten '(a b (() (c)))) => '(a b () (c)))
(check (flatten '(a b (() (c))) 2) => '(a b c))
(check (flatten '(a b (() (c))) -1) => '(a b c))
; error depth flatten
(check-catch 'type-error (flatten '((a) () (b ()) () (c)) 'a))
(check-catch 'type-error (flatten '((a) () (b ()) () (c)) (make-vector 1 1)))

(check-report)