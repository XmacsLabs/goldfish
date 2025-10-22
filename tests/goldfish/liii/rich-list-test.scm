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
        (scheme base)
        (liii rich-list)
        (liii lang)
        (liii error))

(check-set-mode! 'report-failed)


#|
rich-list@range  
生成一个从起始值到结束值（不包含结束值）的数字序列。

语法
----
(rich-list :range start end . step-and-args)

参数
----
start : integer
序列的起始值（包含）。

end : integer  
序列的结束边界值（不包含）。

step-and-args : list
可选参数，可包含步进值和链式方法参数。

返回值
-----
以rich-list形式返回生成的整数序列。

功能
----
根据起始值、结束值和步进值生成连续整数序列。步进可以是正数或负数，但不能为0。

边界条件
--------
- 步进为0时抛出 value-error 异常
- 步进为正且 start ≥ end：返回空列表
- 步进为负且 start ≤ end：返回空列表

性能特征
--------
- 时间复杂度：O(n)，n为序列长度
- 空间复杂度：O(n)，存储生成的完整序列

兼容性
------
- 支持链式调用
- 所有参数必须为整数
|#

;; 基本测试
(check ((rich-list :range 1 5) :collect) => (list 1 2 3 4))
(check ((rich-list :range 1 5 2) :collect) => (list 1 3))
(check ((rich-list :range 1 6 2) :collect) => (list 1 3 5))
(check ((rich-list :range 5 1 -1) :collect) => (list 5 4 3 2))
(check ((rich-list :range 1 5 :map (lambda (x) (* x 2))) :collect) => (list 2 4 6 8))
(check ((rich-list :range 1 10 1 :map (lambda (x) (+ x 1))) :collect) => (list 2 3 4 5 6 7 8 9 10))
(check ((rich-list :range 5 1 1) :collect) => (list))

;; 边界测试
(check ((rich-list :range 10 1 1) :collect) => (list))
(check ((rich-list :range -5 -1 1) :collect) => (list -5 -4 -3 -2))
(check ((rich-list :range -1 -5 -1) :collect) => (list -1 -2 -3 -4))
(check ((rich-list :range 5 6) :collect) => (list 5))
(check ((rich-list :range 5 6 -1) :collect) => (list))
(check ((rich-list :range 1 5) :length) => 4)
(check ((rich-list :range 1 1) :length) => 0)


#|
rich-list@empty
创建一个空的rich-list对象。

语法
----
(rich-list :empty . args)

参数
----
args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回空的列表对象。

说明
----
创建一个不包含任何元素的rich-list。通常用于初始化数据结构或作为
链式操作的起点。

边界条件
--------
- 无参数调用：返回空列表
- 支持链式调用：可与其他rich-list方法组合使用

性能特征
--------
- 时间复杂度：O(1)，固定时间创建
- 空间复杂度：O(1)，创建空对象所需最小内存

兼容性
------
- 与所有rich-list实例方法兼容
- 支持链式调用模式
|#

;; 基本测试
(check ((rich-list :empty) :collect) => ())
(check ((rich-list :empty) :length) => 0)
(check ((rich-list :empty) :empty?) => #t)

;; 边界测试
(check ((rich-list :empty :map (lambda (x) (* x 2))) :collect) => ())
(check ((rich-list :empty :filter (lambda (x) #t)) :collect) => ())
(check ((rich-list :empty :take 0) :collect) => ())
(check ((rich-list :empty :drop 0) :collect) => ())
(check ((rich-list :empty :reverse) :collect) => ())


#|
rich-list@concat
连接两个rich-list为一个新的rich-list。

语法
----
(rich-list :concat lst1 lst2 . args)

参数
----
lst1 : rich-list
第一个待连接的rich-list。

lst2 : rich-list
第二个待连接的rich-list。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-list形式返回连接后的列表对象。

功能
----
将两个rich-list的内容合并为一个新的rich-list，保持原有顺序。
第一个列表的元素在前，第二个列表的元素在后。

边界条件
--------
- 任一为空列表时仍正常连接
- 支持链式调用模式

性能特征
--------
- 时间复杂度：O(n)，n为两个列表长度之和
- 空间复杂度：O(n)，创建新的合并列表

兼容性
------
- 与所有rich-list方法兼容
- 支持链式调用模式
|#

;; 基本测试 - 两个非空列表连接 (保持原来的形式，避免$在还不适的地方)
(check ((rich-list :concat (rich-list '(1 2 3)) (rich-list '(4 5 6))) :collect) => '(1 2 3 4 5 6))
(check ((rich-list :concat (rich-list '(a b)) (rich-list '(c d))) :collect) => '(a b c d))

;; 边界测试 - 第一个列表为空
(check ((rich-list :concat (rich-list :empty) (rich-list '(1 2 3))) :collect) => '(1 2 3))

;; 边界测试 - 第二个列表为空
(check ((rich-list :concat (rich-list '(1 2 3)) (rich-list :empty)) :collect) => '(1 2 3))

;; 边界测试 - 两个列表都为空
(check ((rich-list :concat (rich-list :empty) (rich-list :empty)) :collect) => '())

;; 链式调用测试
(check ((rich-list :concat (rich-list '(1 2)) (rich-list '(3 4))) :map (lambda (x) (* x 2)) :collect) => '(2 4 6 8))
(check ((rich-list :concat (rich-list '(a b)) (rich-list '(c d))) :length) => 4)

;; 验证不改变原列表
(let ((lst1 (rich-list '(1 2 3)))
      (lst2 (rich-list '(4 5 6))))
  (rich-list :concat lst1 lst2)
  (check (lst1 :collect) => '(1 2 3))
  (check (lst2 :collect) => '(4 5 6)))

#|
rich-list@fill
创建一个指定长度、所有元素都为指定值的rich-list。

语法
----
(rich-list :fill n elem)

参数
----
n : integer
要创建的列表长度，必须为非负整数。

elem : any
列表中要填充的元素值，可以是任意类型的对象。

返回值
-----
以rich-list形式返回包含n个相同元素的新列表。

功能
----
创建一个长度为n的rich-list，其中所有元素的值都设置为elem。
当n为0时返回空列表，当n为负数时抛出value-error异常。

边界条件
--------
- n为负数时：抛出value-error异常，错误信息为"n cannot be negative"
- n为0时：返回空rich-list
- n为整数且n≥0时：正常创建指定长度的rich-list
- elem可以是任意类型的Scheme对象，包括函数、列表、数字等

异常处理
--------
当参数n不是非负整数时，函数会抛出value-error类型异常。
此异常属于(liii error)模块，可在测试环境通过check-catch捕获验证。

性能特征
--------
- 时间复杂度：O(n)，需要逐个创建n个元素
- 空间复杂度：O(n)，需要为n个元素分配内存存储新列表

兼容性
------
- 支持与rich-list所有实例方法链式调用
- 返回的rich-list对象可与现有的实例方法无缝组合
|#
;; 基本测试
(check ((rich-list :fill 5 'x) :collect) => '(x x x x x))
(check ((rich-list :fill 3 10) :collect) => '(10 10 10))
(check ((rich-list :fill 1 "hello") :collect) => '("hello"))
(check ((rich-list :fill 0 'a) :collect) => ())

;; 边界测试 - n为0
(check ((rich-list :fill 0 1) :collect) => ())
(check ((rich-list :fill 0 "test") :collect) => ())

;; 边界测试 - 不同类型的elem
(check ((rich-list :fill 3 #t) :collect) => '(#t #t #t))
(check ((rich-list :fill 2 '(1 2 3)) :collect) => '((1 2 3) (1 2 3)))
(check ((rich-list :fill 4 #f) :collect) => '(#f #f #f #f))

;; 边界测试 - 基础边界
(check ((rich-list :fill 0 'test) :collect) => ())
(check ((rich-list :fill 1 42) :collect) => '(42))
(check ((rich-list :fill 100 0) :length) => 100)
(check ((rich-list :fill 2 '(nested list)) :collect) => '((nested list) (nested list)))

;; 边界测试 - 异常处理
(check-catch 'value-error (rich-list :fill -1 'x))
(check-catch 'value-error (rich-list :fill -3 42))

;; 链式调用测试
(check ((rich-list :fill 4 2) :map (lambda (x) (* x 3)) :collect) => '(6 6 6 6))
(check ((rich-list :fill 3 'a) :length) => 3)
(check ((rich-list :fill 5 1) :filter (lambda (x) (= x 1)) :collect) => '(1 1 1 1 1))
(check ((rich-list :fill 3 100) :take 2 :collect) => '(100 100))


#|
rich-list%collect  
将rich-list转换为标准的Scheme列表。

语法
----
(lst :collect)

参数
----
无

返回值
-----
与rich-list包含相同元素的标准Scheme列表。

功能
----
将rich-list对象中包含的元素数据以标准Scheme列表形式返回。
该函数提供与现有Scheme系统的互操作性，允许用户在使用rich-list的
丰富操作方法后，回到传统列表环境继续处理数据。

边界条件
--------
- 空rich-list返回空列表'()
- 保持原始数据的内部结构和引用完整性

性能特征
--------
- 时间复杂度：O(1)，直接访问内部引用
- 空间复杂度：O(1)，仅返回现有对象引用

兼容性
------
- 适用于任何rich-list实例
- 与标准Scheme环境中的list操作无缝兼容
|#

;; 基本功能测试
(check ($ '(1 2 3 4) :collect) => '(1 2 3 4))
(check ($ '(a b c) :collect) => '(a b c))
(check ($ '() :collect) => '())

;; 边界条件测试 - 空列表
(check ((rich-list :empty) :collect) => '())

;; 边界条件测试 - 单元素列表
(check ((rich-list '(42)) :collect) => '(42))

;; 嵌套结构测试
(check ($ '((1 2) (3 4)) :collect) => '((1 2) (3 4)))
(check ($ '(((a)) ((b))) :collect) => '(((a)) ((b))))

;; 多种类型测试
(check ($ '(#t #f "hello" 42) :collect) => '(#t #f "hello" 42))

;; 链式操作结合测试
(check (($ '(1 2 3 4 5) :filter even?) :collect) => '(2 4))
(check (($ '(1 2 3 4) :map (lambda (x) (* x 2))) :collect) => '(2 4 6 8))
(check (($ '(1 2 3 4 5) :take 3) :collect) => '(1 2 3))

;; 验证返回标准Scheme列表
(let ((result ($ '(a b c) :collect)))
  (check (list? result) => #t))

;; 验证列表操作兼容性
(let ((result ($ '(1 2 3) :collect)))
  (check (car result) => 1)
  (check (cadr result) => 2)
  (check (cddr result) => '(3)))


#|
rich-list%find
在rich-list中查找第一个满足条件的元素。

语法
----
(lst :find pred)

参数
----
pred : procedure
用于测试元素的谓词函数，接受一个参数并返回布尔值。

返回值
-----
以option形式返回找到的第一个满足条件的元素。
- 如果找到匹配元素：返回包含该元素的option对象
- 如果没有找到匹配元素：返回none

功能
----
从列表的开头开始遍历，返回第一个满足谓词条件的元素。
使用option类型包装结果，避免空值异常。

边界条件
--------
- 空列表：返回none
- 没有满足条件的元素：返回none
- 多个满足条件的元素：返回第一个匹配的元素

性能特征
--------
- 时间复杂度：O(n)，最坏情况下需要遍历整个列表
- 空间复杂度：O(1)，仅返回option对象引用

兼容性
------
- 与option类型系统兼容
- 支持链式调用模式
|#

;; 基本测试 - 找到元素
(check (($ '(1 2 3 4 5) :find even?) :get) => 2)
(check (($ '(1 3 5 7 9) :find (lambda (x) (> x 5))) :get) => 7)
(check (($ '(a b c d) :find (lambda (x) (eq? x 'c))) :get) => 'c)

;; 边界测试 - 空列表
(check (($ '() :find (lambda (x) #t)) :defined?) => #f)

;; 边界测试 - 没有匹配元素
(check (($ '(1 3 5 7) :find even?) :defined?) => #f)
(check (($ '(a b c) :find (lambda (x) (eq? x 'z))) :defined?) => #f)

;; 边界测试 - 多个匹配元素，返回第一个
(check (($ '(1 2 4 6 8) :find even?) :get) => 2)
(check (($ '(5 10 15 20) :find (lambda (x) (= (modulo x 5) 0))) :get) => 5)

;; 链式调用测试
(check (($ '(1 2 3 4 5) :filter (lambda (x) (> x 2)) :find even?) :get) => 4)


#|
rich-list%find-last
在rich-list中从后往前查找第一个满足条件的元素。

语法
----
(lst :find-last pred)

参数
----
pred : procedure
用于测试元素的谓词函数，接受一个参数并返回布尔值。

返回值
-----
以option形式返回从后往前找到的第一个满足条件的元素。
- 如果找到匹配元素：返回包含该元素的option对象
- 如果没有找到匹配元素：返回none

功能
----
从列表的末尾开始向前遍历，返回第一个满足谓词条件的元素。
与find方法相反，find-last返回最后一个匹配的元素。
使用option类型包装结果，避免空值异常。

边界条件
--------
- 空列表：返回none
- 没有满足条件的元素：返回none
- 多个满足条件的元素：返回最后一个匹配的元素

性能特征
--------
- 时间复杂度：O(n)，最坏情况下需要遍历整个列表
- 空间复杂度：O(n)，需要反转列表（临时空间）

兼容性
------
- 与option类型系统兼容
- 支持链式调用模式
|#

;; 基本测试 - 找到最后一个匹配元素
(check (($ '(1 2 3 4 5) :find-last even?) :get) => 4)
(check (($ '(1 3 5 7 9) :find-last (lambda (x) (> x 5))) :get) => 9)
(check (($ '(a b c d c) :find-last (lambda (x) (eq? x 'c))) :get) => 'c)

;; 边界测试 - 空列表
(check (($ '() :find-last (lambda (x) #t)) :defined?) => #f)

;; 边界测试 - 没有匹配元素
(check (($ '(1 3 5 7) :find-last even?) :defined?) => #f)
(check (($ '(a b c) :find-last (lambda (x) (eq? x 'z))) :defined?) => #f)

;; 边界测试 - 多个匹配元素，返回最后一个
(check (($ '(1 2 4 6 8) :find-last even?) :get) => 8)
(check (($ '(5 10 15 20) :find-last (lambda (x) (= (modulo x 5) 0))) :get) => 20)

;; 链式调用测试
(check (($ '(1 2 3 4 5) :filter (lambda (x) (> x 1)) :find-last even?) :get) => 4)


(check-report)
