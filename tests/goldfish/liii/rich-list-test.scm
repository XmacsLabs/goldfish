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
        (liii lang))

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
(check ((rich-list :fill 0 'x) :collect) => ())

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

;; 基本测试 - 两个非空列表连接
(check ((rich-list :concat (rich-list '(1 2 3)) (rich-list '(4 5 6))) :collect) => '(1 2 3 4 5 6))
(check ((rich-list :concat (rich-list '(a b)) (rich-list '(c d))) :collect) => '(a b c d))

;; 边界测试 - 第一个列表为空
(check ((rich-list :concat (rich-list :empty) (rich-list '(1 2 3))) :collect) => '(1 2 3))

;; 边界测试 - 第二个列表为空
(check ((rich-list :concat (rich-list '(1 2 3)) (rich-list :empty)) :collect) => '(1 2 3))

;; 边界测试 - 两个列表都为空
(check ((rich-list :concat (rich-list :empty) (rich-list :empty)) :collect) => '())

;; 边界测试 - 嵌套rich-list连接
(check ((rich-list :concat (rich-list :range 1 4) (rich-list :range 4 7)) :collect) => '(1 2 3 4 5 6))

;; 链式调用测试
(check ((rich-list :concat (rich-list '(1 2)) (rich-list '(3 4))) :map (lambda (x) (* x 2)) :collect) => '(2 4 6 8))
(check ((rich-list :concat (rich-list '(a b)) (rich-list '(c d))) :length) => 4)

;; 验证不改变原列表
(let ((lst1 (rich-list '(1 2 3)))
      (lst2 (rich-list '(4 5 6))))
  (rich-list :concat lst1 lst2)
  (check (lst1 :collect) => '(1 2 3))
  (check (lst2 :collect) => '(4 5 6)))

(check-report)
