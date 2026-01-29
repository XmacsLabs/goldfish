;
; Copyright (C) 2026 The Goldfish Scheme Authors
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

(import (scheme base)
        (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; --- Data Setup ---
(define b-empty (bag))
(define comp (bag-comparator b-empty))

#|
bag
创建一个新的 bag。

语法
----
(bag element ...)

参数
----
element ... : any
初始元素。

返回值
-----
返回包含指定元素的 bag。
|#
(define b-1-2 (bag 1 2 2))
(define b-list (bag->list b-1-2))
(check (bag-member b-1-2 1 #f) => 1)
(check (bag-member b-1-2 2 #f) => 2)
(check (bag-member b-1-2 3 'none) => 'none)
(check-true (eq? (bag-comparator b-1-2) comp))
(check (bag-member b-empty 1 'missing) => 'missing)

;; bag->list should include duplicates
(check-false (not (member 1 b-list)))
(check-false (not (member 2 b-list)))
(check (length b-list) => 3)




;; 不同类型元素也可存入 bag
(define b-mixed (bag "a" 'a 0))
(check (bag-member b-mixed "a" #f) => "a")
(check (bag-member b-mixed 'a #f) => 'a)
(check (bag-member b-mixed 0 #f) => 0)

;; equal? 等价元素应命中
(define a1 "hello")
(define a2 (string-copy a1))
(define b-strings (bag a1))
(check-true (string=? (bag-member b-strings a2 #f) "hello"))

#|
bag-unfold
使用 unfold 模式创建 bag。

语法
----
(bag-unfold stop? mapper successor seed comparator)

参数
----
stop? : procedure
停止谓词。接收当前种子，返回布尔值。

mapper : procedure
映射函数。接收当前种子，返回要添加到 bag 的元素。

successor : procedure
后继函数。接收当前种子，返回下一个种子。

seed : any
初始种子。

comparator : comparator
比较器。

返回值
-----
返回由 unfold 生成的 bag。
|#
(define b-unfold
  (bag-unfold (lambda (n) (> n 3))
              (lambda (n) n)
              (lambda (n) (+ n 1))
              1
              comp))
(check (bag-member b-unfold 1 #f) => 1)
(check (bag-member b-unfold 2 #f) => 2)
(check (bag-member b-unfold 3 #f) => 3)
(check (bag-member b-unfold 4 'no) => 'no)
(check-true (eq? (bag-comparator b-unfold) comp))
(check-catch 'type-error
             (bag-unfold (lambda (n) #t)
                         (lambda (n) n)
                         (lambda (n) n)
                         0
                         "not a comparator"))

;; stop? 立即为真，返回空 bag
(define b-unfold-empty
  (bag-unfold (lambda (n) #t)
              (lambda (n) n)
              (lambda (n) n)
              0
              comp))
(check (bag-member b-unfold-empty 1 'none) => 'none)

;; mapper 返回常量，重复元素也应能命中
(define b-unfold-dup
  (bag-unfold (lambda (n) (> n 2))
              (lambda (n) 'x)
              (lambda (n) (+ n 1))
              0
              comp))
(check (bag-member b-unfold-dup 'x #f) => 'x)

#|
bag-member
获取 bag 中与给定元素相等的元素。

语法
----
(bag-member bag element default)

参数
----
bag : bag
目标 bag。

element : any
要查找的元素。

default : any
未找到时返回的默认值。

返回值
-----
如果 bag 中存在与 element 等价的元素，返回该元素；否则返回 default。
|#
(check (bag-member b-1-2 2 #f) => 2)
(check (bag-member b-1-2 9 'missing) => 'missing)
(check-catch 'type-error (bag-member "not a bag" 1 #f))
(check (bag-member b-empty 1 'none) => 'none)

#|
bag?
判断是否为 bag。

语法
----
(bag? obj)

参数
----
obj : any
要检查的对象。

返回值
-----
如果 obj 是 bag，返回 #t；否则返回 #f。
|#
(check-true (bag? b-empty))
(check-true (bag? b-1-2))
(check-false (bag? "not a bag"))
(check-false (bag? '()))

#|
bag-contains?
判断 bag 是否包含元素。

语法
----
(bag-contains? bag element)

参数
----
bag : bag
目标 bag。

element : any
要检查的元素。

返回值
-----
如果 bag 中存在与 element 等价的元素，返回 #t；否则返回 #f。
|#
(check-true (bag-contains? b-1-2 2))
(check-false (bag-contains? b-1-2 9))
(check-false (bag-contains? b-empty 1))
(check-catch 'type-error (bag-contains? "not a bag" 1))

#|
bag-empty?
判断 bag 是否为空。

语法
----
(bag-empty? bag)

参数
----
bag : bag
目标 bag。

返回值
-----
如果 bag 为空，返回 #t；否则返回 #f。
|#
(check-true (bag-empty? b-empty))
(check-false (bag-empty? b-1-2))
(check-catch 'type-error (bag-empty? "not a bag"))

#|
bag-disjoint?
判断两个 bag 是否不相交。

语法
----
(bag-disjoint? bag1 bag2)

参数
----
bag1 : bag
第一个 bag。

bag2 : bag
第二个 bag。

返回值
-----
如果两个 bag 没有相等元素，返回 #t；否则返回 #f。
|#
(check-true (bag-disjoint? (bag 1 1) (bag 2 2)))
(check-false (bag-disjoint? (bag 1 1) (bag 1 2)))
(check-true (bag-disjoint? b-empty (bag 1)))
(check-true (bag-disjoint? (bag 1) b-empty))
(check-catch 'type-error (bag-disjoint? "not a bag" (bag 1)))
(check-catch 'type-error (bag-disjoint? (bag 1) "not a bag"))

#|
bag-comparator
获取 bag 的 comparator。

语法
----
(bag-comparator bag)

参数
----
bag : bag
目标 bag。

返回值
-----
返回 bag 使用的 comparator。
|#
(check-true (eq? (bag-comparator b-empty) comp))
(check-true (eq? (bag-comparator b-1-2) comp))

#|
内部校验 check-bag 的函数也要覆盖错误分支。
|#
(check-catch 'type-error (bag-member "not a bag" 1 #f))


(check-report)
