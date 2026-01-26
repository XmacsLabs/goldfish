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
        (liii set)
        (srfi srfi-128))

(check-set-mode! 'report-failed)

(define comp (make-default-comparator))

;; --- Data Setup ---
(define s-empty (set comp))
(define s-1 (set comp 1))
(define s-1-2 (set comp 1 2))
(define s-1-2-3 (set comp 1 2 3))
(define s-2-3-4 (set comp 2 3 4))
(define s-4-5 (set comp 4 5))

#|
set?
检查对象是否为 set。

语法
----
(set? obj)

参数
----
obj : any
要检查的对象。

返回值
-----
如果 obj 是 set，返回 #t；否则返回 #f。
|#
(check-true (set? s-empty))
(check-true (set? s-1))
(check-false (set? "not a set"))
(check-false (set? '()))
(check-false (set? #(1 2 3)))


#|
set
创建一个新的 set。

语法
----
(set comparator element ...)

参数
----
comparator : comparator
元素比较器。

element ... : any
初始元素。

返回值
-----
返回包含指定元素的 set。
|#
(check-true (set? (set comp 1 2 3)))
(check-true (set-contains? (set comp 1) 1))

#|
list->set
将列表转换为 set。

语法
----
(list->set comparator list)

参数
----
comparator : comparator
元素比较器。

list : list
要转换的列表。

返回值
-----
返回包含列表中所有元素的 set。
|#
(check-true (set=? s-1-2-3 (list->set comp '(1 2 3))))
(check-true (set=? s-empty (list->set comp '())))
;; Duplicates in list should be handled
(check-true (set=? s-1-2 (list->set comp '(1 2 2 1))))

#|
set-copy
复制一个 set。

语法
----
(set-copy set)

参数
----
set : set
要复制的 set。

返回值
-----
返回一个新的 set，包含原 set 的所有元素，且比较器相同。
|#
(let ((copy (set-copy s-1-2)))
  (check-true (set=? s-1-2 copy))
  (check-false (eq? s-1-2 copy))) ; Ensure new instance
(check-true (set-empty? (set-copy s-empty)))

#|
set-unfold
使用 unfold 模式创建 set。

语法
----
(set-unfold stop? mapper successor seed comparator)

参数
----
stop? : procedure
停止谓词。接收当前种子，返回布尔值。

mapper : procedure
映射函数。接收当前种子，返回要添加到 set 的元素。

successor : procedure
后继函数。接收当前种子，返回下一个种子。

seed : any
初始种子值。

comparator : comparator
元素比较器。

返回值
-----
返回生成的 set。
|#
;; Create set {0, 1, 2, ..., 9}
(define s-10 (set-unfold 
               (lambda (x) (= x 10)) 
               (lambda (x) x) 
               (lambda (x) (+ x 1)) 
               0 
               comp))
(check-true (set-contains? s-10 0))
(check-true (set-contains? s-10 9))
(check-false (set-contains? s-10 10))

#|
set-contains?
检查 set 是否包含指定元素。

语法
----
(set-contains? set element)

参数
----
set : set
目标 set。

element : any
要检查的元素。

返回值
-----
如果 set 包含 element，返回 #t；否则返回 #f。
|#
(check-true (set-contains? s-1 1))
(check-false (set-contains? s-1 2))
(check-false (set-contains? s-empty 1))

#|
set-empty?
检查 set 是否为空。

语法
----
(set-empty? set)

参数
----
set : set
要检查的 set。

返回值
-----
如果 set 为空，返回 #t；否则返回 #f。
|#
(check-true (set-empty? s-empty))
(check-false (set-empty? s-1))

#|
set-disjoint?
检查两个 set 是否不相交（没有共同元素）。

语法
----
(set-disjoint? set1 set2)

参数
----
set1, set2 : set
要检查的 set。

返回值
-----
如果两个 set 没有共同元素，返回 #t；否则返回 #f。
|#
(check-true (set-disjoint? s-1-2-3 s-4-5))
(check-false (set-disjoint? s-1-2-3 s-2-3-4)) ; share 2, 3
(check-true (set-disjoint? s-empty s-1))
(check-true (set-disjoint? s-1 s-empty))
(check-true (set-disjoint? s-empty s-empty))

#|
set=?
检查两个或多个 set 是否相等。

语法
----
(set=? set1 set2 ...)

参数
----
set1, set2, ... : set
要比较的 set。

返回值
-----
如果所有 set 都包含相同的元素，返回 #t；否则返回 #f。
注意：比较器必须相同。
|#
(check-true (set=? s-empty s-empty))
(check-true (set=? s-1 s-1))
(check-true (set=? s-1 (set comp 1)))
(check-false (set=? s-1 s-empty))
(check-false (set=? s-1 s-1-2))
;; Multiple arguments
(check-true (set=? s-1 (set comp 1) (list->set comp '(1))))
(check-false (set=? s-1 s-1 s-empty))

#|
set<=?
检查一个 set 是否为另一个 set 的子集。

语法
----
(set<=? set1 set2 ...)

参数
----
set1, set2, ... : set
要检查的 set。

返回值
-----
如果每个 set 都是其后一个 set 的子集，返回 #t；否则返回 #f。
|#
(check-true (set<=? s-empty s-1))
(check-true (set<=? s-1 s-1-2))
(check-true (set<=? s-1-2 s-1-2-3))
(check-true (set<=? s-1-2 s-1-2))
(check-false (set<=? s-1-2 s-1))
;; Chain
(check-true (set<=? s-empty s-1 s-1-2 s-1-2-3))
(check-false (set<=? s-empty s-1-2 s-1)) ; Broken chain

#|
set<?
检查一个 set 是否为另一个 set 的真子集。

语法
----
(set<? set1 set2 ...)

参数
----
set1, set2, ... : set
要检查的 set。

返回值
-----
如果每个 set 都是其后一个 set 的真子集，返回 #t；否则返回 #f。
|#
(check-true (set<? s-empty s-1))
(check-true (set<? s-1 s-1-2))
(check-false (set<? s-1 s-1))
(check-false (set<? s-1-2 s-1))
;; Chain
(check-true (set<? s-empty s-1 s-1-2))

#|
set>=?
检查一个 set 是否为另一个 set 的超集。

语法
----
(set>=? set1 set2 ...)

参数
----
set1, set2, ... : set
要检查的 set。

返回值
-----
如果每个 set 都是其后一个 set 的超集，返回 #t；否则返回 #f。
|#
(check-true (set>=? s-1 s-empty))
(check-true (set>=? s-1-2 s-1))
(check-true (set>=? s-1 s-1))
(check-false (set>=? s-1 s-1-2))
;; Chain
(check-true (set>=? s-1-2-3 s-1-2 s-1 s-empty))

#|
set>?
检查一个 set 是否为另一个 set 的真超集。

语法
----
(set>? set1 set2 ...)

参数
----
set1, set2, ... : set
要检查的 set。

返回值
-----
如果每个 set 都是其后一个 set 的真超集，返回 #t；否则返回 #f。
|#
(check-true (set>? s-1 s-empty))
(check-true (set>? s-1-2 s-1))
(check-false (set>? s-1 s-1))
(check-false (set>? s-1 s-1-2))
;; Chain
(check-true (set>? s-1-2 s-1 s-empty))

;; --- Different Data Types ---
(define sym-comp (make-default-comparator))
(define s-sym (set sym-comp 'a 'b 'c))
(check-true (set-contains? s-sym 'a))
(check-false (set-contains? s-sym 'd))
(check-true (set=? s-sym (list->set sym-comp '(c b a))))

(define str-comp (make-comparator string? string=? string<? string-hash))
(define s-str (set str-comp "apple" "banana"))
(check-true (set-contains? s-str "apple"))
(check-false (set-contains? s-str "pear"))

;; --- Error Handling ---
;; Check for comparator mismatch
(check-catch #t (set=? s-1 s-str))
(check-catch #t (set<=? s-1 s-str))
(check-catch #t (set-disjoint? s-1 s-str))

;; Check for non-set arguments
; (check-catch #t (set-size "not a set")) ; set-size is not exported
(check-catch #t (set-contains? "not a set" 1))

;; --- Large Set Test ---
(define (range n)
  (let loop ((i 0) (acc '()))
    (if (= i n) (reverse acc)
        (loop (+ i 1) (cons i acc)))))

(define big-n 100)
(define big-list (range big-n))
(define s-big (list->set comp big-list))
;; Check basic existence
(check-true (set-contains? s-big 0))
(check-true (set-contains? s-big (- big-n 1)))
(check-false (set-contains? s-big big-n))
;; Check subset logic on large sets
(define s-big-minus-one (set-copy s-big))
;; (We can't easily remove elements with current API, so let's build a smaller one)
(define s-small-big (list->set comp (range (- big-n 1))))
(check-true (set<? s-small-big s-big))
(check-true (set=? s-big (list->set comp big-list)))

(check-report)