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
        (liii vector)
        (liii cut)
        (liii base)
        (only (scheme base) let-values))

(check-set-mode! 'report-failed)

(for-each (lambda (p) (check (procedure? p) => #t))
  (list
   vector-empty?
   vector-count
   vector-any vector-every vector-copy vector-copy!
   vector-index vector-index-right vector-partition
   vector-swap! vector-reverse! vector-cumulate reverse-list->vector
   vector=))

(check-true (vector? (int-vector 1 2 3)))
(check-catch 'wrong-type-arg (int-vector 1 2 'a))

#|
vector-ref
按索引访问向量中的元素。

语法
----
(vector-ref vector k)

参数
----
vector : vector?
要访问的向量

k : exact?
必须是非负的精确整数，表示要访问的索引位置，必须小于向量的长度

返回值
-----
any?
向量中位置k处的元素

说明
----
1. 从0开始索引，第一个元素的索引为0
2. 索引k必须在有效范围内：0 <= k < (vector-length vector)
3. 返回k位置处的元素，可以是任何类型的值
4. 向量是固定长度的数据结构，访问操作的时间复杂度为O(1)

错误处理
--------
out-of-range
当k为负数或大于等于向量长度时抛出错误。

wrong-type-arg
当vector不是向量或k不是精确整数时抛出错误。

|#

(let1 v #(1 2 3)
  (check (vector-ref v 0) => 1)
  (check (vector-ref v 1) => 2)
  (check (vector-ref v 2) => 3))

;; 边界情况测试
(let1 v #(a b c d)
  (check (vector-ref v 0) => 'a)  ; 第一个元素
  (check (vector-ref v 3) => 'd)) ; 最后一个元素

;; 空向量测试
(check-catch 'out-of-range (vector-ref #() 0))

;; 单元素向量测试
(check (vector-ref #(42) 0) => 42)

;; 错误处理测试
(let1 v #(1 2 3)
  ; 索引超出范围
  (check-catch 'out-of-range (vector-ref v -1))
  (check-catch 'out-of-range (vector-ref v 3)))

;; 不同类型向量测试
(let1 v #(1 2.5 "hello" symbol #\c #t #f)
  (check (vector-ref v 0) => 1)
  (check (vector-ref v 2) => "hello")
  (check (vector-ref v 4) => #\c)
  (check (vector-ref v 6) => #f))


#|
vector-set!
修改向量中指定位置的元素。

语法
----
(vector-set! vector k obj)

参数
----
vector : vector?
要修改的向量

k : exact?
必须是非负的精确整数，表示要修改的索引位置，必须小于向量的长度

obj : any?
要设置的新值，可以是任何类型的值

返回值
-----
未定义值

说明
----
1. 从0开始索引，第一个元素的索引为0
2. 索引k必须在有效范围内：0 <= k < (vector-length vector)
3. 将向量中位置k处的元素修改为obj
4. 向量是固定长度的数据结构，修改操作的时间复杂度为O(1)
5. 这是一个副作用操作，会直接修改原始向量

错误处理
--------
out-of-range
当k为负数或大于等于向量长度时抛出错误。

wrong-type-arg
当vector不是向量或k不是精确整数时抛出错误。
|#

;; 基本功能测试
(let1 v #(1 2 3)
  (vector-set! v 1 42)
  (check v => #(1 42 3)))

;; 边界情况测试
(let1 v #(a b c d)
  (vector-set! v 0 'x)  ; 修改第一个元素
  (vector-set! v 3 'y)  ; 修改最后一个元素
  (check v => #(x b c y)))

;; 单元素向量测试
(let1 v #(42)
  (vector-set! v 0 100)
  (check v => #(100)))

;; 不同类型值测试
(let1 v #(1 2 3 4 5)
  (vector-set! v 0 "string")
  (vector-set! v 1 3.14)
  (vector-set! v 2 'symbol)
  (vector-set! v 3 #\c)
  (vector-set! v 4 #t)
  (check v => #("string" 3.14 symbol #\c #t)))

;; 错误处理测试
(let1 v #(1 2 3)
  ; 索引超出范围
  (check-catch 'out-of-range (vector-set! v -1 42))
  (check-catch 'out-of-range (vector-set! v 3 42)))

#|
vector-length
获取向量的长度（元素个数）。

语法
----
(vector-length vector)

参数
----
vector : vector?
要获取长度的向量

返回值
-----
exact?
向量的长度，即包含的元素个数，是一个非负的精确整数

说明
----
1. 返回向量中元素的数量
2. 空向量的长度为0
3. 向量长度是固定的，创建后不会改变
4. 时间复杂度为O(1)

错误处理
--------
wrong-type-arg
当vector不是向量时抛出错误。

|#

;; 基本功能测试
(check (vector-length #()) => 0)  ; 空向量
(check (vector-length #(42)) => 1)  ; 单元素向量
(check (vector-length #(1 2 3)) => 3)  ; 多元素向量

;; 不同类型向量测试
(check (vector-length #(1 2.5 "hello" symbol #\c #t #f)) => 7)

;; 错误处理测试
(check-catch 'wrong-type-arg (vector-length 'not-a-vector))

#|
make-vector
创建指定长度的向量。

语法
----
(make-vector k)
(make-vector k fill)

参数
----
k : exact?
必须是非负的精确整数，表示要创建的向量长度

fill : any? (可选)
向量的初始值，可以是任何类型的值。如果未提供，则向量元素未定义

返回值
-----
vector?
新创建的向量，长度为k

说明
----
1. 创建一个长度为k的新向量
2. 如果提供了fill参数，则所有元素都初始化为fill
3. 如果未提供fill参数，则向量元素的值未定义
4. 向量长度是固定的，创建后不会改变
5. 时间复杂度为O(k)

错误处理
--------
wrong-type-arg
当k不是精确整数时抛出错误。

out-of-range
当k为负数时抛出错误。

示例
----
(make-vector 3) => #(#<undefined> #<undefined> #<undefined>)
(make-vector 3 0) => #(0 0 0)
(make-vector 2 'a) => #(a a)
(make-vector 0) => #()
|#

;;; make-vector 测试

;; 基本功能测试
(check (vector-length (make-vector 0)) => 0)  ; 空向量
(check (vector-length (make-vector 3)) => 3)  ; 长度为3的向量

;; 带初始值的测试
(check (make-vector 3 0) => #(0 0 0))  ; 初始化为0
(check (make-vector 2 'a) => #(a a))  ; 初始化为符号
(check (make-vector 1 "hello") => #("hello"))  ; 初始化为字符串

;; 边界情况测试
(check (make-vector 0) => #())  ; 长度为0的向量
(check (vector-length (make-vector 1)) => 1)  ; 长度为1的向量，无初始值

;; 不同类型初始值测试
(let1 v (make-vector 5 3.14)
  (check (vector-length v) => 5)
  (check (vector-ref v 0) => 3.14)
  (check (vector-ref v 4) => 3.14))

;; 错误处理测试
(check-catch 'wrong-type-arg (make-vector 'not-a-number))  ; 非数字参数
(check-catch 'wrong-type-arg (make-vector -1))  ; 负长度
(check-catch 'wrong-type-arg (make-vector -1 0))  ; 负长度带初始值

#|
list->vector
将列表转换为向量。

语法
----
(list->vector list)

参数
----
list : list?
要转换为向量的列表

返回值
-----
vector?
新创建的向量，包含列表中的所有元素，顺序与列表相同

说明
----
1. 创建一个新向量，长度等于列表的长度
2. 向量中的元素顺序与列表中的元素顺序相同
3. 列表中的每个元素都会被复制到向量中
4. 向量长度是固定的，创建后不会改变
5. 时间复杂度为O(n)，其中n是列表的长度

错误处理
--------
wrong-type-arg
当list不是列表时抛出错误。

示例
----
(list->vector '()) => #()
(list->vector '(a b c)) => #(a b c)
(list->vector '(1 2 3)) => #(1 2 3)
(list->vector '(1 "hello" #\c)) => #(1 "hello" #\c)
|#

;;; list->vector 测试

;; 基本功能测试
(check (list->vector '()) => #())  ; 空列表
(check (list->vector '(a b c)) => #(a b c))  ; 符号列表
(check (list->vector '(1 2 3)) => #(1 2 3))  ; 数字列表

;; 边界情况测试
(check (list->vector '(42)) => #(42))  ; 单元素列表
(check (list->vector '(a)) => #(a))  ; 单符号列表

;; 不同类型元素测试
(check (list->vector '(1 2.5 "hello" symbol #\c #t #f)) => #(1 2.5 "hello" symbol #\c #t #f))

;; 嵌套结构测试
(check (list->vector '((1 2) (3 4))) => #((1 2) (3 4)))  ; 嵌套列表

;; 错误处理测试
(check-catch 'wrong-type-arg (list->vector 'not-a-list))  ; 非列表参数
(check-catch 'wrong-type-arg (list->vector '(1 2 . 3)))  ; 非正规列表

#|
vector->list
将向量转换为列表。

语法
----
(vector->list vector)
(vector->list vector start)
(vector->list vector start end)

参数
----
vector : vector?
要转换为列表的向量

start : exact? (可选)
必须是非负的精确整数，表示起始索引位置，默认为0

end : exact? (可选)
必须是非负的精确整数，表示结束索引位置，默认为向量的长度

返回值
-----
list?
新创建的列表，包含向量中指定范围内的元素，顺序与向量相同

说明
----
1. 创建一个新列表，长度等于指定范围内的元素数量
2. 列表中的元素顺序与向量中的元素顺序相同
3. 向量中的每个元素都会被复制到列表中
4. 如果未指定 start 和 end，则转换整个向量
5. 如果只指定 start，则从 start 开始到向量末尾
6. 时间复杂度为O(n)，其中n是转换的元素数量

错误处理
--------
out-of-range
当start或end为负数，或start大于end，或end大于向量长度时抛出错误。

wrong-type-arg
当vector不是向量，或start/end不是精确整数时抛出错误。

示例
----
(vector->list #()) => ()
(vector->list #(a b c)) => (a b c)
(vector->list #(1 2 3)) => (1 2 3)
(vector->list #(1 "hello" #\c)) => (1 "hello" #\c)
(vector->list #(1 2 3 4) 1) => (2 3 4)
(vector->list #(1 2 3 4) 1 3) => (2 3)
|#

;;; vector->list 测试

;; 基本功能测试
(check (vector->list #()) => ())  ; 空向量
(check (vector->list #(a b c)) => '(a b c))  ; 符号向量
(check (vector->list #(1 2 3)) => '(1 2 3))  ; 数字向量

;; 边界情况测试
(check (vector->list #(42)) => '(42))  ; 单元素向量
(check (vector->list #(a)) => '(a))  ; 单符号向量

;; 不同类型元素测试
(check (vector->list #(1 2.5 "hello" symbol #\c #t #f)) => '(1 2.5 "hello" symbol #\c #t #f))

;; 嵌套结构测试
(check (vector->list #((1 2) (3 4))) => '((1 2) (3 4)))  ; 嵌套向量

;; 带索引参数的测试
(check (vector->list #(0 1 2 3) 1) => '(1 2 3))  ; 从索引1开始
(check (vector->list #(0 1 2 3) 2) => '(2 3))  ; 从索引2开始
(check (vector->list #(0 1 2 3) 3) => '(3))  ; 从索引3开始
(check (vector->list #(0 1 2 3) 4) => ())  ; 从索引4开始（空列表）

;; 带起始和结束索引的测试
(check (vector->list #(0 1 2 3) 1 3) => '(1 2))  ; 索引1到3（不包括3）
(check (vector->list #(0 1 2 3) 0 4) => '(0 1 2 3))  ; 整个向量
(check (vector->list #(0 1 2 3) 1 1) => ())  ; 空范围
(check (vector->list #(0 1 2 3) 2 4) => '(2 3))  ; 索引2到4

;; 错误处理测试
(let1 v #(1 2 3)
  ; 索引超出范围
  (check-catch 'out-of-range (vector->list v -1))  ; 负索引
  (check-catch 'out-of-range (vector->list v 4))  ; 索引超出长度
  (check-catch 'out-of-range (vector->list v 2 5))  ; 结束索引超出长度
  (check-catch 'out-of-range (vector->list v 3 2)))  ; 起始索引大于结束索引

;; 类型错误测试
(check-catch 'wrong-type-arg (vector->list 'not-a-vector))  ; 非向量参数
(check-catch 'wrong-type-arg (vector->list #(1 2 3) 'not-a-number))  ; 非数字起始索引
(check-catch 'wrong-type-arg (vector->list #(1 2 3) 0 'not-a-number))  ; 非数字结束索引

#|
vector-copy
创建向量的副本。

语法
----
(vector-copy vector)
(vector-copy vector start)
(vector-copy vector start end)

参数
----
vector : vector?
要复制的向量

start : exact? (可选)
必须是非负的精确整数，表示起始索引位置，默认为0

end : exact? (可选)
必须是非负的精确整数，表示结束索引位置，默认为向量的长度

返回值
-----
vector?
新创建的向量，包含原始向量中指定范围内的元素

说明
----
1. 创建一个新向量，长度等于指定范围内的元素数量
2. 新向量中的元素与原始向量中对应位置的元素相同（通过eqv?比较）
3. 新向量与原始向量是不同的对象（通过eq?比较）
4. 如果未指定 start 和 end，则复制整个向量
5. 如果只指定 start，则从 start 开始到向量末尾
6. 时间复杂度为O(n)，其中n是复制的元素数量

错误处理
--------
out-of-range
当start或end为负数，或start大于end，或end大于向量长度时抛出错误。

wrong-type-arg
当vector不是向量，或start/end不是精确整数时抛出错误。

示例
----
(vector-copy #(1 2 3)) => #(1 2 3)
(vector-copy #(1 2 3 4) 1) => #(2 3 4)
(vector-copy #(1 2 3 4) 1 3) => #(2 3)
(vector-copy #(1 2 3 4) 4) => #()
|#

;;; vector-copy 测试

;; 基本功能测试
(check (vector-copy #(0 1 2 3)) => #(0 1 2 3))
(check (vector-copy #(0 1 2 3) 1) => #(1 2 3))
(check (vector-copy #(0 1 2 3) 3) => #(3))
(check (vector-copy #(0 1 2 3) 4) => #())

(check-catch 'out-of-range (vector-copy #(0 1 2 3) 5))
(check-catch 'out-of-range (vector-copy #(0 1 2 3) 1 5))

(define my-vector #(0 1 2 3))
(check (eqv? my-vector (vector-copy #(0 1 2 3))) => #f)
(check-true
  (eqv? (vector-ref my-vector 2)
        (vector-ref (vector-copy #(0 1 2 3)) 2)))

(check (vector-copy #(0 1 2 3) 1 1) => #())
(check (vector-copy #(0 1 2 3) 1 2) => #(1))
(check (vector-copy #(0 1 2 3) 1 4) => #(1 2 3))

;; 空向量测试
(check (vector-copy #()) => #())  ; 空向量复制
(check (vector-copy #() 0) => #())  ; 空向量从索引0开始
(check (vector-copy #() 0 0) => #())  ; 空向量指定空范围

;; 单元素向量测试
(check (vector-copy #(42)) => #(42))  ; 单元素向量
(check (vector-copy #(42) 0) => #(42))  ; 单元素向量从索引0开始
(check (vector-copy #(42) 0 1) => #(42))  ; 单元素向量指定范围
(check (vector-copy #(42) 1) => #())  ; 单元素向量从索引1开始

;; 不同类型元素测试
(let1 v #(1 2.5 "hello" symbol #\c #t #f)
  (check (vector-copy v) => v)  ; 完整复制
  (check (vector-copy v 2 5) => #("hello" symbol #\c)))  ; 部分复制

;; 嵌套结构测试
(check (vector-copy #((1 2) (3 4))) => #((1 2) (3 4)))  ; 嵌套向量
(check (vector-copy #((1 2) (3 4)) 1) => #((3 4)))  ; 嵌套向量部分复制

;; 对象标识测试
(let1 original #(a b c)
  (let1 copied (vector-copy original)
    (check-true (vector? copied))  ; 是向量
    (check-false (eq? original copied))  ; 不是同一个对象
    (check-true (eqv? (vector-ref original 1) (vector-ref copied 1)))  ; 元素相同
    (check (vector-length copied) => (vector-length original))))  ; 长度相同

;; 修改独立性测试
(let1 original #(1 2 3)
  (let1 copied (vector-copy original)
    (vector-set! copied 1 99)  ; 修改副本
    (check original => #(1 2 3))  ; 原始向量不变
    (check copied => #(1 99 3))))  ; 副本已修改

;; 更多边界范围测试
(check (vector-copy #(0 1 2 3) 0 0) => #())  ; 空范围
(check (vector-copy #(0 1 2 3) 2 2) => #())  ; 空范围
(check (vector-copy #(0 1 2 3) 0 1) => #(0))  ; 第一个元素
(check (vector-copy #(0 1 2 3) 3 4) => #(3))  ; 最后一个元素

;; 错误处理测试 - 类型错误
(check-catch 'wrong-type-arg (vector-copy 'not-a-vector))  ; 非向量参数
(check-catch 'wrong-type-arg (vector-copy #(1 2 3) 'not-a-number))  ; 非数字起始索引
(check-catch 'wrong-type-arg (vector-copy #(1 2 3) 0 'not-a-number))  ; 非数字结束索引

;; 错误处理测试 - 范围错误
(let1 v #(1 2 3)
  (check-catch 'out-of-range (vector-copy v -1))  ; 负起始索引
  (check-catch 'out-of-range (vector-copy v 4))  ; 起始索引超出长度
  (check-catch 'out-of-range (vector-copy v 2 5))  ; 结束索引超出长度
  (check-catch 'out-of-range (vector-copy v 3 2)))  ; 起始索引大于结束索引

(check-true (int-vector? (int-vector 1 2 3)))
(check-false (int-vector? (vector 1 2 3)))

(check-true (vector-empty? (vector)))
(check-false (vector-empty? (vector 1)))
(check-catch 'type-error (vector-empty? 1))

; trivial cases
(check-true (vector= eq?))
(check-true (vector= eq? '#(a)))
; basic cases
(check-true (vector= eq? '#(a b c d) '#(a b c d)))
(check-false (vector= eq? '#(a b c d) '#(a b d c)))
(check-false (vector= = '#(1 2 3 4 5) '#(1 2 3 4)))
(check-true (vector= = '#(1 2 3 4) '#(1 2 3 4)))
(check-true (vector= equal? '#(1 2 3) '#(1 2 3) '#(1 2 3)))
(check-false (vector= equal? '#(1 2 3) '#(1 2 3) '#(1 2 3 4)))
; error cases
(check-catch 'type-error (vector= 1 (vector (vector 'a)) (vector (vector 'a))))
; complex cases in srfi-133
(check-true (vector= equal? (vector (vector 'a)) (vector (vector 'a))))
(check-false (vector= eq? (vector (vector 'a)) (vector (vector 'a))))
(check (vector-fold + 0 #(1 2 3 4)) => 10)  ; 1 + 2 + 3 + 4 = 10
(check (vector-fold * 1 #(1 2 3 4)) => 24)  ; 1 * 2 * 3 * 4 = 24

(check (vector-fold (lambda (x acc) (cons x acc)) '() #(1 2 3)) => '(3 2 1))

(check (vector-fold (lambda (x acc) (+ acc (if (even? x) 1 0))) 0 #(1 2 3 4)) => 2)

(check (vector-fold + 0 #()) => 0)
(check (vector-fold * 1 #()) => 1)

(check (vector-fold + 0 #(5)) => 5)
(check (vector-fold * 1 #(5)) => 5)

(check (vector-fold string-append "" #("a" "b" "c")) => "cba")
(check (vector-fold (lambda (x acc) (and acc x)) #t #(#t #t #f)) => #f)

(check (vector-fold-right + 0 #(1 2 3 4)) => 10)  ; 4 + 3 + 2 + 1 = 10
(check (vector-fold-right * 1 #(1 2 3 4)) => 24)  ; 4 * 3 * 2 * 1 = 24
(check (vector-fold-right (lambda (x acc) (cons x acc)) '() #(1 2 3)) => '(1 2 3))  ; 保持顺序
(check (vector-fold-right (lambda (x acc) (+ acc (if (even? x) 1 0))) 0 #(1 2 3 4)) => 2)  ; 统计偶数个数

(check (vector-fold-right + 0 #()) => 0)
(check (vector-fold-right * 1 #()) => 1)

(check (vector-fold-right + 0 #(5)) => 5)
(check (vector-fold-right * 1 #(5)) => 5)

(check (vector-fold-right string-append "" #("a" "b" "c")) => "abc")
(check (vector-fold-right (lambda (x acc) (and acc x)) #t #(#t #t #f)) => #f)

(check (vector-fold (lambda (x acc) (cons x acc)) '() #(1 2 3)) => '(3 2 1))  ; vector-fold 反转向量
(check (vector-fold-right (lambda (x acc) (cons x acc)) '() #(1 2 3)) => '(1 2 3))  ; vector-fold-right 保持顺序

(check
  (let ((lst (make-list 5)))
    (vector-for-each
      (lambda (i) (list-set! lst i (* i i)))
      #(0 1 2 3 4))
    lst)
  => '(0 1 4 9 16))

(check
  (let ((lst (make-list 5)))
    (vector-for-each
      (lambda (i) (list-set! lst i (* i i)))
      #(0 1 2))
    lst)
  => '(0 1 4 #f #f))

(check
  (let ((lst (make-list 5)))
    (vector-for-each
      (lambda (i) (list-set! lst i (* i i)))
      #())
    lst)
  => '(#f #f #f #f #f))

(check (vector-count even? #()) => 0)
(check (vector-count even? #(1 3 5 7 9)) => 0)
(check (vector-count even? #(1 3 4 7 8)) => 2)

; Trivial cases.
(check (vector-cumulate + 0 '#(1 2 3 4)) => #(1 3 6 10))
(check (vector-cumulate - 0 '#(1 2 3 4)) => #(-1 -3 -6 -10))
(check (vector-cumulate * 1 '#(-1 -2 -3 -4)) => #(-1 2 -6 24))

;;; Test cases of vec.
; Not a vec input.
(check-catch 'type-error (vector-cumulate + 0 'a))
; Empty vec test.
(check (vector-cumulate + 0 '#()) => #())

;; Test cases of fn.
; A case with consant fn.
(check (vector-cumulate (lambda (x y) 'a) 0 '#(1 2 3)) => #(a a a))
; A wrong-number-of-args case with 1-arg fn.
(check-catch 'wrong-number-of-args (vector-cumulate (lambda (x) 'a) 0 '#(1 2 3)))
; A wrong-type-arg case with args can't be mapped by fn.
(check-catch 'wrong-type-arg (vector-cumulate + '(1) '#(1 2 3)))

;;; Test cases of knil.
; A case of different type of knil/cumu and vec-i.
(check (vector-cumulate (lambda (x y) (+ x 2)) 0 '#('a 'b 'c)) => #(2 4 6))
(check (vector-any even? #()) => #f)
(check (vector-any even? #(1 3 5 7 9)) => #f)
(check (vector-any even? #(1 3 4 7 8)) => #t)

(check (vector-every odd? #()) => #t)
(check (vector-every odd? #(1 3 5 7 9)) => #t)
(check (vector-every odd? #(1 3 4 7 8)) => #f)

(check (vector-index even? #()) => #f)
(check (vector-index even? #(1 3 5 7 9)) => #f)
(check (vector-index even? #(1 3 4 7 8)) => 2)

(check (vector-index-right even? #()) => #f)
(check (vector-index-right even? #(1 3 5 7 9)) => #f)
(check (vector-index-right even? #(1 3 4 7 8)) => 4)

(check (vector-skip even? #(1 2 3 4)) => 0)  ; 第一个元素 1 不满足 even?
(check (vector-skip odd? #(1 3 5 7)) => #f)  ; 所有元素都满足 odd?
(check (vector-skip (lambda (x) (< x 5)) #(1 2 3 4 5)) => 4)  ; 第一个不满足谓词的元素是 5
(check (vector-skip (lambda (x) (char=? x #\a)) #(#\a #\a #\b #\c)) => 2)  ; 第一个不满足谓词的元素是 #\b

(check (vector-skip even? #()) => #f)  ; 空向量没有元素，返回 #f

(check (vector-skip even? #(1)) => 0)  ; 第一个元素 1 不满足 even?
(check (vector-skip odd? #(2)) => 0)  ; 第一个元素 2 满足 even?

(check (vector-skip (lambda (x) (string=? x "a")) #("a" "a" "b" "c")) => 2)  ; 第一个不满足谓词的元素是 "b"
(check (vector-skip (lambda (x) (eq? x #t)) #(#t #t #f #t)) => 2)  ; 第一个不满足谓词的元素是 #f

(check (vector-skip (lambda (x) (> x 0)) #(1 2 3 4)) => #f)  ; 所有元素都满足谓词
(check (vector-skip (lambda (x) (char-alphabetic? x)) #(#\a #\b #\c)) => #f)  ; 所有元素都满足谓词

(check (vector-skip-right even? #(1 2 3 4)) => 2)  ; 从右开始，第一个不满足 even? 的元素是 3
(check (vector-skip-right odd? #(1 3 5 7)) => #f)  ; 所有元素都满足 odd?
(check (vector-skip-right (lambda (x) (< x 5)) #(1 2 3 4 5)) => 4)  ; 从右开始，第一个不满足谓词的元素是 5
(check (vector-skip-right (lambda (x) (char=? x #\a)) #(#\a #\a #\b #\c)) => 3)  ; 从右开始，第一个不满足谓词的元素是 #\c

(check (vector-skip-right even? #()) => #f)  ; 空向量没有元素，返回 #f

(check (vector-skip-right even? #(1)) => 0)  ; 第一个元素 1 不满足 even?
(check (vector-skip-right odd? #(2)) => 0)  ; 第一个元素 2 满足 even?

(check (vector-skip-right (lambda (x) (string=? x "a")) #("a" "a" "b" "c")) => 3)  ; 从右开始，第一个不满足谓词的元素是 "c"
(check (vector-skip-right (lambda (x) (eq? x #t)) #(#t #t #f #t)) => 2)  ; 从右开始，第一个不满足谓词的元素是 #f

(check (vector-skip-right (lambda (x) (> x 0)) #(1 2 3 4)) => #f)  ; 所有元素都满足谓词
(check (vector-skip-right (lambda (x) (char-alphabetic? x)) #(#\a #\b #\c)) => #f)  ; 所有元素都满足谓词

(define (vector-partition->list pred v)
  (let-values (((ret cnt) (vector-partition pred v))) (list ret cnt)))

(check (vector-partition->list even? #()) => '(#() 0))
(check (vector-partition->list even? #(1 3 5 7 9)) => '(#(1 3 5 7 9) 0))
(check (vector-partition->list even? #(1 3 4 7 8)) => '(#(4 8 1 3 7) 2))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 1 2)
(check my-vector => #(0 2 1 3))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 1 1)
(check my-vector => #(0 1 2 3))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 0 (- (vector-length my-vector) 1))
(check my-vector => #(3 1 2 0))

(check-catch 'out-of-range
  (vector-swap! my-vector 1 (vector-length my-vector)))

(let ((vec (vector 1 2 3 4)))
  (vector-reverse! vec)
  (check vec => #(4 3 2 1)))

(let ((vec (vector 'a 'b 'c 'd)))
  (vector-reverse! vec 1 3)
  (check vec => #(a c b d)))

(let ((vec (vector 10 20 30)))
  (vector-reverse! vec 2 2)
  (check vec => #(10 20 30)))

(check-catch 'wrong-number-of-args 
  (vector-reverse! (vector 1 2) 0 2 3)) 

(check-catch 'type-error 
  (vector-reverse! (vector 1 2) 'a 2)) 

(check-catch 'type-error 
  (vector-reverse! (vector 1 2) 0 'b)) 

(check-catch 'out-of-range 
  (vector-reverse! (vector 1 2) -1 2)) 

(check-catch 'out-of-range 
  (vector-reverse! (vector 1 2) 0 5)) 

(check-catch 'out-of-range 
  (vector-reverse! (vector 1 2) 2 1))

(let ((vec (vector)))
  (vector-reverse! vec 0 0)
  (check vec => #()))

(let ((vec (vector 100)))
  (vector-reverse! vec)
  (check vec => #(100)))

(let ((vec (vector 1 2 3)))
  (vector-reverse! vec)
  (vector-reverse! vec)
  (check vec => #(1 2 3)))

(define my-vector (vector 0 1 2 3 4))
(fill! my-vector #f)
(check my-vector => #(#f #f #f #f #f)) 

(define my-vector (vector 0 1 2 3 4))
(fill! my-vector #f 1 2)
(check my-vector => #(0 #f 2 3 4))

#|
vector-fill!
用指定的值填充向量的元素。

语法
----
(vector-fill! vector fill)
(vector-fill! vector fill start)
(vector-fill! vector fill start end)

参数
----
vector : vector?
要填充的向量

fill : any?
要填充的值，可以是任何类型的值

start : exact? (可选)
必须是非负的精确整数，表示起始索引位置，默认为0

end : exact? (可选)
必须是非负的精确整数，表示结束索引位置，默认为向量的长度

返回值
-----
未定义值

说明
----
1. 将向量中指定范围内的所有元素设置为fill值
2. 如果未指定start和end，则填充整个向量
3. 如果只指定start，则从start开始到向量末尾
4. 这是一个副作用操作，会直接修改原始向量
5. 时间复杂度为O(n)，其中n是填充的元素数量

错误处理
--------
out-of-range
当start或end为负数，或start大于end，或end大于向量长度时抛出错误。

wrong-type-arg
当vector不是向量，或start/end不是精确整数时抛出错误。

示例
----
(let ((v (vector 1 2 3 4))) (vector-fill! v 0) v) => #(0 0 0 0)
(let ((v (vector 1 2 3 4))) (vector-fill! v 'a 1) v) => #(1 a a a)
(let ((v (vector 1 2 3 4))) (vector-fill! v #\x 1 3) v) => #(1 #\x #\x 4)
|#

;;; vector-fill! 测试

;; 基本功能测试
(let1 v (vector 1 2 3 4)
  (vector-fill! v 0)
  (check v => #(0 0 0 0)))

(let1 v (vector 'a 'b 'c 'd)
  (vector-fill! v 'x)
  (check v => #(x x x x)))

;; 带起始索引的测试
(let1 v (vector 1 2 3 4)
  (vector-fill! v 'a 1)
  (check v => #(1 a a a)))

(let1 v (vector 1 2 3 4)
  (vector-fill! v #\x 2)
  (check v => #(1 2 #\x #\x)))

;; 带起始和结束索引的测试
(let1 v (vector 1 2 3 4)
  (vector-fill! v #\x 1 3)
  (check v => #(1 #\x #\x 4)))

(let1 v (vector 1 2 3 4)
  (vector-fill! v "hello" 0 2)
  (check v => #("hello" "hello" 3 4)))

;; 空向量测试
(let1 v (vector)
  (vector-fill! v 42)
  (check v => #()))

(let1 v (vector)
  (vector-fill! v 42 0 0)
  (check v => #()))

;; 单元素向量测试
(let1 v (vector 100)
  (vector-fill! v 999)
  (check v => #(999)))

(let1 v (vector 100)
  (vector-fill! v 999 0 1)
  (check v => #(999)))

;; 不同类型值测试
(let1 v (vector 1 2 3 4 5)
  (vector-fill! v "string")
  (check v => #("string" "string" "string" "string" "string")))

(let1 v (vector 1 2 3 4 5)
  (vector-fill! v 3.14 1 4)
  (check v => #(1 3.14 3.14 3.14 5)))

(let1 v (vector 1 2 3 4 5)
  (vector-fill! v 'symbol 2 5)
  (check v => #(1 2 symbol symbol symbol)))

(let1 v (vector 1 2 3 4 5)
  (vector-fill! v #\c 3 4)
  (check v => #(1 2 3 #\c 5)))

(let1 v (vector 1 2 3 4 5)
  (vector-fill! v #t 0 1)
  (check v => #(#t 2 3 4 5)))

;; 边界范围测试
(let1 v (vector 0 1 2 3)
  (vector-fill! v 99 0 0)  ; 空范围
  (check v => #(0 1 2 3)))

(let1 v (vector 0 1 2 3)
  (vector-fill! v 99 2 2)  ; 空范围
  (check v => #(0 1 2 3)))

(let1 v (vector 0 1 2 3)
  (vector-fill! v 99 0 1)  ; 第一个元素
  (check v => #(99 1 2 3)))

(let1 v (vector 0 1 2 3)
  (vector-fill! v 99 3 4)  ; 最后一个元素
  (check v => #(0 1 2 99)))

;; 错误处理测试 - 类型错误
(check-catch 'wrong-type-arg (vector-fill! 'not-a-vector 42))  ; 非向量参数
(check-catch 'wrong-type-arg (vector-fill! #(1 2 3) 42 'not-a-number))  ; 非数字起始索引
(check-catch 'wrong-type-arg (vector-fill! #(1 2 3) 42 0 'not-a-number))  ; 非数字结束索引

;; 错误处理测试 - 范围错误
(let1 v #(1 2 3)
  (check-catch 'out-of-range (vector-fill! v 42 -1))  ; 负起始索引
  (check-catch 'out-of-range (vector-fill! v 42 4))  ; 起始索引超出长度
  (check-catch 'out-of-range (vector-fill! v 42 2 5))  ; 结束索引超出长度
  (check-catch 'out-of-range (vector-fill! v 42 3 2)))  ; 起始索引大于结束索引 

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))

;(< at 0)
(check-catch 'out-of-range (vector-copy! b -1 a))

;(< start 0)
(check-catch 'out-of-range (vector-copy! b 0 a -1))

;(> start (vector-length from))
(check-catch 'out-of-range (vector-copy! b 0 a 6))

;(> end (vector-length from))
(check-catch 'out-of-range (vector-copy! b 0 a 0 6))

;(> start end)
(check-catch 'out-of-range (vector-copy! b 0 a 2 1))

;(> (+ at (- end start)) (vector-length to))
(check-catch 'out-of-range (vector-copy! b 6 a))

(check-catch 'out-of-range (vector-copy! b 1 a))

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))
(vector-copy! b 0 a 1)
(check b => #("a1" "a2" "a3" "a4" "b4"))

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))
(vector-copy! b 0 a 0 5)
(check b => #("a0" "a1" "a2" "a3" "a4")) 

(check (reverse-list->vector '()) => '#())
(check (reverse-list->vector '(1 2 3)) => '#(3 2 1))

(check-catch 'type-error (reverse-list->vector '(1 2 . 3)))

(check-catch 'type-error (reverse-list->vector (circular-list 1 2 3)))

(check (vector->string (vector #\0 #\1 #\2 #\3)) => "0123")
(check (vector->string (vector #\a #\b #\c)) => "abc")

(check (vector->string (vector #\0 #\1 #\2 #\3) 0 4) => "0123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1) => "123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 4) => "123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 3) => "12")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 2) => "1")

(check-catch 'out-of-range (vector->string (vector #\0 #\1 #\2 #\3) 2 10))

(check (vector->string (vector 0 1 #\2 3 4) 2 3) => "2")

(check-catch 'wrong-type-arg (vector->string (vector 0 1 #\2 3 4) 1 3))

(check (string->vector "0123") => (vector #\0 #\1 #\2 #\3))
(check (string->vector "abc") => (vector #\a #\b #\c))

(check (string->vector "0123" 0 4) => (vector #\0 #\1 #\2 #\3))
(check (string->vector "0123" 1) => (vector #\1 #\2 #\3))
(check (string->vector "0123" 1 4) => (vector #\1 #\2 #\3))
(check (string->vector "0123" 1 3) => (vector #\1 #\2))
(check (string->vector "0123" 1 2) => (vector #\1))

(check-catch 'out-of-range (string->vector "0123" 2 10))

(check (vector-filter even? #(1 2 3 4 5 6)) => #(2 4 6))
(check (vector-filter (lambda (x) (> x 3)) #(1 2 3 4 5 6)) => #(4 5 6))
(check (vector-filter (lambda (x) (string? x)) #(1 "a" 2 "b" 3)) => #("a" "b"))
(check (vector-filter (lambda (x) #t) #()) => #())
(check (vector-filter (lambda (x) #f) #(1 2 3)) => #())

#|
vector-append
连接多个向量，创建一个新向量。

语法
----
(vector-append vector ...)

参数
----
vector : vector?
要连接的向量，可以接受零个或多个向量参数

返回值
-----
vector?
新创建的向量，包含所有输入向量的元素，按参数顺序连接

说明
----
1. 创建一个新向量，长度等于所有输入向量长度的总和
2. 新向量中的元素按参数顺序排列：第一个向量的元素在前，然后是第二个向量的元素，依此类推
3. 如果未提供任何向量参数，则返回空向量
4. 如果只提供一个向量参数，则返回该向量的副本
5. 新向量与输入向量是不同的对象
6. 时间复杂度为O(n)，其中n是所有输入向量中元素的总数

错误处理
--------
wrong-type-arg
当任何参数不是向量时抛出错误。

示例
----
(vector-append) => #()
(vector-append #(1 2 3)) => #(1 2 3)
(vector-append #(1 2) #(3 4)) => #(1 2 3 4)
(vector-append #(a b) #(c d) #(e f)) => #(a b c d e f)
(vector-append #() #(1) #(2 3)) => #(1 2 3)
|#

;;; vector-append 测试

;; 基本功能测试
(check (vector-append) => #())  ; 无参数
(check (vector-append #(1 2 3)) => #(1 2 3))  ; 单向量
(check (vector-append #(1 2) #(3 4)) => #(1 2 3 4))  ; 两个向量
(check (vector-append #(a b) #(c d) #(e f)) => #(a b c d e f))  ; 三个向量

;; 空向量测试
(check (vector-append #()) => #())  ; 单个空向量
(check (vector-append #() #()) => #())  ; 两个空向量
(check (vector-append #() #(1) #()) => #(1))  ; 混合空向量

;; 单元素向量测试
(check (vector-append #(42)) => #(42))  ; 单元素向量
(check (vector-append #(1) #(2) #(3)) => #(1 2 3))  ; 多个单元素向量

;; 不同类型元素测试
(check (vector-append #(1 2.5) #("hello" symbol) #(#\c #t #f)) => #(1 2.5 "hello" symbol #\c #t #f))

;; 嵌套结构测试
(check (vector-append #((1 2)) #((3 4))) => #((1 2) (3 4)))  ; 嵌套向量

;; 对象标识测试
(let1 original #(a b c)
  (let1 appended (vector-append original)
    (check-true (vector? appended))  ; 是向量
    (check-false (eq? original appended))  ; 不是同一个对象
    (check (vector-length appended) => (vector-length original))))  ; 长度相同

;; 修改独立性测试
(let1 v1 #(1 2 3)
  (let1 v2 #(4 5 6)
    (let1 result (vector-append v1 v2)
      (vector-set! result 0 99)  ; 修改结果向量
      (check v1 => #(1 2 3))  ; 原始向量不变
      (check v2 => #(4 5 6))  ; 原始向量不变
      (check result => #(99 2 3 4 5 6)))))  ; 结果已修改

;; 错误处理测试
(check-catch 'wrong-type-arg (vector-append 'not-a-vector))  ; 非向量参数
(check-catch 'wrong-type-arg (vector-append #(1 2) 'not-a-vector))  ; 混合参数
(check-catch 'wrong-type-arg (vector-append #(1 2) 3 #(4 5)))  ; 数字参数

(check-report)

