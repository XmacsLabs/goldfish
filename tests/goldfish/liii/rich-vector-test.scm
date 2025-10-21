(import (liii check)
        (scheme base)
        (liii rich-vector)
        (liii lang)
        (liii error))

(check-set-mode! 'report-failed)


#|
rich-vector@empty
创建一个空的rich-vector对象。

语法
----
(rich-vector :empty . args)

参数
----
args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-vector形式返回空的向量对象。

说明
----
创建一个不包含任何元素的rich-vector。通常用于初始化数据结构或作为
链式操作的起点。

边界条件
--------
- 无参数调用：返回空向量
- 支持链式调用：可与其他rich-vector方法组合使用

性能特征
--------
- 时间复杂度：O(1)，固定时间创建
- 空间复杂度：O(1)，创建空对象所需最小内存

兼容性
------
- 与所有rich-vector实例方法兼容
- 支持链式调用模式
|#

;; 基本测试
(check ((rich-vector :empty) :collect) => #())
(check ((rich-vector :empty) :length) => 0)
(check ((rich-vector :empty) :empty?) => #t)

;; 边界测试
(check ((rich-vector :empty :map (lambda (x) (* x 2))) :collect) => #())
(check ((rich-vector :empty :filter (lambda (x) #t)) :collect) => #())
(check ((rich-vector :empty :take 0) :collect) => #())
(check ((rich-vector :empty :drop 0) :collect) => #())
(check ((rich-vector :empty :reverse) :collect) => #())

;;; @empty 构造函数测试
(let ((empty-v (rich-vector :empty)))
  (check (empty-v :is-instance-of 'rich-vector) => #t)
  (check (= (empty-v :length) 0) => #t)
  (check (empty-v :empty?) => #t)
  (check (equal? (empty-v :to-list) '()) => #t)
  (check (equal? (empty-v :to-string) "#()") => #t))

;;; 测试构造函数
(let ((v (rich-vector #(1 2 3))))
  (check (v :is-instance-of 'rich-vector) => #t)
  (check (= (v :length) 3) => #t))

;;; 测试基本操作
(let ((v (rich-vector #(1 2 3))))
  (check (= (v :fold 0 +) 6) => #t)
  (check (= (v :head) 1) => #t)
  (check (= (v :last) 3) => #t))

;;; 测试元素查找
(let ((v (rich-vector #(1 2 3))))
  (check (= (v :index-of 2) 1) => #t)
  (check (v :contains 2) => #t))

;;; 测试转换
(let ((v (rich-vector #(1 2 3))))
  (check (equal? (v :to-list) '(1 2 3)) => #t))

;;; 测试函数式操作
(let ((v (rich-vector #(1 2 3))))
  (check (equal? ((v :map (lambda (x) (* x 2))) :to-list) '(2 4 6)) => #t)
  (check (equal? ((v :filter (lambda (x) (> x 1))) :to-list) '(2 3)) => #t))

#|
rich-vector@fill
创建一个包含重复元素的rich-vector对象。

语法
----
(rich-vector :fill n elem . args)

参数
----
n : integer
向量的长度，必须是非负整数。

elem : any
用于填充向量的元素。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-vector形式返回包含n个elem元素的向量对象。

说明
----
创建一个长度为n的向量，所有元素都是elem。

边界条件
--------
- n = 0：返回空向量
- n > 0：返回包含n个elem元素的向量
- n < 0：抛出value-error
- n不是整数：抛出type-error

性能特征
--------
- 时间复杂度：O(n)，需要初始化n个元素
- 空间复杂度：O(n)，需要存储n个元素

兼容性
------
- 与所有rich-vector实例方法兼容
- 支持链式调用模式
|#

;; 基本测试
(check ((rich-vector :fill 3 42) :collect) => #(42 42 42))
(check ((rich-vector :fill 0 42) :collect) => #())
(check ((rich-vector :fill 1 "hello") :collect) => #("hello"))

;; 边界测试
(check ((rich-vector :fill 3 42 :map (lambda (x) (+ x 1))) :collect) => #(43 43 43))
(check ((rich-vector :fill 2 "a" :filter (lambda (x) (string=? x "a"))) :collect) => #("a" "a"))

;;; @fill 构造函数测试
(let ((filled-v (rich-vector :fill 4 99)))
  (check (filled-v :is-instance-of 'rich-vector) => #t)
  (check (= (filled-v :length) 4) => #t)
  (check (equal? (filled-v :to-list) '(99 99 99 99)) => #t)
  (check (equal? (filled-v :to-string) "#(99 99 99 99)") => #t))

(check-report)