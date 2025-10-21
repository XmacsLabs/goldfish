(import (liii check)
        (liii option))

(check-set-mode! 'report-failed)

#|
option
创建包含值的option对象。

语法
----
(option value)

参数
----
value : any
用于包装到option中的值。

返回值
-----
以option形式返回包装后的值对象。

说明
----
将任意值包装为option对象，用于函数式编程中处理可能缺失的值。

边界条件
--------
- 非空值：创建包含该值的option
- 空值：创建空的option

性能特征
--------
- 时间复杂度：O(1)，直接包装现有值
- 空间复杂度：O(1)，需要存储值引用

兼容性
------
- 支持所有option实例方法
- 与none函数配合使用
|#

;;; 测试构造函数
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '())))
  (check (opt1 :defined?) => #t)
  (check (opt2 :defined?) => #t)
  (check (opt3 :empty?) => #t))

;;; 测试基本操作
(let ((opt1 (option 42))
      (opt2 (none)))
  (check (opt1 :get) => 42)
  (check (opt1 :get-or-else 0) => 42)
  (check (opt2 :get-or-else 0) => 0))

;;; 测试谓词函数
(let ((opt1 (option 42))
      (opt2 (none)))
  (check-true (opt1 :defined?))
  (check-false (opt2 :defined?))
  (check-false (opt1 :empty?))
  (check-true (opt2 :empty?)))

;;; 测试高阶函数
(let ((opt1 (option 42))
      (opt2 (none)))
  (check-true (opt1 :forall (lambda (x) (= x 42))))
  (check-false (opt1 :forall (lambda (x) (= x 0))))
  (check-false (opt2 :forall (lambda (x) (= x 42))))

  (check-true (opt1 :exists (lambda (x) (= x 42))))
  (check-false (opt1 :exists (lambda (x) (= x 0))))
  (check-false (opt2 :exists (lambda (x) (= x 42))))

  (check-true (opt1 :contains 42))
  (check-false (opt1 :contains 0))
  (check-false (opt2 :contains 42)))

;;; 测试迭代操作
(let ((result '()))
  ((option 42) :for-each (lambda (x) (set! result (cons x result))))
  (check result => '(42)))

(let ((result '()))
  ((none) :for-each (lambda (x) (set! result (cons x result))))
  (check result => '()))

;;; 测试转换操作
(let ((opt1 (option 42))
      (opt2 (none)))
  (check ((opt1 :map (lambda (x) (+ x 1))) :get) => 43)
  (check ((opt2 :map (lambda (x) (+ x 1))) :empty?) => #t)

  (check ((opt1 :flat-map (lambda (x) (option (+ x 1)))) :get) => 43)
  (check ((opt2 :flat-map (lambda (x) (option (+ x 1)))) :empty?) => #t)

  (check ((opt1 :filter (lambda (x) (> x 40))) :get) => 42)
  (check ((opt1 :filter (lambda (x) (< x 40))) :empty?) => #t)
  (check ((opt2 :filter (lambda (x) (> x 40))) :empty?) => #t))

(check-report)