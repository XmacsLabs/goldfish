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

#|
none
创建空的option对象。

语法
----
(none)

参数
----
无参数。

返回值
-----
返回空的option对象。

说明
----
创建表示缺失值的空option对象。

边界条件
--------
- 总是返回空的option对象

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 支持所有option实例方法
|#

;;; 测试none函数
(let ((opt (none)))
  (check (opt :empty?) => #t)
  (check (opt :defined?) => #f))

#|
option%get
获取option对象中的值。

语法
----
(option%get)

参数
----
无参数。

返回值
-----
返回option对象中包装的值。

说明
----
从option对象中提取包装的值。如果option为空，会抛出错误。

边界条件
--------
- 非空option：返回包装的值
- 空option：抛出错误

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
|#

;;; 测试option%get方法
(let ((opt1 (option 42))
      (opt2 (option "hello")))
  (check (opt1 :get) => 42)
  (check (opt2 :get) => "hello"))

#|
option%get-or-else
安全获取option对象中的值，如果option为空则返回默认值。

语法
----
(option%get-or-else default)

参数
----
default : any
当option为空时返回的默认值，可以是任意值或返回值的函数。

返回值
-----
如果option非空，返回包装的值；如果option为空，返回默认值。

说明
----
安全地从option对象中提取包装的值，避免空option的错误。

边界条件
--------
- 非空option：返回包装的值
- 空option：返回默认值
- 默认值为函数：调用函数并返回结果

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
|#

;;; 测试option%get-or-else方法
(let ((opt1 (option 42))
      (opt2 (none)))
  (check (opt1 :get-or-else 0) => 42)
  (check (opt2 :get-or-else 0) => 0)
  (check (opt2 :get-or-else (lambda () "default")) => "default"))

#|
option%or-else
链式操作option对象，如果当前option为空则返回备选option。

语法
----
(option%or-else default . args)

参数
----
default : option
当当前option为空时返回的备选option对象。
args : any
可选的额外参数，用于链式操作。

返回值
-----
如果当前option非空，返回当前option；如果当前option为空，返回备选option。

说明
----
提供链式操作option对象的能力，支持多个备选option的链式调用。

边界条件
--------
- 非空option：返回当前option
- 空option：返回备选option
- 参数类型检查：default必须是option类型

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
|#

;;; 测试option%or-else方法
(let ((opt1 (option 42))
      (opt2 (option 0))
      (opt3 (none)))
  (check ((opt1 :or-else opt2) :get) => 42)
  (check ((opt3 :or-else opt1) :get) => 42)
  (check ((opt3 :or-else opt2) :get) => 0))

;;; 测试基本操作
(let ((opt1 (option 42))
      (opt2 (none)))
  (check (opt1 :get) => 42))

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