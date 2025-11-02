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

(import (liii oop) (liii check) (liii error) (liii base) (liii case) (liii rich-string))
(check-set-mode! 'report-failed)

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

#|
typed-define
定义一个带有类型检查和默认值的函数。

语法
----
(typed-define (name (param1 type-pred1 default1) (param2 type-pred2 default2) ...)
  body-expr
  ...)

参数
----
name : symbol
要定义的函数名称。

param : (symbol predicate [default])
参数定义，包含：
- 参数名称 (symbol)
- 类型谓词 (procedure)，用于参数类型检查
- 可选默认值 (any)，当参数未提供时使用

body-expr : any
函数体表达式，可以包含多个表达式。

返回值
----
procedure
返回一个函数，该函数接受关键字参数，支持类型检查和默认值。

描述
----
typed-define 是 (liii oop) 模块中用于定义类型安全函数的宏。它允许为函数的每个参数
指定类型谓词和默认值，在函数调用时会自动进行类型检查，确保参数类型正确。

该宏生成的函数使用关键字参数调用方式，参数顺序可以任意排列。每个参数都会在运行时
进行类型检查，如果类型不匹配会抛出 'type-error 异常。

特点
----
- 支持运行时类型检查
- 支持参数默认值
- 使用关键字参数调用方式
- 参数顺序可以任意排列
- 提供清晰的错误信息

注意事项
----
- 类型谓词必须是返回布尔值的函数
- 默认值必须符合类型谓词的要求
- 函数调用时必须使用关键字参数语法
- 所有参数都会进行类型检查，包括默认值
- 类型错误会抛出 'type-error 异常
|#

(typed-define (person (name string? "Bob") (age integer?))
  (string-append name " is " (number->string age) " years old"))

(check (person :age 21) => "Bob is 21 years old")
(check (person :name "Alice" :age 25) => "Alice is 25 years old")
(check-catch 'type-error (person :name 123 :age 25))

;; 测试带有默认值的 typed-define
(typed-define (greet (message string? "Hello") (times integer? 1))
  (apply string-append (make-list times message)))

(check (greet) => "Hello")
(check (greet :message "Hi" :times 3) => "HiHiHi")
(check-catch 'type-error (greet :times "not-a-number"))


#|
define-case-class
定义类似 Scala 的 case class，提供类型安全的样本类。

语法
----
(define-case-class class-name fields . private-fields-and-methods)

参数
----
class-name : symbol
要定义的 case class 名称。

fields : list
字段定义列表，每个字段格式为 (field-name type-predicate [default-value])。

private-fields-and-methods : any
可选的私有字段和方法定义。

返回值
----
procedure
返回一个函数，该函数可以用于创建 case class 实例或调用静态方法。

描述
----
`define-case-class` 是 (liii oop) 模块中用于定义样本类的核心宏。
它创建类型安全的 case class，支持字段验证、方法分发和不可变数据结构。

字段定义中每个字段由三部分组成：
- field-name: 字段名称（符号）
- type-predicate: 类型断言函数，用于验证字段值的类型
- default-value: 可选，字段的默认值

方法类型包括：
- 静态方法: 以 `@` 开头的函数定义，通过类名调用
- 实例方法: 以 `%` 开头的函数定义，通过实例调用
- 内部方法: 普通函数定义，仅在类内部可见

私有字段使用 `define` 定义，仅在类内部可见。

特点
----
- 类型安全: 创建实例时会自动验证字段类型
- 不可变性: 字段默认不可变，通过关键字参数创建新实例
- 模式匹配: 支持通过字段名访问字段值
- 方法分发: 支持静态方法和实例方法
- 相等性比较: 自动实现 `:equals` 方法
- 字符串表示: 自动实现 `:to-string` 方法

注意事项
----
- 方法名不能与字段名冲突
- 字段类型验证在运行时进行
- 实例方法通过 `%` 前缀定义
- 静态方法通过 `@` 前缀定义
- 私有字段仅在类内部可见
|#

(define-case-class person
  ((name string? "Bob")
   (age integer?)))

(let1 bob (person :name "Bob" :age 21)
  (check (bob 'name) => "Bob")
  (check (bob 'age) => 21)
  (check ((bob :name "hello") 'name) => "hello")
  (check-catch 'value-error (bob 'sex))
  (check-catch 'value-error (bob :sex))
  (check-true (bob :is-instance-of 'person))
  (check-true (person :is-type-of bob))
  (check (bob :to-string) => "(person :name \"Bob\" :age 21)"))

(check-catch 'type-error (person 1 21))

(let ((bob (person "Bob" 21))
      (get-name (lambda (x)
                 (case* x
                   ((#<procedure?>) (x 'name))
                   (else (value-error))))))
  (check (get-name bob) => "Bob")
  (check-catch 'value-error (get-name 1)))

(define-case-class jerson
  ((name string?)
   (age integer?))
  
  (define (%to-string)
    (string-append "I am " name " " (number->string age) " years old!"))
  (define (%greet x)
    (string-append "Hi " x ", " (%to-string)))
  )

(let1 bob (jerson "Bob" 21)
  (check (bob :to-string) => "I am Bob 21 years old!")
  (check (bob :greet "Alice") => "Hi Alice, I am Bob 21 years old!"))

(define-case-class anonymous ()
  (define name "")

  (define (%get-name) name)

  (define (%set-name! x)
    (set! name x))
  )

(let1 p (anonymous)
  (p :set-name! "Alice")
  (check (p :get-name) => "Alice"))

(define-case-class my-bool ()
  (define data #t)

  (define (%set-true!)
    (set! data #t))
  (define (%set-false!)
    (set! data #f))
 
  (define (%true?) data)
  (define (%false?) (not (%true?)))
  
  (define (@apply x)
    (let1 r (my-bool)
      (cond ((eq? x 'true)
             (r :set-true!))
            ((eq? x 'false)
             (r :set-false!))
            ((boolean? x)
             (if x (r :set-true!) (r :set-false!)))
            (else (r :set-false!)))
      r))
  )

(check-true ((my-bool 'true) :true?))
(check-true ((my-bool 'false) :false?))
(check-true ((my-bool #t) :true?))
(check-true ((my-bool #f) :false?))
(check-true (my-bool :is-type-of (my-bool 'true)))

(define-case-class test-case-class
  ((name string?))
  
  (define (@this-is-a-static-method)
    (test-case-class "static"))
  
  (define (%this-is-a-instance-method)
    (test-case-class (string-append name "instance")))
  )

(let1 hello (test-case-class "hello ")
  (check-catch 'value-error (hello :this-is-a-static-method))
  (check (test-case-class :this-is-a-static-method) => (test-case-class "static")))

(let ()
  (define-case-class person ((name string?) (country string?))
    (define (@default)
      (person "Andy" "China"))
    (define (%set-country! c . xs)
      (set! country c)
      (apply (%this) (if (null? xs) '(:this) xs)))
    (define (%set-name! n . xs)
      (set! name n)
      (apply (%this) (if (null? xs) '(:this) xs)))
    (define (%to-string)
      (format #f "Hello ~a from ~a" name country)))

  (define Andy (person :default))
  (check-catch 'wrong-type-arg (person :this))
  (check (Andy :to-string) => "Hello Andy from China")
  (check (Andy :set-country! "USA" :to-string) => "Hello Andy from USA")
  (check (Andy :to-string) => "Hello Andy from USA")
  (check (Andy :set-country! "China" :set-name! "Ancker-0" :to-string) => "Hello Ancker-0 from China")
  (check (Andy :set-country! "China") => (person "Ancker-0" "China"))
  (check (Andy :this :set-country! "USA" :this :set-name! "Andy" :this :to-string) => "Hello Andy from USA")
  (check-true (person :is-type-of Andy)))

(let ()
  (define-case-class person ((name string?) (country string?))
    (chained-define (@default)
      (person "Andy" "China"))
    (chained-define (set-country! c)
      (set! country c)
      (%this))
    (chained-define (set-name! n)
      (set! name n)
      (%this))
    (chained-define (%set-both! n c)
      (set-country! c)
      (set-name! n)
      (%this))
    (chained-define (%to-string)
      (rich-string (format #f "Hello ~a from ~a" name country))))
  (check (person :default :to-string :get) => "Hello Andy from China")
  (check (person :default :set-both! "Bob" "Russia" :to-string :get) => "Hello Bob from Russia")
  (check-catch 'value-error (person :default :set-country! "French")))

(check-catch 'syntax-error
  (eval
    '(define-case-class instance-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (%name)
        name))))

(check-catch 'syntax-error
  (eval
    '(define-case-class static-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (@name)
        name))))

(check-catch 'syntax-error
  (eval
    '(define-case-class internal-methods-conflict-test
       ((name string?)
        (test-name string?)
        (age integer?))
       (define (test-name str)
         (string-append str " ")))))

(check-report)
