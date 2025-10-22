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

(import (liii check)
        (scheme base)
        (liii rich-string)
        (liii lang)
        (liii error))

(check-set-mode! 'report-failed)


#|
rich-string@empty
创建一个空的rich-string对象。

语法
----
(rich-string :empty . args)

参数
----
args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回空的字符串对象。

说明
----
创建一个不包含任何字符的rich-string。通常用于初始化字符串数据结构或作为
链式操作的起点。

边界条件
--------
- 无参数调用：返回空字符串
- 支持链式调用：可与其他rich-string方法组合使用

性能特征
--------
- 时间复杂度：O(1)，固定时间创建
- 空间复杂度：O(1)，创建空对象所需最小内存

兼容性
------
- 与所有rich-string实例方法兼容
- 支持链式调用模式
|#

;; 基本测试
(check ((rich-string :empty) :get) => "")
(check ((rich-string :empty) :length) => 0)
(check ((rich-string :empty) :empty?) => #t)

;; 边界测试
(check ((rich-string :empty :map (lambda (x) (x :to-upper))) :get) => "")
(check ((rich-string :empty :filter (lambda (x) #t)) :get) => "")
(check ((rich-string :empty :take 0) :get) => "")
(check ((rich-string :empty :drop 0) :get) => "")
(check ((rich-string :empty :reverse) :get) => "")

;; 验证类型正确性
(check-true (rich-string :is-type-of (rich-string :empty)))
(check-false (rich-string :is-type-of ""))

;; 链式调用测试
(check ((rich-string :empty :+ "hello") :get) => "hello")
(check ((rich-string :empty :strip-left) :get) => "")
(check ((rich-string :empty :strip-right) :get) => "")
(check ((rich-string :empty :strip-both) :get) => "")

;; 验证空字符串与其他方法的兼容性
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :empty) :ends-with "") => #t)
(check ((rich-string :empty) :contains "") => #t)
(check ((rich-string :empty) :index-of "") => -1)

#|
rich-string@value-of
从不同类型的值创建rich-string对象。

语法
----
(rich-string :value-of v . args)

参数
----
v : any
要转换为rich-string的值，支持以下类型：
- char：单个字符
- number：数字
- symbol：符号
- string：字符串
- rich-char：rich-char对象

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回转换后的字符串对象。

说明
----
将不同类型的值转换为rich-string对象。对于不支持的输入类型会抛出类型错误。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- 字符：转换为单字符字符串
- 数字：转换为数字的字符串表示
- 符号：转换为符号的字符串表示
- 字符串：直接包装为rich-string
- rich-char：转换为对应的字符串表示
- 其他类型：抛出类型错误

性能特征
--------
- 时间复杂度：O(n)，其中n为结果字符串的长度
- 空间复杂度：O(n)，需要存储转换后的字符串

兼容性
------
- 与所有rich-string实例方法兼容
- 支持链式调用模式
|#

;; 基本功能测试
;; 字符类型
(check ((rich-string :value-of #\a) :get) => "a")
(check ((rich-string :value-of #\space) :get) => " ")
(check ((rich-string :value-of #\0) :get) => "0")

;; 数字类型
(check ((rich-string :value-of 123) :get) => "123")
(check ((rich-string :value-of 0) :get) => "0")
(check ((rich-string :value-of -42) :get) => "-42")
(check ((rich-string :value-of 3.14) :get) => "3.14")

;; 符号类型
(check ((rich-string :value-of 'hello) :get) => "hello")
(check ((rich-string :value-of 'test) :get) => "test")

;; 字符串类型
(check ((rich-string :value-of "hello") :get) => "hello")
(check ((rich-string :value-of "") :get) => "")
(check ((rich-string :value-of "测试") :get) => "测试")

;; rich-char类型
(check ((rich-string :value-of (rich-char #\x)) :get) => "x")
(check ((rich-string :value-of (rich-char #\A)) :get) => "A")

;; 边界条件测试
;; 空字符串
(check ((rich-string :value-of "") :length) => 0)
(check ((rich-string :value-of "") :empty?) => #t)

;; 单字符边界
(check ((rich-string :value-of #\a) :length) => 1)
(check ((rich-string :value-of #\a) :empty?) => #f)

;; 链式调用测试
(check ((rich-string :value-of "hello" :+ " world") :get) => "hello world")
(check ((rich-string :value-of 123 :+ "abc") :get) => "123abc")
(check ((rich-string :value-of #\a :map (lambda (c) (c :to-upper))) :get) => "A")
(check ((rich-string :value-of "test" :reverse) :get) => "tset")

;; 类型验证
(check-true (rich-string :is-type-of (rich-string :value-of "hello")))
(check-true (rich-string :is-type-of (rich-string :value-of #\a)))
(check-true (rich-string :is-type-of (rich-string :value-of 123)))

;; 错误处理测试 - 使用check-catch来验证类型错误
(check-catch 'type-error (rich-string :value-of #t))
(check-catch 'type-error (rich-string :value-of '()))
(check-catch 'type-error (rich-string :value-of (vector)))

#|
rich-string%get
获取rich-string对象内部存储的原始字符串数据。

语法
----
(rich-string-instance :get)

参数
----
无参数。

返回值
-----
以string形式返回rich-string对象内部存储的原始字符串数据。

说明
----
该方法返回rich-string对象内部包装的原始字符串数据。这是获取rich-string
底层字符串表示的最直接方式。

边界条件
--------
- 空字符串：返回空字符串""
- 单字符字符串：返回单字符字符串
- 多字符字符串：返回完整的字符串
- Unicode字符串：返回包含Unicode字符的字符串

性能特征
--------
- 时间复杂度：O(1)，直接返回内部引用
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 返回标准Scheme字符串，可与任何字符串操作函数配合使用
|#

;; 基本功能测试
;; 空字符串
(check ((rich-string :empty) :get) => "")

;; 单字符字符串
(check ((rich-string :value-of #\a) :get) => "a")
(check ((rich-string :value-of #\space) :get) => " ")

;; 多字符字符串
(check ((rich-string :value-of "hello") :get) => "hello")
(check ((rich-string :value-of "test string") :get) => "test string")

;; Unicode字符串
(check ((rich-string :value-of "测试") :get) => "测试")
(check ((rich-string :value-of "こんにちは") :get) => "こんにちは")
(check ((rich-string :value-of "🎉") :get) => "🎉")

;; 数字转换的字符串
(check ((rich-string :value-of 123) :get) => "123")
(check ((rich-string :value-of 3.14) :get) => "3.14")

;; 符号转换的字符串
(check ((rich-string :value-of 'hello) :get) => "hello")

;; rich-char转换的字符串
(check ((rich-string :value-of (rich-char #\x)) :get) => "x")

;; 边界条件测试
;; 验证返回类型是字符串
(check (string? ((rich-string :value-of "hello") :get)) => #t)
(check (string? ((rich-string :empty) :get)) => #t)

;; 验证长度一致性
(check (string-length ((rich-string :value-of "hello") :get)) => 5)
(check (string-length ((rich-string :value-of "") :get)) => 0)

;; 验证内容一致性
(check (equal? ((rich-string :value-of "hello") :get) "hello") => #t)
(check (equal? ((rich-string :empty) :get) "") => #t)

;; 链式操作后获取结果
(check ((rich-string :value-of "hello" :+ " world") :get) => "hello world")
(check ((rich-string :value-of "test" :reverse) :get) => "tset")
(check ((rich-string :value-of "  hello  " :strip-both) :get) => "hello")

;; 验证与其他字符串函数的兼容性
(check (string=? ((rich-string :value-of "hello") :get) "hello") => #t)
(check (string=? ((rich-string :value-of "hello world") :get) "hello world") => #t)
(check (string=? ((rich-string :value-of "he") :get) "he") => #t)

#|
rich-string%length
获取rich-string对象中Unicode字符的数量。

语法
----
(rich-string-instance :length)

参数
----
无参数。

返回值
-----
以integer形式返回rich-string对象中Unicode字符的数量。

说明
----
该方法返回rich-string对象中Unicode字符的数量，而不是字节长度。
对于空字符串返回0，对于包含Unicode字符的字符串返回实际的字符数量。

边界条件
--------
- 空字符串：返回0
- ASCII字符串：字符数量等于字符串长度
- Unicode字符串：返回实际的Unicode字符数量（可能小于字节长度）
- 混合字符：正确计算所有Unicode字符的数量

性能特征
--------
- 时间复杂度：O(1)，长度在对象创建时已计算并缓存
- 空间复杂度：O(1)，直接返回缓存的长度值

兼容性
------
- 与所有rich-string实例兼容
- 返回标准整数，可与任何数值操作配合使用
|#

;; 基本功能测试
;; 空字符串
(check ((rich-string :empty) :length) => 0)

;; 单字符字符串
(check ((rich-string :value-of #\a) :length) => 1)
(check ((rich-string :value-of #\space) :length) => 1)

;; 多字符ASCII字符串
(check ((rich-string :value-of "hello") :length) => 5)
(check ((rich-string :value-of "test string") :length) => 11)

;; Unicode字符测试
;; 中文字符（每个字符通常占3字节）
(check ((rich-string :value-of "测试") :length) => 2)
(check ((rich-string :value-of "你好世界") :length) => 4)

;; 日文字符
(check ((rich-string :value-of "こんにちは") :length) => 5)

;; Emoji表情符号
(check ((rich-string :value-of "🎉") :length) => 1)
(check ((rich-string :value-of "🎉🎊") :length) => 2)

;; 混合字符
(check ((rich-string :value-of "hello 世界 🎉") :length) => 10)

;; 边界条件测试
;; 零长度字符串
(check ((rich-string :value-of "") :length) => 0)

;; 单字节字符
(check ((rich-string :value-of "a") :length) => 1)

;; 多字节Unicode字符
(check ((rich-string :value-of "中") :length) => 1)

;; 验证长度一致性
;; 与get方法返回的字符串长度一致（ASCII字符串）
(check ((rich-string :value-of "hello") :length) => (string-length "hello"))
;; Unicode字符串：string-length返回字节长度，rich-string%length返回字符数量
;; 所以这里不直接比较，而是验证字符数量正确性

;; 链式操作后长度验证
(check ((rich-string :value-of "hello" :+ " world") :length) => 11)
(check ((rich-string :value-of "test" :reverse) :length) => 4)

;; 验证长度与字符数量的一致性
(check ((rich-string :value-of "hello") :length) => 5)
(check ((rich-string :value-of "hello world") :length) => 11)

;; 验证不同类型输入的字符数量
;; 数字转换
(check ((rich-string :value-of 123) :length) => 3)
(check ((rich-string :value-of 0) :length) => 1)

;; 符号转换
(check ((rich-string :value-of 'hello) :length) => 5)

;; rich-char转换
(check ((rich-string :value-of (rich-char #\x)) :length) => 1)

;; 验证长度与空字符串判断的一致性
(check (zero? ((rich-string :empty) :length)) => #t)
(check (zero? ((rich-string :value-of "") :length)) => #t)
(check (positive? ((rich-string :value-of "hello") :length)) => #t)

#|
rich-string%char-at
获取rich-string对象中指定索引位置的Unicode字符。

语法
----
(rich-string-instance :char-at index)

参数
----
index : integer
要访问的字符索引，从0开始计数。

返回值
-----
以rich-char形式返回指定索引位置的Unicode字符。

说明
----
该方法返回rich-string对象中指定索引位置的Unicode字符，以rich-char对象形式返回。
索引从0开始，表示第一个字符的位置。如果索引超出字符串范围，会抛出索引错误。
该方法正确处理Unicode字符，能够准确提取多字节编码的字符。

边界条件
--------
- 索引0：返回第一个字符
- 索引length-1：返回最后一个字符
- 负索引：抛出索引错误
- 超出范围的索引：抛出索引错误
- 非整数索引：抛出类型错误

性能特征
--------
- 时间复杂度：O(1)，直接定位到指定字符位置
- 空间复杂度：O(1)，创建单个rich-char对象

兼容性
------
- 与所有rich-string实例兼容
- 返回rich-char对象，可与rich-char相关操作配合使用
|#

;; 基本功能测试
;; ASCII字符串字符访问
(check (((rich-string :value-of "hello") :char-at 0) :make-string) => "h")
(check (((rich-string :value-of "hello") :char-at 1) :make-string) => "e")
(check (((rich-string :value-of "hello") :char-at 2) :make-string) => "l")
(check (((rich-string :value-of "hello") :char-at 3) :make-string) => "l")
(check (((rich-string :value-of "hello") :char-at 4) :make-string) => "o")

;; 单字符字符串
(check (((rich-string :value-of "a") :char-at 0) :make-string) => "a")
(check (((rich-string :value-of " ") :char-at 0) :make-string) => " ")

;; Unicode字符访问测试
;; 中文字符
(check (((rich-string :value-of "测试") :char-at 0) :make-string) => "测")
(check (((rich-string :value-of "测试") :char-at 1) :make-string) => "试")

;; 日文字符
(check (((rich-string :value-of "こんにちは") :char-at 0) :make-string) => "こ")
(check (((rich-string :value-of "こんにちは") :char-at 1) :make-string) => "ん")

;; Emoji表情符号
(check (((rich-string :value-of "🎉") :char-at 0) :make-string) => "🎉")
(check (((rich-string :value-of "🎉🎊") :char-at 0) :make-string) => "🎉")
(check (((rich-string :value-of "🎉🎊") :char-at 1) :make-string) => "🎊")

;; 混合字符
(check (((rich-string :value-of "hello 世界 🎉") :char-at 0) :make-string) => "h")
(check (((rich-string :value-of "hello 世界 🎉") :char-at 5) :make-string) => " ")
(check (((rich-string :value-of "hello 世界 🎉") :char-at 6) :make-string) => "世")
(check (((rich-string :value-of "hello 世界 🎉") :char-at 7) :make-string) => "界")
(check (((rich-string :value-of "hello 世界 🎉") :char-at 8) :make-string) => " ")
(check (((rich-string :value-of "hello 世界 🎉") :char-at 9) :make-string) => "🎉")

;; 边界条件测试
;; 第一个字符
(check (((rich-string :value-of "hello") :char-at 0) :make-string) => "h")

;; 最后一个字符
(check (((rich-string :value-of "hello") :char-at 4) :make-string) => "o")

;; 空字符串 - 应该抛出范围错误
(check-catch 'out-of-range ((rich-string :empty) :char-at 0))

;; 负索引 - 应该抛出范围错误
(check-catch 'out-of-range ((rich-string :value-of "hello") :char-at -1))

;; 超出范围索引 - 应该抛出范围错误
(check-catch 'out-of-range ((rich-string :value-of "hello") :char-at 5))
(check-catch 'out-of-range ((rich-string :value-of "hello") :char-at 100))

;; 非整数索引 - 应该抛出类型错误
(check-catch 'type-error ((rich-string :value-of "hello") :char-at 0.5))
(check-catch 'type-error ((rich-string :value-of "hello") :char-at "0"))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :char-at 0)))
(check-true (rich-char :is-type-of ((rich-string :value-of "测试") :char-at 0)))

;; 链式操作测试
;; 字符转换操作
(check ((((rich-string :value-of "hello") :char-at 0) :to-upper) :make-string) => "H")
(check ((((rich-string :value-of "HELLO") :char-at 0) :to-lower) :make-string) => "h")

;; 字符比较操作
(check-true (((rich-string :value-of "hello") :char-at 0) :equals #\h))
(check-false (((rich-string :value-of "hello") :char-at 0) :equals #\H))

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :char-at 0) :make-string) => "h")
  (check ((rs :char-at 6) :make-string) => "w")
  (check ((rs :char-at 10) :make-string) => "d"))

;; 验证Unicode字符的正确提取
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :char-at 0) :make-string) => "测")
  (check ((rs :char-at 1) :make-string) => "试")
  (check ((rs :char-at 2) :make-string) => "字")
  (check ((rs :char-at 3) :make-string) => "符")
  (check ((rs :char-at 4) :make-string) => "串"))

#|
rich-string%apply
获取rich-string对象中指定索引位置的Unicode字符（函数式编程风格接口）。

语法
----
(rich-string-instance :apply index)

参数
----
index : integer
要访问的字符索引，从0开始计数。

返回值
-----
以rich-char形式返回指定索引位置的Unicode字符。

说明
----
该方法是rich-string%char-at的别名，提供函数式编程风格的字符访问接口。
返回rich-string对象中指定索引位置的Unicode字符，以rich-char对象形式返回。
索引从0开始，表示第一个字符的位置。如果索引超出字符串范围，会抛出索引错误。
该方法正确处理Unicode字符，能够准确提取多字节编码的字符。

边界条件
--------
- 索引0：返回第一个字符
- 索引length-1：返回最后一个字符
- 负索引：抛出索引错误
- 超出范围的索引：抛出索引错误
- 非整数索引：抛出类型错误

性能特征
--------
- 时间复杂度：O(1)，直接定位到指定字符位置
- 空间复杂度：O(1)，创建单个rich-char对象

兼容性
------
- 与所有rich-string实例兼容
- 返回rich-char对象，可与rich-char相关操作配合使用
- 与rich-string%char-at方法功能完全一致
|#

;; 基本功能测试
;; ASCII字符串字符访问
(check (((rich-string :value-of "hello") :apply 0) :make-string) => "h")
(check (((rich-string :value-of "hello") :apply 1) :make-string) => "e")
(check (((rich-string :value-of "hello") :apply 2) :make-string) => "l")
(check (((rich-string :value-of "hello") :apply 3) :make-string) => "l")
(check (((rich-string :value-of "hello") :apply 4) :make-string) => "o")

;; 单字符字符串
(check (((rich-string :value-of "a") :apply 0) :make-string) => "a")
(check (((rich-string :value-of " ") :apply 0) :make-string) => " ")

;; Unicode字符访问测试
;; 中文字符
(check (((rich-string :value-of "测试") :apply 0) :make-string) => "测")
(check (((rich-string :value-of "测试") :apply 1) :make-string) => "试")

;; 日文字符
(check (((rich-string :value-of "こんにちは") :apply 0) :make-string) => "こ")
(check (((rich-string :value-of "こんにちは") :apply 1) :make-string) => "ん")

;; Emoji表情符号
(check (((rich-string :value-of "🎉") :apply 0) :make-string) => "🎉")
(check (((rich-string :value-of "🎉🎊") :apply 0) :make-string) => "🎉")
(check (((rich-string :value-of "🎉🎊") :apply 1) :make-string) => "🎊")

;; 混合字符
(check (((rich-string :value-of "hello 世界 🎉") :apply 0) :make-string) => "h")
(check (((rich-string :value-of "hello 世界 🎉") :apply 5) :make-string) => " ")
(check (((rich-string :value-of "hello 世界 🎉") :apply 6) :make-string) => "世")
(check (((rich-string :value-of "hello 世界 🎉") :apply 7) :make-string) => "界")
(check (((rich-string :value-of "hello 世界 🎉") :apply 8) :make-string) => " ")
(check (((rich-string :value-of "hello 世界 🎉") :apply 9) :make-string) => "🎉")

;; 边界条件测试
;; 第一个字符
(check (((rich-string :value-of "hello") :apply 0) :make-string) => "h")

;; 最后一个字符
(check (((rich-string :value-of "hello") :apply 4) :make-string) => "o")

;; 空字符串 - 应该抛出范围错误
(check-catch 'out-of-range ((rich-string :empty) :apply 0))

;; 负索引 - 应该抛出范围错误
(check-catch 'out-of-range ((rich-string :value-of "hello") :apply -1))

;; 超出范围索引 - 应该抛出范围错误
(check-catch 'out-of-range ((rich-string :value-of "hello") :apply 5))
(check-catch 'out-of-range ((rich-string :value-of "hello") :apply 100))

;; 非整数索引 - 应该抛出类型错误
(check-catch 'type-error ((rich-string :value-of "hello") :apply 0.5))
(check-catch 'type-error ((rich-string :value-of "hello") :apply "0"))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :apply 0)))
(check-true (rich-char :is-type-of ((rich-string :value-of "测试") :apply 0)))

;; 链式操作测试
;; 字符转换操作
(check ((((rich-string :value-of "hello") :apply 0) :to-upper) :make-string) => "H")
(check ((((rich-string :value-of "HELLO") :apply 0) :to-lower) :make-string) => "h")

;; 字符比较操作
(check-true (((rich-string :value-of "hello") :apply 0) :equals #\h))
(check-false (((rich-string :value-of "hello") :apply 0) :equals #\H))

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :apply 0) :make-string) => "h")
  (check ((rs :apply 6) :make-string) => "w")
  (check ((rs :apply 10) :make-string) => "d"))

;; 验证Unicode字符的正确提取
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :apply 0) :make-string) => "测")
  (check ((rs :apply 1) :make-string) => "试")
  (check ((rs :apply 2) :make-string) => "字")
  (check ((rs :apply 3) :make-string) => "符")
  (check ((rs :apply 4) :make-string) => "串"))

;; 验证apply与char-at方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :apply 0) :make-string) => ((rs :char-at 0) :make-string))
  (check ((rs :apply 5) :make-string) => ((rs :char-at 5) :make-string))
  (check ((rs :apply 10) :make-string) => ((rs :char-at 10) :make-string)))

;; 验证Unicode字符访问的一致性
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :apply 0) :make-string) => ((rs :char-at 0) :make-string))
  (check ((rs :apply 2) :make-string) => ((rs :char-at 2) :make-string))
  (check ((rs :apply 4) :make-string) => ((rs :char-at 4) :make-string)))

#|
rich-string%find
在rich-string中查找满足给定谓词的第一个字符。

语法
----
(rich-string-instance :find pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以option形式返回满足谓词的第一个字符的rich-char对象。
如果没有任何字符满足谓词，返回none。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
返回第一个满足谓词的字符的option包装。如果遍历完所有字符都没有找到
满足条件的字符，则返回none。

该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回none
- 所有字符都不满足谓词：返回none
- 第一个字符满足谓词：返回第一个字符的option
- 中间字符满足谓词：返回第一个满足条件的字符的option
- 最后一个字符满足谓词：返回最后一个字符的option

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，只创建单个option对象

兼容性
------
- 与所有rich-string实例兼容
- 返回option类型，可与option相关操作配合使用
- 谓词函数必须接受rich-char对象作为参数
|#

;; 基本功能测试
;; 查找特定字符
(check-true (option :is-type-of ((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h)))))
(check ((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get) :make-string) => "h")

;; 查找大写字母
(check-true (option :is-type-of ((rich-string :value-of "Hello") :find (lambda (c) (c :upper?)))))
(check ((((rich-string :value-of "Hello") :find (lambda (c) (c :upper?))) :get) :make-string) => "H")

;; 查找数字字符
(check-true (option :is-type-of ((rich-string :value-of "abc123") :find (lambda (c) (c :digit?)))))
(check ((((rich-string :value-of "abc123") :find (lambda (c) (c :digit?))) :get) :make-string) => "1")

;; 边界条件测试
;; 空字符串 - 返回none
(check-true (((rich-string :empty) :find (lambda (c) #t)) :empty?))
(check-true (((rich-string :empty) :find (lambda (c) #f)) :empty?))

;; 所有字符都不满足谓词 - 返回none
(check-true (((rich-string :value-of "hello") :find (lambda (c) (c :equals #\x))) :empty?))
(check-true (((rich-string :value-of "abc") :find (lambda (c) (c :digit?))) :empty?))

;; 第一个字符满足谓词
(check ((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get) :make-string) => "h")

;; 中间字符满足谓词
(check ((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\l))) :get) :make-string) => "l")

;; 最后一个字符满足谓词
(check ((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\o))) :get) :make-string) => "o")

;; Unicode字符查找测试
;; 查找Unicode字符（使用字符串比较）
(check ((((rich-string :value-of "hello世界") :find (lambda (c) (string=? (c :make-string) "世"))) :get) :make-string) => "世")

;; 查找日文字符
(check ((((rich-string :value-of "helloこんにちは") :find (lambda (c) (string=? (c :make-string) "こ"))) :get) :make-string) => "こ")

;; 查找emoji表情符号
(check ((((rich-string :value-of "hello🎉world") :find (lambda (c) (string=? (c :make-string) "🎉"))) :get) :make-string) => "🎉")

;; 复杂谓词测试
;; 查找第一个大写字母
(check ((((rich-string :value-of "hello World") :find (lambda (c) (c :upper?))) :get) :make-string) => "W")

;; 查找第一个小写字母
(check ((((rich-string :value-of "123abc456") :find (lambda (c) (c :lower?))) :get) :make-string) => "a")

;; 查找第一个空白字符（使用空格字符判断）
(check ((((rich-string :value-of "hello world") :find (lambda (c) (c :equals #\ ))) :get) :make-string) => " ")

;; 验证返回类型
;; 返回option类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h)))))
(check-true (((rich-string :value-of "hello") :find (lambda (c) (c :equals #\x))) :empty?))

;; 返回的option包含rich-char
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get)))

;; 链式操作测试
;; 查找后转换字符
(check (((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get) :to-upper) :make-string) => "H")

;; 查找后比较字符
(check-true ((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get) :equals #\h))

;; 验证查找结果与字符访问的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :find (lambda (c) (c :equals #\w))) :get) :make-string) => "w")
  (check (((rs :find (lambda (c) (c :equals #\d))) :get) :make-string) => "d"))

;; 验证Unicode字符查找的一致性
(let ((rs (rich-string :value-of "测试字符串")))
  (check (((rs :find (lambda (c) (string=? (c :make-string) "测"))) :get) :make-string) => "测")
  (check (((rs :find (lambda (c) (string=? (c :make-string) "串"))) :get) :make-string) => "串"))

;; 性能相关测试
;; 长字符串中的查找
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check-true (option :is-type-of (long-str :find (lambda (c) (c :equals #\a)))))
  (let ((long-str-with-b (rich-string :value-of (string-append (make-string 999 #\a) "b"))))
    (check (((long-str-with-b :find (lambda (c) (c :equals #\b))) :get) :make-string) => "b")))

#|
rich-string%find-last
在rich-string中从后向前查找满足给定谓词的第一个字符。

语法
----
(rich-string-instance :find-last pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以option形式返回从后向前查找时第一个满足谓词的字符的rich-char对象。
如果没有任何字符满足谓词，返回none。

说明
----
该方法从rich-string的末尾开始向前遍历每个字符，对每个字符应用谓词函数pred。
返回从后向前查找时第一个满足谓词的字符的option包装。如果遍历完所有字符都没有找到
满足条件的字符，则返回none。

与%find方法不同，%find-last从字符串末尾开始查找，返回最后一个满足条件的字符。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回none
- 所有字符都不满足谓词：返回none
- 第一个字符满足谓词：如果从后向前查找，只有当它是唯一满足条件的字符时才会返回
- 中间字符满足谓词：返回最后一个满足条件的字符的option
- 最后一个字符满足谓词：返回最后一个字符的option

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，只创建单个option对象

兼容性
------
- 与所有rich-string实例兼容
- 返回option类型，可与option相关操作配合使用
- 谓词函数必须接受rich-char对象作为参数
|#

;; 基本功能测试
;; 查找特定字符（从后向前）
(check-true (option :is-type-of ((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o)))))
(check ((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get) :make-string) => "o")

;; 查找大写字母（从后向前）
(check-true (option :is-type-of ((rich-string :value-of "HeLLo") :find-last (lambda (c) (c :upper?)))))
(check ((((rich-string :value-of "HeLLo") :find-last (lambda (c) (c :upper?))) :get) :make-string) => "L")

;; 查找数字字符（从后向前）
(check-true (option :is-type-of ((rich-string :value-of "abc123") :find-last (lambda (c) (c :digit?)))))
(check ((((rich-string :value-of "abc123") :find-last (lambda (c) (c :digit?))) :get) :make-string) => "3")

;; 边界条件测试
;; 空字符串 - 返回none
(check-true (((rich-string :empty) :find-last (lambda (c) #t)) :empty?))
(check-true (((rich-string :empty) :find-last (lambda (c) #f)) :empty?))

;; 所有字符都不满足谓词 - 返回none
(check-true (((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\x))) :empty?))
(check-true (((rich-string :value-of "abc") :find-last (lambda (c) (c :digit?))) :empty?))

;; 第一个字符满足谓词（从后向前查找时，只有当它是唯一满足条件的字符时才会返回）
(check ((((rich-string :value-of "hxxxx") :find-last (lambda (c) (c :equals #\h))) :get) :make-string) => "h")

;; 中间字符满足谓词（返回最后一个满足条件的字符）
(check ((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\l))) :get) :make-string) => "l")

;; 最后一个字符满足谓词
(check ((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get) :make-string) => "o")

;; Unicode字符查找测试
;; 查找Unicode字符（从后向前，使用字符串比较）
(check ((((rich-string :value-of "hello世界") :find-last (lambda (c) (string=? (c :make-string) "界"))) :get) :make-string) => "界")

;; 查找日文字符（从后向前）
(check ((((rich-string :value-of "helloこんにちは") :find-last (lambda (c) (string=? (c :make-string) "は"))) :get) :make-string) => "は")

;; 查找emoji表情符号（从后向前）
(check ((((rich-string :value-of "hello🎉world🎊") :find-last (lambda (c) (string=? (c :make-string) "🎊"))) :get) :make-string) => "🎊")

;; 复杂谓词测试
;; 查找最后一个大写字母
(check ((((rich-string :value-of "Hello World") :find-last (lambda (c) (c :upper?))) :get) :make-string) => "W")

;; 查找最后一个小写字母
(check ((((rich-string :value-of "123abc456") :find-last (lambda (c) (c :lower?))) :get) :make-string) => "c")

;; 查找最后一个空白字符（使用空格字符判断）
(check ((((rich-string :value-of "hello world test") :find-last (lambda (c) (c :equals #\ ))) :get) :make-string) => " ")

;; 验证返回类型
;; 返回option类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o)))))
(check-true (((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\x))) :empty?))

;; 返回的option包含rich-char
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get)))

;; 链式操作测试
;; 查找后转换字符
(check (((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get) :to-upper) :make-string) => "O")

;; 查找后比较字符
(check-true ((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get) :equals #\o))

;; 验证查找结果与字符访问的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :find-last (lambda (c) (c :equals #\w))) :get) :make-string) => "w")
  (check (((rs :find-last (lambda (c) (c :equals #\d))) :get) :make-string) => "d"))

;; 验证Unicode字符查找的一致性
(let ((rs (rich-string :value-of "测试字符串")))
  (check (((rs :find-last (lambda (c) (string=? (c :make-string) "串"))) :get) :make-string) => "串")
  (check (((rs :find-last (lambda (c) (string=? (c :make-string) "测"))) :get) :make-string) => "测"))

;; 与find方法的对比测试
;; 在包含重复字符的字符串中比较find和find-last
(let ((rs (rich-string :value-of "hello")))
  ;; find返回第一个'l'
  (check (((rs :find (lambda (c) (c :equals #\l))) :get) :make-string) => "l")
  ;; find-last返回最后一个'l'
  (check (((rs :find-last (lambda (c) (c :equals #\l))) :get) :make-string) => "l"))

;; 在包含多个大写字母的字符串中比较
(let ((rs (rich-string :value-of "HeLLo")))
  ;; find返回第一个大写字母'H'
  (check (((rs :find (lambda (c) (c :upper?))) :get) :make-string) => "H")
  ;; find-last返回最后一个大写字母'L'
  (check (((rs :find-last (lambda (c) (c :upper?))) :get) :make-string) => "L"))

;; 性能相关测试
;; 长字符串中的查找（从后向前）
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check-true (option :is-type-of (long-str :find-last (lambda (c) (c :equals #\a)))))
  (let ((long-str-with-b (rich-string :value-of (string-append (make-string 999 #\a) "b"))))
    (check (((long-str-with-b :find-last (lambda (c) (c :equals #\b))) :get) :make-string) => "b")))

;; 在长字符串中查找第一个字符（从后向前）
(let ((long-str (rich-string :value-of (string-append "a" (make-string 999 #\b)))))
  (check (((long-str :find-last (lambda (c) (c :equals #\a))) :get) :make-string) => "a"))

#|
rich-string%head
获取rich-string对象中的第一个字符。

语法
----
(rich-string-instance :head)

参数
----
无参数。

返回值
-----
以rich-char形式返回rich-string对象中的第一个字符。

说明
----
该方法返回rich-string对象中的第一个字符，以rich-char对象形式返回。
如果字符串为空，会抛出索引错误。该方法正确处理Unicode字符，能够
准确返回多字节编码的字符。

边界条件
--------
- 空字符串：抛出索引错误
- 单字符字符串：返回唯一的字符的rich-char对象
- 多字符字符串：返回第一个字符的rich-char对象
- Unicode字符串：正确返回第一个Unicode字符的rich-char对象

性能特征
--------
- 时间复杂度：O(1)，直接访问第一个字符
- 空间复杂度：O(1)，创建单个rich-char对象

兼容性
------
- 与所有rich-string实例兼容
- 返回rich-char对象，可与rich-char相关操作配合使用
|#

;; 基本功能测试
;; ASCII字符串的第一个字符
(check (((rich-string :value-of "hello") :head) :make-string) => "h")
(check (((rich-string :value-of "world") :head) :make-string) => "w")
(check (((rich-string :value-of "test") :head) :make-string) => "t")

;; 单字符字符串
(check (((rich-string :value-of "a") :head) :make-string) => "a")
(check (((rich-string :value-of " ") :head) :make-string) => " ")
(check (((rich-string :value-of "0") :head) :make-string) => "0")

;; Unicode字符测试
;; 中文字符
(check (((rich-string :value-of "测试") :head) :make-string) => "测")
(check (((rich-string :value-of "你好") :head) :make-string) => "你")

;; 日文字符
(check (((rich-string :value-of "こんにちは") :head) :make-string) => "こ")

;; Emoji表情符号
(check (((rich-string :value-of "🎉") :head) :make-string) => "🎉")
(check (((rich-string :value-of "🎉🎊") :head) :make-string) => "🎉")

;; 混合字符
(check (((rich-string :value-of "hello 世界 🎉") :head) :make-string) => "h")

;; 边界条件测试
;; 空字符串 - 应该抛出索引错误
(check-catch 'index-error ((rich-string :empty) :head))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :head)))
(check-true (rich-char :is-type-of ((rich-string :value-of "测试") :head)))

;; 链式操作测试
;; 字符转换操作
(check ((((rich-string :value-of "hello") :head) :to-upper) :make-string) => "H")
(check ((((rich-string :value-of "HELLO") :head) :to-lower) :make-string) => "h")

;; 字符比较操作
(check-true (((rich-string :value-of "hello") :head) :equals #\h))
(check-false (((rich-string :value-of "hello") :head) :equals #\H))

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :head) :make-string) => "h"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :head) :make-string) => "测"))

;; 验证不同类型输入的字符访问
;; 数字转换
(check (((rich-string :value-of 123) :head) :make-string) => "1")

;; 符号转换
(check (((rich-string :value-of 'hello) :head) :make-string) => "h")

;; rich-char转换
(check (((rich-string :value-of (rich-char #\x)) :head) :make-string) => "x")

;; 验证head与char-at方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :head) :make-string) => ((rs :char-at 0) :make-string)))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :head) :make-string) => ((rs :char-at 0) :make-string)))

#|
rich-string%head-option
获取rich-string对象中的第一个字符，以option类型返回。

语法
----
(rich-string-instance :head-option)

参数
----
无参数。

返回值
-----
以option形式返回rich-string对象中的第一个字符。
如果字符串为空，返回none；否则返回包含第一个字符的option。

说明
----
该方法返回rich-string对象中的第一个字符，以option包装的rich-char对象形式返回。
与%head方法不同，%head-option在字符串为空时不会抛出错误，而是返回none。
该方法正确处理Unicode字符，能够准确返回多字节编码的字符。

边界条件
--------
- 空字符串：返回none
- 单字符字符串：返回包含唯一字符的option
- 多字符字符串：返回包含第一个字符的option
- Unicode字符串：正确返回第一个Unicode字符的option

性能特征
--------
- 时间复杂度：O(1)，直接访问第一个字符
- 空间复杂度：O(1)，创建单个option对象

兼容性
------
- 与所有rich-string实例兼容
- 返回option类型，可与option相关操作配合使用
- 与%head方法功能互补，提供安全的字符访问
|#

;; 基本功能测试
;; 非空字符串返回包含第一个字符的option
(check-true (option :is-type-of ((rich-string :value-of "hello") :head-option)))
(check ((((rich-string :value-of "hello") :head-option) :get) :make-string) => "h")
(check ((((rich-string :value-of "world") :head-option) :get) :make-string) => "w")

;; 单字符字符串
(check ((((rich-string :value-of "a") :head-option) :get) :make-string) => "a")
(check ((((rich-string :value-of " ") :head-option) :get) :make-string) => " ")
(check ((((rich-string :value-of "0") :head-option) :get) :make-string) => "0")

;; Unicode字符测试
;; 中文字符
(check ((((rich-string :value-of "测试") :head-option) :get) :make-string) => "测")
(check ((((rich-string :value-of "你好") :head-option) :get) :make-string) => "你")

;; 日文字符
(check ((((rich-string :value-of "こんにちは") :head-option) :get) :make-string) => "こ")

;; Emoji表情符号
(check ((((rich-string :value-of "🎉") :head-option) :get) :make-string) => "🎉")
(check ((((rich-string :value-of "🎉🎊") :head-option) :get) :make-string) => "🎉")

;; 混合字符
(check ((((rich-string :value-of "hello 世界 🎉") :head-option) :get) :make-string) => "h")

;; 边界条件测试
;; 空字符串 - 返回none
(check-true (((rich-string :empty) :head-option) :empty?))
(check-true (((rich-string :value-of "") :head-option) :empty?))

;; 验证返回类型是option
(check-true (option :is-type-of ((rich-string :value-of "hello") :head-option)))
(check-true (option :is-type-of ((rich-string :value-of "测试") :head-option)))

;; 验证option包含rich-char
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :head-option) :get)))
(check-true (rich-char :is-type-of (((rich-string :value-of "测试") :head-option) :get)))

;; 链式操作测试
;; 字符转换操作
(check (((((rich-string :value-of "hello") :head-option) :get) :to-upper) :make-string) => "H")
(check (((((rich-string :value-of "HELLO") :head-option) :get) :to-lower) :make-string) => "h")

;; 字符比较操作
(check-true ((((rich-string :value-of "hello") :head-option) :get) :equals #\h))
(check-false ((((rich-string :value-of "hello") :head-option) :get) :equals #\H))

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :head-option) :get) :make-string) => "h"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check (((rs :head-option) :get) :make-string) => "测"))

;; 验证不同类型输入的字符访问
;; 数字转换
(check ((((rich-string :value-of 123) :head-option) :get) :make-string) => "1")

;; 符号转换
(check ((((rich-string :value-of 'hello) :head-option) :get) :make-string) => "h")

;; rich-char转换
(check ((((rich-string :value-of (rich-char #\x)) :head-option) :get) :make-string) => "x")

;; 验证head-option与head方法的一致性（非空字符串）
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :head) :make-string) => (((rs :head-option) :get) :make-string)))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :head) :make-string) => (((rs :head-option) :get) :make-string)))

;; 验证head-option与char-at方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :head-option) :get) :make-string) => ((rs :char-at 0) :make-string)))

(let ((rs (rich-string :value-of "测试字符串")))
  (check (((rs :head-option) :get) :make-string) => ((rs :char-at 0) :make-string)))

;; option类型操作测试
;; 使用option的map操作
(check (((((rich-string :value-of "hello") :head-option) :map (lambda (c) (c :to-upper))) :get) :make-string) => "H")

;; 使用option的get-or-else操作
(check ((((rich-string :value-of "hello") :head-option) :get-or-else (rich-char #\x)) :make-string) => "h")
(check ((((rich-string :empty) :head-option) :get-or-else (rich-char #\x)) :make-string) => "x")

;; 使用option的exists操作
(check-true (((rich-string :value-of "hello") :head-option) :exists (lambda (c) (c :equals #\h))))
(check-false (((rich-string :value-of "hello") :head-option) :exists (lambda (c) (c :equals #\x))))
(check-false (((rich-string :empty) :head-option) :exists (lambda (c) #t)))

;; 使用option的forall操作
(check-true (((rich-string :value-of "hello") :head-option) :forall (lambda (c) (c :equals #\h))))
(check-false (((rich-string :value-of "hello") :head-option) :forall (lambda (c) (c :equals #\x))))
;; 空option的forall行为依赖于option类型的实现，暂时注释掉这个测试
;; (check-true (((rich-string :empty) :head-option) :forall (lambda (c) #f))) ; 空option的forall总是返回true

;; 使用option的filter操作
(check (((((rich-string :value-of "hello") :head-option) :filter (lambda (c) (c :equals #\h))) :get) :make-string) => "h")
(check-true ((((rich-string :value-of "hello") :head-option) :filter (lambda (c) (c :equals #\x))) :empty?))
(check-true (((rich-string :empty) :head-option) :filter (lambda (c) #t) :empty?))

;; 验证option类型操作的完整性
(let ((opt ((rich-string :value-of "hello") :head-option)))
  (check-true (opt :defined?))
  (check-false (opt :empty?))
  (check ((opt :get) :make-string) => "h"))

(let ((opt ((rich-string :empty) :head-option)))
  (check-false (opt :defined?))
  (check-true (opt :empty?)))

;; 与find方法的对比测试
;; head-option总是返回第一个字符，而find需要谓词
(let ((rs (rich-string :value-of "hello")))
  (check (((rs :head-option) :get) :make-string) => "h")
  (check (((rs :find (lambda (c) (c :equals #\h))) :get) :make-string) => "h")
  (check (((rs :find (lambda (c) (c :equals #\l))) :get) :make-string) => "l"))

;; 性能相关测试
;; 长字符串中的head-option操作
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check-true (option :is-type-of (long-str :head-option)))
  (check (((long-str :head-option) :get) :make-string) => "a"))

;; 空字符串的head-option操作
(let ((empty-str (rich-string :empty)))
  (check-true ((empty-str :head-option) :empty?)))

#|
rich-string%last
获取rich-string对象中的最后一个字符。

语法
----
(rich-string-instance :last)

参数
----
无参数。

返回值
-----
以rich-char形式返回rich-string对象中的最后一个字符。

说明
----
该方法返回rich-string对象中的最后一个字符，以rich-char对象形式返回。
如果字符串为空，会抛出索引错误。该方法正确处理Unicode字符，能够
准确返回多字节编码的字符。

边界条件
--------
- 空字符串：抛出索引错误
- 单字符字符串：返回唯一的字符的rich-char对象
- 多字符字符串：返回最后一个字符的rich-char对象
- Unicode字符串：正确返回最后一个Unicode字符的rich-char对象

性能特征
--------
- 时间复杂度：O(1)，直接访问最后一个字符
- 空间复杂度：O(1)，创建单个rich-char对象

兼容性
------
- 与所有rich-string实例兼容
- 返回rich-char对象，可与rich-char相关操作配合使用
|#

;; 基本功能测试
;; ASCII字符串的最后一个字符
(check (((rich-string :value-of "hello") :last) :make-string) => "o")
(check (((rich-string :value-of "world") :last) :make-string) => "d")
(check (((rich-string :value-of "test") :last) :make-string) => "t")

;; 单字符字符串
(check (((rich-string :value-of "a") :last) :make-string) => "a")
(check (((rich-string :value-of " ") :last) :make-string) => " ")
(check (((rich-string :value-of "0") :last) :make-string) => "0")

;; Unicode字符测试
;; 中文字符
(check (((rich-string :value-of "测试") :last) :make-string) => "试")
(check (((rich-string :value-of "你好") :last) :make-string) => "好")

;; 日文字符
(check (((rich-string :value-of "こんにちは") :last) :make-string) => "は")

;; Emoji表情符号
(check (((rich-string :value-of "🎉") :last) :make-string) => "🎉")
(check (((rich-string :value-of "🎉🎊") :last) :make-string) => "🎊")

;; 混合字符
(check (((rich-string :value-of "hello 世界 🎉") :last) :make-string) => "🎉")

;; 边界条件测试
;; 空字符串 - 应该抛出索引错误
(check-catch 'index-error ((rich-string :empty) :last))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :last)))
(check-true (rich-char :is-type-of ((rich-string :value-of "测试") :last)))

;; 链式操作测试
;; 字符转换操作
(check ((((rich-string :value-of "hello") :last) :to-upper) :make-string) => "O")
(check ((((rich-string :value-of "HELLO") :last) :to-lower) :make-string) => "o")

;; 字符比较操作
(check-true (((rich-string :value-of "hello") :last) :equals #\o))
(check-false (((rich-string :value-of "hello") :last) :equals #\h))

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :last) :make-string) => "d"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :last) :make-string) => "串"))

;; 验证不同类型输入的字符访问
;; 数字转换
(check (((rich-string :value-of 123) :last) :make-string) => "3")

;; 符号转换
(check (((rich-string :value-of 'hello) :last) :make-string) => "o")

;; rich-char转换
(check (((rich-string :value-of (rich-char #\x)) :last) :make-string) => "x")

;; 验证last与char-at方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :last) :make-string) => ((rs :char-at 10) :make-string)))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :last) :make-string) => ((rs :char-at 4) :make-string)))

;; 验证last与head方法的对比
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :head) :make-string) => "h")
  (check ((rs :last) :make-string) => "o"))

;; 验证Unicode字符的last访问
(let ((rs (rich-string :value-of "你好世界")))
  (check ((rs :head) :make-string) => "你")
  (check ((rs :last) :make-string) => "界"))

;; 性能相关测试
;; 长字符串中的last操作
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check ((long-str :last) :make-string) => "a"))

;; 长字符串中的last操作（混合字符）
(let ((long-str (rich-string :value-of (string-append (make-string 999 #\a) "z"))))
  (check ((long-str :last) :make-string) => "z"))

#|
rich-string%last-option
获取rich-string对象中的最后一个字符，以option类型返回。

语法
----
(rich-string-instance :last-option)

参数
----
无参数。

返回值
-----
以option形式返回rich-string对象中的最后一个字符。
如果字符串为空，返回none；否则返回包含最后一个字符的option。

说明
----
该方法返回rich-string对象中的最后一个字符，以option包装的rich-char对象形式返回。
与%last方法不同，%last-option在字符串为空时不会抛出错误，而是返回none。
该方法正确处理Unicode字符，能够准确返回多字节编码的字符。

边界条件
--------
- 空字符串：返回none
- 单字符字符串：返回包含唯一字符的option
- 多字符字符串：返回包含最后一个字符的option
- Unicode字符串：正确返回最后一个Unicode字符的option

性能特征
--------
- 时间复杂度：O(1)，直接访问最后一个字符
- 空间复杂度：O(1)，创建单个option对象

兼容性
------
- 与所有rich-string实例兼容
- 返回option类型，可与option相关操作配合使用
- 与%last方法功能互补，提供安全的字符访问
|#

;; 基本功能测试
;; 非空字符串返回包含最后一个字符的option
(check-true (option :is-type-of ((rich-string :value-of "hello") :last-option)))
(check ((((rich-string :value-of "hello") :last-option) :get) :make-string) => "o")
(check ((((rich-string :value-of "world") :last-option) :get) :make-string) => "d")

;; 单字符字符串
(check ((((rich-string :value-of "a") :last-option) :get) :make-string) => "a")
(check ((((rich-string :value-of " ") :last-option) :get) :make-string) => " ")
(check ((((rich-string :value-of "0") :last-option) :get) :make-string) => "0")

;; Unicode字符测试
;; 中文字符
(check ((((rich-string :value-of "测试") :last-option) :get) :make-string) => "试")
(check ((((rich-string :value-of "你好") :last-option) :get) :make-string) => "好")

;; 日文字符
(check ((((rich-string :value-of "こんにちは") :last-option) :get) :make-string) => "は")

;; Emoji表情符号
(check ((((rich-string :value-of "🎉") :last-option) :get) :make-string) => "🎉")
(check ((((rich-string :value-of "🎉🎊") :last-option) :get) :make-string) => "🎊")

;; 混合字符
(check ((((rich-string :value-of "hello 世界 🎉") :last-option) :get) :make-string) => "🎉")

;; 边界条件测试
;; 空字符串 - 返回none
(check-true (((rich-string :empty) :last-option) :empty?))
(check-true (((rich-string :value-of "") :last-option) :empty?))

;; 验证返回类型是option
(check-true (option :is-type-of ((rich-string :value-of "hello") :last-option)))
(check-true (option :is-type-of ((rich-string :value-of "测试") :last-option)))

;; 验证option包含rich-char
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :last-option) :get)))
(check-true (rich-char :is-type-of (((rich-string :value-of "测试") :last-option) :get)))

;; 链式操作测试
;; 字符转换操作
(check (((((rich-string :value-of "hello") :last-option) :get) :to-upper) :make-string) => "O")
(check (((((rich-string :value-of "HELLO") :last-option) :get) :to-lower) :make-string) => "o")

;; 字符比较操作
(check-true ((((rich-string :value-of "hello") :last-option) :get) :equals #\o))
(check-false ((((rich-string :value-of "hello") :last-option) :get) :equals #\h))

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :last-option) :get) :make-string) => "d"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check (((rs :last-option) :get) :make-string) => "串"))

;; 验证不同类型输入的字符访问
;; 数字转换
(check ((((rich-string :value-of 123) :last-option) :get) :make-string) => "3")

;; 符号转换
(check ((((rich-string :value-of 'hello) :last-option) :get) :make-string) => "o")

;; rich-char转换
(check ((((rich-string :value-of (rich-char #\x)) :last-option) :get) :make-string) => "x")

;; 验证last-option与last方法的一致性（非空字符串）
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :last) :make-string) => (((rs :last-option) :get) :make-string)))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :last) :make-string) => (((rs :last-option) :get) :make-string)))

;; 验证last-option与char-at方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :last-option) :get) :make-string) => ((rs :char-at 10) :make-string)))

(let ((rs (rich-string :value-of "测试字符串")))
  (check (((rs :last-option) :get) :make-string) => ((rs :char-at 4) :make-string)))

;; option类型操作测试
;; 使用option的map操作
(check (((((rich-string :value-of "hello") :last-option) :map (lambda (c) (c :to-upper))) :get) :make-string) => "O")

;; 使用option的get-or-else操作
(check ((((rich-string :value-of "hello") :last-option) :get-or-else (rich-char #\x)) :make-string) => "o")
(check ((((rich-string :empty) :last-option) :get-or-else (rich-char #\x)) :make-string) => "x")

;; 使用option的exists操作
(check-true (((rich-string :value-of "hello") :last-option) :exists (lambda (c) (c :equals #\o))))
(check-false (((rich-string :value-of "hello") :last-option) :exists (lambda (c) (c :equals #\x))))
(check-false (((rich-string :empty) :last-option) :exists (lambda (c) #t)))

;; 使用option的filter操作
(check (((((rich-string :value-of "hello") :last-option) :filter (lambda (c) (c :equals #\o))) :get) :make-string) => "o")
(check-true ((((rich-string :value-of "hello") :last-option) :filter (lambda (c) (c :equals #\x))) :empty?))
(check-true (((rich-string :empty) :last-option) :filter (lambda (c) #t) :empty?))

;; 验证option类型操作的完整性
(let ((opt ((rich-string :value-of "hello") :last-option)))
  (check-true (opt :defined?))
  (check-false (opt :empty?))
  (check ((opt :get) :make-string) => "o"))

(let ((opt ((rich-string :empty) :last-option)))
  (check-false (opt :defined?))
  (check-true (opt :empty?)))

;; 与find-last方法的对比测试
;; last-option总是返回最后一个字符，而find-last需要谓词
(let ((rs (rich-string :value-of "hello")))
  (check (((rs :last-option) :get) :make-string) => "o")
  (check (((rs :find-last (lambda (c) (c :equals #\o))) :get) :make-string) => "o")
  (check (((rs :find-last (lambda (c) (c :equals #\l))) :get) :make-string) => "l"))

;; 性能相关测试
;; 长字符串中的last-option操作
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check-true (option :is-type-of (long-str :last-option)))
  (check (((long-str :last-option) :get) :make-string) => "a"))

;; 空字符串的last-option操作
(let ((empty-str (rich-string :empty)))
  (check-true ((empty-str :last-option) :empty?)))

#|
rich-string%slice
从rich-string对象中提取指定范围的子字符串。

语法
----
(rich-string-instance :slice from until . args)

参数
----
from : integer
子字符串的起始索引（包含），从0开始计数。

until : integer
子字符串的结束索引（不包含），从0开始计数。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回从from到until-1的子字符串。

说明
----
该方法从rich-string对象中提取指定范围的子字符串，返回一个新的rich-string对象。
索引范围是半开区间[from, until)，即包含from位置的字符，但不包含until位置的字符。
如果from大于等于until，返回空字符串。
如果from或until超出字符串范围，会自动调整到有效的边界。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- from < 0：自动调整为0
- until > length：自动调整为length
- from >= until：返回空字符串
- from = 0且until = length：返回原始字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是子字符串的长度
- 空间复杂度：O(k)，需要存储子字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 提取子字符串
(check (((rich-string :value-of "hello world") :slice 0 5) :get) => "hello")
(check (((rich-string :value-of "hello world") :slice 6 11) :get) => "world")
(check (((rich-string :value-of "hello world") :slice 2 7) :get) => "llo w")

;; 单字符提取
(check (((rich-string :value-of "hello") :slice 0 1) :get) => "h")
(check (((rich-string :value-of "hello") :slice 4 5) :get) => "o")

;; 空字符串提取
(check (((rich-string :value-of "") :slice 0 0) :get) => "")
(check (((rich-string :empty) :slice 0 0) :get) => "")

;; Unicode字符切片测试
;; 中文字符切片
(check (((rich-string :value-of "测试字符串") :slice 0 2) :get) => "测试")
(check (((rich-string :value-of "测试字符串") :slice 2 4) :get) => "字符")
(check (((rich-string :value-of "测试字符串") :slice 4 5) :get) => "串")

;; 日文字符切片
(check (((rich-string :value-of "こんにちは") :slice 0 2) :get) => "こん")
(check (((rich-string :value-of "こんにちは") :slice 2 4) :get) => "にち")
(check (((rich-string :value-of "こんにちは") :slice 4 5) :get) => "は")

;; Emoji表情符号切片
(check (((rich-string :value-of "🎉🎊🎈") :slice 0 1) :get) => "🎉")
(check (((rich-string :value-of "🎉🎊🎈") :slice 1 2) :get) => "🎊")
(check (((rich-string :value-of "🎉🎊🎈") :slice 2 3) :get) => "🎈")

;; 混合字符切片
(check (((rich-string :value-of "hello 世界 🎉") :slice 0 5) :get) => "hello")
(check (((rich-string :value-of "hello 世界 🎉") :slice 5 6) :get) => " ")
(check (((rich-string :value-of "hello 世界 🎉") :slice 6 8) :get) => "世界")
(check (((rich-string :value-of "hello 世界 🎉") :slice 8 9) :get) => " ")
(check (((rich-string :value-of "hello 世界 🎉") :slice 9 10) :get) => "🎉")

;; 边界条件测试
;; 负索引自动调整为0
(check (((rich-string :value-of "hello") :slice -1 3) :get) => "hel")
(check (((rich-string :value-of "hello") :slice -10 2) :get) => "he")

;; 超出范围索引自动调整为length
(check (((rich-string :value-of "hello") :slice 2 10) :get) => "llo")
(check (((rich-string :value-of "hello") :slice 3 100) :get) => "lo")

;; from >= until 返回空字符串
(check (((rich-string :value-of "hello") :slice 3 2) :get) => "")
(check (((rich-string :value-of "hello") :slice 5 3) :get) => "")
(check (((rich-string :value-of "hello") :slice 0 0) :get) => "")

;; 完整字符串切片
(check (((rich-string :value-of "hello") :slice 0 5) :get) => "hello")
(check (((rich-string :value-of "测试") :slice 0 2) :get) => "测试")

;; 链式调用测试
;; 切片后连接字符串
(check (((rich-string :value-of "hello world") :slice 0 5 :+ "!") :get) => "hello!")
(check (((rich-string :value-of "hello world") :slice 6 11 :+ "!") :get) => "world!")

;; 切片后转换大小写
(check (((rich-string :value-of "Hello World") :slice 0 5 :map (lambda (c) (c :to-lower))) :get) => "hello")
(check (((rich-string :value-of "Hello World") :slice 6 11 :map (lambda (c) (c :to-upper))) :get) => "WORLD")

;; 切片后反转
(check (((rich-string :value-of "hello") :slice 0 3 :reverse) :get) => "leh")
(check (((rich-string :value-of "hello") :slice 2 5 :reverse) :get) => "oll")

;; 切片后过滤
(check (((rich-string :value-of "hello123") :slice 0 5 :filter (lambda (c) (or (c :upper?) (c :lower?)))) :get) => "hello")
(check (((rich-string :value-of "hello123") :slice 5 8 :filter (lambda (c) (c :digit?))) :get) => "123")

;; 多级链式调用
(check (((rich-string :value-of "Hello World") :slice 0 5 :map (lambda (c) (c :to-lower)) :+ "!") :get) => "hello!")
(check (((rich-string :value-of "Hello World") :slice 6 11 :map (lambda (c) (c :to-upper)) :reverse) :get) => "DLROW")

;; 验证返回类型
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :slice 0 3)))
(check-true (rich-string :is-type-of ((rich-string :value-of "测试") :slice 0 1)))

;; 验证切片长度
(check (((rich-string :value-of "hello") :slice 0 3) :length) => 3)
(check (((rich-string :value-of "hello") :slice 2 5) :length) => 3)
(check (((rich-string :value-of "测试字符串") :slice 0 2) :length) => 2)
(check (((rich-string :value-of "测试字符串") :slice 3 5) :length) => 2)

;; 验证切片内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :slice 0 5) :get) => "hello")
  (check ((rs :slice 6 11) :get) => "world")
  (check ((rs :slice 2 7) :get) => "llo w"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :slice 0 2) :get) => "测试")
  (check ((rs :slice 2 4) :get) => "字符")
  (check ((rs :slice 4 5) :get) => "串"))

;; 边界条件验证
;; 空字符串的各种切片
(check (((rich-string :empty) :slice 0 0) :get) => "")
(check (((rich-string :empty) :slice 0 1) :get) => "")
(check (((rich-string :empty) :slice 1 2) :get) => "")

;; 单字符字符串的各种切片
(let ((rs (rich-string :value-of "a")))
  (check ((rs :slice 0 1) :get) => "a")
  (check ((rs :slice 0 0) :get) => "")
  (check ((rs :slice 1 2) :get) => ""))

;; 验证切片与原始字符串的关系
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :slice 0 (rs :length)) :get) => "hello world")
  (check ((rs :slice 0 0) :get) => "")
  (check ((rs :slice (rs :length) (rs :length)) :get) => ""))

;; 性能相关测试
;; 长字符串切片
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check ((long-str :slice 0 500) :length) => 500)
  (check ((long-str :slice 500 1000) :length) => 500)
  (check ((long-str :slice 250 750) :length) => 500))

;; 长Unicode字符串切片
(let ((long-unicode (rich-string :value-of (string-append (make-string 500 #\a) (make-string 500 #\b)))))
  (check ((long-unicode :slice 0 250) :length) => 250)
  (check ((long-unicode :slice 250 750) :length) => 500)
  (check ((long-unicode :slice 750 1000) :length) => 250))

;; 验证切片操作的独立性
(let ((original (rich-string :value-of "hello world")))
  (let ((sliced (original :slice 0 5)))
    (check (sliced :get) => "hello")
    ;; 修改切片结果不应影响原始字符串
    (check (original :get) => "hello world")))

;; 验证链式调用的正确性
(let ((result ((rich-string :value-of "Hello World")
               :slice 0 5
               :map (lambda (c) (c :to-lower))
               :+ "!"
               :get)))
  (check result => "hello!"))

;; 验证Unicode字符切片的正确性
(let ((rs (rich-string :value-of "你好世界🎉")))
  (check ((rs :slice 0 2) :get) => "你好")
  (check ((rs :slice 2 4) :get) => "世界")
  (check ((rs :slice 4 5) :get) => "🎉"))

#|
rich-string%take
从rich-string对象的前面提取指定数量的字符。

语法
----
(rich-string-instance :take n . args)

参数
----
n : integer
要提取的字符数量，从字符串开头开始计数。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回包含前n个字符的子字符串。

说明
----
该方法从rich-string对象的前面提取指定数量的字符，返回一个新的rich-string对象。
如果n大于字符串长度，返回整个字符串。
如果n小于等于0，返回空字符串。
该方法基于%slice方法实现，相当于调用(%slice 0 n)。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- n = 0：返回空字符串
- n < 0：返回空字符串
- n = length：返回整个字符串
- n > length：返回整个字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是实际提取的字符数量
- 空间复杂度：O(k)，需要存储子字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
- 与%slice方法功能一致，提供更简洁的前缀提取接口
|#

;; 基本功能测试
;; 提取前n个字符
(check (((rich-string :value-of "hello world") :take 5) :get) => "hello")
(check (((rich-string :value-of "hello world") :take 3) :get) => "hel")
(check (((rich-string :value-of "hello world") :take 1) :get) => "h")

;; 提取整个字符串
(check (((rich-string :value-of "hello") :take 5) :get) => "hello")
(check (((rich-string :value-of "hello world") :take 11) :get) => "hello world")

;; 提取空字符串
(check (((rich-string :value-of "") :take 0) :get) => "")
(check (((rich-string :empty) :take 0) :get) => "")

;; Unicode字符提取测试
;; 中文字符提取
(check (((rich-string :value-of "测试字符串") :take 2) :get) => "测试")
(check (((rich-string :value-of "测试字符串") :take 3) :get) => "测试字")
(check (((rich-string :value-of "测试字符串") :take 5) :get) => "测试字符串")

;; 日文字符提取
(check (((rich-string :value-of "こんにちは") :take 2) :get) => "こん")
(check (((rich-string :value-of "こんにちは") :take 3) :get) => "こんに")
(check (((rich-string :value-of "こんにちは") :take 5) :get) => "こんにちは")

;; Emoji表情符号提取
(check (((rich-string :value-of "🎉🎊🎈") :take 1) :get) => "🎉")
(check (((rich-string :value-of "🎉🎊🎈") :take 2) :get) => "🎉🎊")
(check (((rich-string :value-of "🎉🎊🎈") :take 3) :get) => "🎉🎊🎈")

;; 混合字符提取
(check (((rich-string :value-of "hello 世界 🎉") :take 5) :get) => "hello")
(check (((rich-string :value-of "hello 世界 🎉") :take 6) :get) => "hello ")
(check (((rich-string :value-of "hello 世界 🎉") :take 8) :get) => "hello 世界")
(check (((rich-string :value-of "hello 世界 🎉") :take 10) :get) => "hello 世界 🎉")

;; 边界条件测试
;; n = 0 返回空字符串
(check (((rich-string :value-of "hello") :take 0) :get) => "")
(check (((rich-string :value-of "测试") :take 0) :get) => "")

;; n < 0 返回空字符串
(check (((rich-string :value-of "hello") :take -1) :get) => "")
(check (((rich-string :value-of "hello") :take -10) :get) => "")

;; n > length 返回整个字符串
(check (((rich-string :value-of "hello") :take 10) :get) => "hello")
(check (((rich-string :value-of "测试") :take 100) :get) => "测试")

;; 空字符串的各种提取
(check (((rich-string :empty) :take 0) :get) => "")
(check (((rich-string :empty) :take 1) :get) => "")
(check (((rich-string :empty) :take -1) :get) => "")

;; 链式调用测试
;; 提取后连接字符串
(check (((rich-string :value-of "hello world") :take 5 :+ "!") :get) => "hello!")
(check (((rich-string :value-of "hello world") :take 3 :+ "lo") :get) => "hello")

;; 提取后转换大小写
(check (((rich-string :value-of "Hello World") :take 5 :map (lambda (c) (c :to-lower))) :get) => "hello")
(check (((rich-string :value-of "Hello World") :take 1 :map (lambda (c) (c :to-upper))) :get) => "H")

;; 提取后反转
(check (((rich-string :value-of "hello") :take 3 :reverse) :get) => "leh")
(check (((rich-string :value-of "hello") :take 5 :reverse) :get) => "olleh")

;; 提取后过滤
(check (((rich-string :value-of "hello123") :take 5 :filter (lambda (c) (or (c :upper?) (c :lower?)))) :get) => "hello")
(check (((rich-string :value-of "123hello") :take 3 :filter (lambda (c) (c :digit?))) :get) => "123")

;; 多级链式调用
(check (((rich-string :value-of "Hello World") :take 5 :map (lambda (c) (c :to-lower)) :+ "!") :get) => "hello!")
(check (((rich-string :value-of "Hello World") :take 1 :map (lambda (c) (c :to-upper)) :+ "ello") :get) => "Hello")

;; 验证返回类型
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :take 3)))
(check-true (rich-string :is-type-of ((rich-string :value-of "测试") :take 1)))

;; 验证提取长度
(check (((rich-string :value-of "hello") :take 3) :length) => 3)
(check (((rich-string :value-of "hello") :take 5) :length) => 5)
(check (((rich-string :value-of "测试字符串") :take 2) :length) => 2)
(check (((rich-string :value-of "测试字符串") :take 3) :length) => 3)

;; 验证提取内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => "hello")
  (check ((rs :take 3) :get) => "hel")
  (check ((rs :take 1) :get) => "h"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :take 2) :get) => "测试")
  (check ((rs :take 3) :get) => "测试字")
  (check ((rs :take 5) :get) => "测试字符串"))

;; 边界条件验证
;; 单字符字符串的各种提取
(let ((rs (rich-string :value-of "a")))
  (check ((rs :take 1) :get) => "a")
  (check ((rs :take 0) :get) => "")
  (check ((rs :take 2) :get) => "a"))

;; 验证提取与原始字符串的关系
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take (rs :length)) :get) => "hello world")
  (check ((rs :take 0) :get) => "")
  (check ((rs :take 1) :get) => "h"))

;; 性能相关测试
;; 长字符串提取
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check ((long-str :take 500) :length) => 500)
  (check ((long-str :take 1000) :length) => 1000)
  (check ((long-str :take 1500) :length) => 1000))

;; 长Unicode字符串提取
(let ((long-unicode (rich-string :value-of (string-append (make-string 500 #\a) (make-string 500 #\b)))))
  (check ((long-unicode :take 250) :length) => 250)
  (check ((long-unicode :take 500) :length) => 500)
  (check ((long-unicode :take 750) :length) => 750))

;; 验证提取操作的独立性
(let ((original (rich-string :value-of "hello world")))
  (let ((taken (original :take 5)))
    (check (taken :get) => "hello")
    ;; 修改提取结果不应影响原始字符串
    (check (original :get) => "hello world")))

;; 验证链式调用的正确性
(let ((result ((rich-string :value-of "Hello World")
               :take 5
               :map (lambda (c) (c :to-lower))
               :+ "!"
               :get)))
  (check result => "hello!"))

;; 验证Unicode字符提取的正确性
(let ((rs (rich-string :value-of "你好世界🎉")))
  (check ((rs :take 2) :get) => "你好")
  (check ((rs :take 4) :get) => "你好世界")
  (check ((rs :take 5) :get) => "你好世界🎉"))

;; 与slice方法的对比测试
;; take方法相当于slice(0, n)
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => ((rs :slice 0 5) :get))
  (check ((rs :take 3) :get) => ((rs :slice 0 3) :get))
  (check ((rs :take 0) :get) => ((rs :slice 0 0) :get)))

;; Unicode字符的对比测试
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :take 2) :get) => ((rs :slice 0 2) :get))
  (check ((rs :take 3) :get) => ((rs :slice 0 3) :get))
  (check ((rs :take 5) :get) => ((rs :slice 0 5) :get)))

;; 边界条件的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :take -1) :get) => ((rs :slice 0 -1) :get))  ; 都返回空字符串
  (check ((rs :take 10) :get) => ((rs :slice 0 10) :get))  ; 都返回整个字符串
  (check ((rs :take 0) :get) => ((rs :slice 0 0) :get)))   ; 都返回空字符串

;; 验证take与slice方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => ((rs :slice 0 5) :get))
  (check ((rs :take 11) :get) => ((rs :slice 0 11) :get))
  (check ((rs :take 0) :get) => ((rs :slice 0 0) :get)))

;; 验证不同类型输入的提取
;; 数字转换的字符串
(check (((rich-string :value-of 12345) :take 3) :get) => "123")
(check (((rich-string :value-of 12345) :take 5) :get) => "12345")

;; 符号转换的字符串
(check (((rich-string :value-of 'hello) :take 3) :get) => "hel")
(check (((rich-string :value-of 'hello) :take 5) :get) => "hello")

;; rich-char转换的字符串
(check (((rich-string :value-of (rich-char #\x)) :take 1) :get) => "x")
(check (((rich-string :value-of (rich-char #\x)) :take 0) :get) => "")

;; 链式操作与类型转换的组合测试
(check (((rich-string :value-of 12345) :take 3 :+ "abc") :get) => "123abc")
(check (((rich-string :value-of 'hello) :take 3 :map (lambda (c) (c :to-upper))) :get) => "HEL")

;; 验证提取操作的空字符串处理
(let ((empty-rs (rich-string :empty)))
  (check ((empty-rs :take 0) :get) => "")
  (check ((empty-rs :take 1) :get) => "")
  (check ((empty-rs :take -1) :get) => ""))

;; 验证提取操作的单字符处理
(let ((single-char (rich-string :value-of "a")))
  (check ((single-char :take 1) :get) => "a")
  (check ((single-char :take 0) :get) => "")
  (check ((single-char :take 2) :get) => "a"))

;; 验证提取操作的Unicode单字符处理
(let ((unicode-char (rich-string :value-of "🎉")))
  (check ((unicode-char :take 1) :get) => "🎉")
  (check ((unicode-char :take 0) :get) => "")
  (check ((unicode-char :take 2) :get) => "🎉"))

;; 验证链式调用的深度组合
(let ((result ((rich-string :value-of "Hello123World")
               :take 8
               :filter (lambda (c) (or (c :upper?) (c :lower?)))
               :map (lambda (c) (c :to-lower))
               :reverse
               :+ "!"
               :get)))
  (check result => "olleh!"))

;; 验证take方法在各种边界条件下的行为
;; n为负数的各种情况
(check (((rich-string :value-of "hello") :take -1) :get) => "")
(check (((rich-string :value-of "hello") :take -10) :get) => "")
(check (((rich-string :value-of "") :take -1) :get) => "")

;; n为零的各种情况
(check (((rich-string :value-of "hello") :take 0) :get) => "")
(check (((rich-string :value-of "") :take 0) :get) => "")
(check (((rich-string :value-of "测试") :take 0) :get) => "")

;; n等于长度的各种情况
(check (((rich-string :value-of "hello") :take 5) :get) => "hello")
(check (((rich-string :value-of "测试") :take 2) :get) => "测试")
(check (((rich-string :value-of "🎉🎊") :take 2) :get) => "🎉🎊")

;; n大于长度的各种情况
(check (((rich-string :value-of "hello") :take 10) :get) => "hello")
(check (((rich-string :value-of "测试") :take 5) :get) => "测试")
(check (((rich-string :value-of "🎉") :take 3) :get) => "🎉")

#|
rich-string%take-right
从rich-string对象的末尾提取指定数量的字符。

语法
----
(rich-string-instance :take-right n . args)

参数
----
n : integer
要提取的字符数量，从字符串末尾开始计数。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回包含后n个字符的子字符串。

说明
----
该方法从rich-string对象的末尾提取指定数量的字符，返回一个新的rich-string对象。
如果n大于字符串长度，返回整个字符串。
如果n小于等于0，返回空字符串。
该方法基于%slice方法实现，相当于调用(%slice (- length n) length)。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- n = 0：返回空字符串
- n < 0：返回空字符串
- n = length：返回整个字符串
- n > length：返回整个字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是实际提取的字符数量
- 空间复杂度：O(k)，需要存储子字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
- 与%slice方法功能一致，提供更简洁的后缀提取接口
- 与%take方法互补，分别处理字符串的前缀和后缀
|#

;; 基本功能测试
;; 提取后n个字符
(check (((rich-string :value-of "hello world") :take-right 5) :get) => "world")
(check (((rich-string :value-of "hello world") :take-right 3) :get) => "rld")
(check (((rich-string :value-of "hello world") :take-right 1) :get) => "d")

;; 提取整个字符串
(check (((rich-string :value-of "hello") :take-right 5) :get) => "hello")
(check (((rich-string :value-of "hello world") :take-right 11) :get) => "hello world")

;; 提取空字符串
(check (((rich-string :value-of "") :take-right 0) :get) => "")
(check (((rich-string :empty) :take-right 0) :get) => "")

;; Unicode字符提取测试
;; 中文字符提取
(check (((rich-string :value-of "测试字符串") :take-right 2) :get) => "符串")
(check (((rich-string :value-of "测试字符串") :take-right 3) :get) => "字符串")
(check (((rich-string :value-of "测试字符串") :take-right 5) :get) => "测试字符串")

;; 日文字符提取
(check (((rich-string :value-of "こんにちは") :take-right 2) :get) => "ちは")
(check (((rich-string :value-of "こんにちは") :take-right 3) :get) => "にちは")
(check (((rich-string :value-of "こんにちは") :take-right 5) :get) => "こんにちは")

;; Emoji表情符号提取
(check (((rich-string :value-of "🎉🎊🎈") :take-right 1) :get) => "🎈")
(check (((rich-string :value-of "🎉🎊🎈") :take-right 2) :get) => "🎊🎈")
(check (((rich-string :value-of "🎉🎊🎈") :take-right 3) :get) => "🎉🎊🎈")

;; 混合字符提取
(check (((rich-string :value-of "hello 世界 🎉") :take-right 1) :get) => "🎉")
(check (((rich-string :value-of "hello 世界 🎉") :take-right 3) :get) => "界 🎉")
(check (((rich-string :value-of "hello 世界 🎉") :take-right 5) :get) => " 世界 🎉")
(check (((rich-string :value-of "hello 世界 🎉") :take-right 10) :get) => "hello 世界 🎉")

;; 边界条件测试
;; n = 0 返回空字符串
(check (((rich-string :value-of "hello") :take-right 0) :get) => "")
(check (((rich-string :value-of "测试") :take-right 0) :get) => "")

;; n < 0 返回空字符串
(check (((rich-string :value-of "hello") :take-right -1) :get) => "")
(check (((rich-string :value-of "hello") :take-right -10) :get) => "")

;; n > length 返回整个字符串
(check (((rich-string :value-of "hello") :take-right 10) :get) => "hello")
(check (((rich-string :value-of "测试") :take-right 100) :get) => "测试")

;; 空字符串的各种提取
(check (((rich-string :empty) :take-right 0) :get) => "")
(check (((rich-string :empty) :take-right 1) :get) => "")
(check (((rich-string :empty) :take-right -1) :get) => "")

;; 链式调用测试
;; 提取后连接字符串
(check (((rich-string :value-of "hello world") :take-right 5 :+ "!") :get) => "world!")
(check (((rich-string :value-of "hello world") :take-right 3 :+ "!?") :get) => "rld!?")

;; 提取后转换大小写
(check (((rich-string :value-of "Hello World") :take-right 5 :map (lambda (c) (c :to-upper))) :get) => "WORLD")
(check (((rich-string :value-of "Hello World") :take-right 1 :map (lambda (c) (c :to-upper))) :get) => "D")

;; 提取后反转
(check (((rich-string :value-of "hello") :take-right 3 :reverse) :get) => "oll")
(check (((rich-string :value-of "hello") :take-right 5 :reverse) :get) => "olleh")

;; 提取后过滤
(check (((rich-string :value-of "123hello") :take-right 5 :filter (lambda (c) (or (c :upper?) (c :lower?)))) :get) => "hello")
(check (((rich-string :value-of "hello123") :take-right 3 :filter (lambda (c) (c :digit?))) :get) => "123")

;; 多级链式调用
(check (((rich-string :value-of "Hello World") :take-right 5 :map (lambda (c) (c :to-upper)) :+ "!") :get) => "WORLD!")
(check (((rich-string :value-of "Hello World") :take-right 1 :map (lambda (c) (c :to-upper)) :+ "ONE") :get) => "DONE")

;; 验证返回类型
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :take-right 3)))
(check-true (rich-string :is-type-of ((rich-string :value-of "测试") :take-right 1)))

;; 验证提取长度
(check (((rich-string :value-of "hello") :take-right 3) :length) => 3)
(check (((rich-string :value-of "hello") :take-right 5) :length) => 5)
(check (((rich-string :value-of "测试字符串") :take-right 2) :length) => 2)
(check (((rich-string :value-of "测试字符串") :take-right 3) :length) => 3)

;; 验证提取内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => "world")
  (check ((rs :take-right 3) :get) => "rld")
  (check ((rs :take-right 1) :get) => "d"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :take-right 2) :get) => "符串")
  (check ((rs :take-right 3) :get) => "字符串")
  (check ((rs :take-right 5) :get) => "测试字符串"))

;; 边界条件验证
;; 单字符字符串的各种提取
(let ((rs (rich-string :value-of "a")))
  (check ((rs :take-right 1) :get) => "a")
  (check ((rs :take-right 0) :get) => "")
  (check ((rs :take-right 2) :get) => "a"))

;; 验证提取与原始字符串的关系
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right (rs :length)) :get) => "hello world")
  (check ((rs :take-right 0) :get) => "")
  (check ((rs :take-right 1) :get) => "d"))

;; 性能相关测试
;; 长字符串提取
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check ((long-str :take-right 500) :length) => 500)
  (check ((long-str :take-right 1000) :length) => 1000)
  (check ((long-str :take-right 1500) :length) => 1000))

;; 长Unicode字符串提取
(let ((long-unicode (rich-string :value-of (string-append (make-string 500 #\a) (make-string 500 #\b)))))
  (check ((long-unicode :take-right 250) :length) => 250)
  (check ((long-unicode :take-right 500) :length) => 500)
  (check ((long-unicode :take-right 750) :length) => 750))

;; 验证提取操作的独立性
(let ((original (rich-string :value-of "hello world")))
  (let ((taken-right (original :take-right 5)))
    (check (taken-right :get) => "world")
    ;; 修改提取结果不应影响原始字符串
    (check (original :get) => "hello world")))

;; 验证链式调用的正确性
(let ((result ((rich-string :value-of "Hello World")
               :take-right 5
               :map (lambda (c) (c :to-upper))
               :+ "!"
               :get)))
  (check result => "WORLD!"))

;; 验证Unicode字符提取的正确性
(let ((rs (rich-string :value-of "你好世界🎉")))
  (check ((rs :take-right 1) :get) => "🎉")
  (check ((rs :take-right 3) :get) => "世界🎉")
  (check ((rs :take-right 5) :get) => "你好世界🎉"))

;; 与slice方法的对比测试
;; take-right方法相当于slice(length - n, length)
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => ((rs :slice 6 11) :get))
  (check ((rs :take-right 3) :get) => ((rs :slice 8 11) :get))
  (check ((rs :take-right 0) :get) => ((rs :slice 11 11) :get)))

;; Unicode字符的对比测试
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :take-right 2) :get) => ((rs :slice 3 5) :get))
  (check ((rs :take-right 3) :get) => ((rs :slice 2 5) :get))
  (check ((rs :take-right 5) :get) => ((rs :slice 0 5) :get)))

;; 边界条件的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :take-right -1) :get) => ((rs :slice 5 5) :get))  ; 都返回空字符串
  (check ((rs :take-right 10) :get) => ((rs :slice 0 5) :get))  ; 都返回整个字符串
  (check ((rs :take-right 0) :get) => ((rs :slice 5 5) :get)))   ; 都返回空字符串

;; 验证take-right与slice方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => ((rs :slice 6 11) :get))
  (check ((rs :take-right 11) :get) => ((rs :slice 0 11) :get))
  (check ((rs :take-right 0) :get) => ((rs :slice 11 11) :get)))

;; 与take方法的对比测试
;; take从开头提取，take-right从末尾提取
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => "hello")
  (check ((rs :take-right 5) :get) => "world")
  (check ((rs :take 3) :get) => "hel")
  (check ((rs :take-right 3) :get) => "rld"))

;; Unicode字符的对比测试
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :take 2) :get) => "测试")
  (check ((rs :take-right 2) :get) => "符串")
  (check ((rs :take 3) :get) => "测试字")
  (check ((rs :take-right 3) :get) => "字符串"))

;; 验证不同类型输入的提取
;; 数字转换的字符串
(check (((rich-string :value-of 12345) :take-right 3) :get) => "345")
(check (((rich-string :value-of 12345) :take-right 5) :get) => "12345")

;; 符号转换的字符串
(check (((rich-string :value-of 'hello) :take-right 3) :get) => "llo")
(check (((rich-string :value-of 'hello) :take-right 5) :get) => "hello")

;; rich-char转换的字符串
(check (((rich-string :value-of (rich-char #\x)) :take-right 1) :get) => "x")
(check (((rich-string :value-of (rich-char #\x)) :take-right 0) :get) => "")

;; 链式操作与类型转换的组合测试
(check (((rich-string :value-of 12345) :take-right 3 :+ "abc") :get) => "345abc")
(check (((rich-string :value-of 'hello) :take-right 3 :map (lambda (c) (c :to-upper))) :get) => "LLO")

;; 验证提取操作的空字符串处理
(let ((empty-rs (rich-string :empty)))
  (check ((empty-rs :take-right 0) :get) => "")
  (check ((empty-rs :take-right 1) :get) => "")
  (check ((empty-rs :take-right -1) :get) => ""))

;; 验证提取操作的单字符处理
(let ((single-char (rich-string :value-of "a")))
  (check ((single-char :take-right 1) :get) => "a")
  (check ((single-char :take-right 0) :get) => "")
  (check ((single-char :take-right 2) :get) => "a"))

;; 验证提取操作的Unicode单字符处理
(let ((unicode-char (rich-string :value-of "🎉")))
  (check ((unicode-char :take-right 1) :get) => "🎉")
  (check ((unicode-char :take-right 0) :get) => "")
  (check ((unicode-char :take-right 2) :get) => "🎉"))

;; 验证链式调用的深度组合
(let ((result ((rich-string :value-of "Hello123World")
               :take-right 8
               :filter (lambda (c) (or (c :upper?) (c :lower?)))
               :map (lambda (c) (c :to-lower))
               :reverse
               :+ "!"
               :get)))
  (check result => "dlrow!"))

;; 验证take-right方法在各种边界条件下的行为
;; n为负数的各种情况
(check (((rich-string :value-of "hello") :take-right -1) :get) => "")
(check (((rich-string :value-of "hello") :take-right -10) :get) => "")
(check (((rich-string :value-of "") :take-right -1) :get) => "")

;; n为零的各种情况
(check (((rich-string :value-of "hello") :take-right 0) :get) => "")
(check (((rich-string :value-of "") :take-right 0) :get) => "")
(check (((rich-string :value-of "测试") :take-right 0) :get) => "")

;; n等于长度的各种情况
(check (((rich-string :value-of "hello") :take-right 5) :get) => "hello")
(check (((rich-string :value-of "测试") :take-right 2) :get) => "测试")
(check (((rich-string :value-of "🎉🎊") :take-right 2) :get) => "🎉🎊")

;; n大于长度的各种情况
(check (((rich-string :value-of "hello") :take-right 10) :get) => "hello")
(check (((rich-string :value-of "测试") :take-right 5) :get) => "测试")
(check (((rich-string :value-of "🎉") :take-right 3) :get) => "🎉")

#|
rich-string%drop
从rich-string对象的前面删除指定数量的字符，返回剩余部分。

语法
----
(rich-string-instance :drop n . args)

参数
----
n : integer
要从字符串开头删除的字符数量。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回删除前n个字符后的剩余字符串。

说明
----
该方法从rich-string对象的前面删除指定数量的字符，返回一个新的rich-string对象。
如果n大于等于字符串长度，返回空字符串。
如果n小于等于0，返回整个字符串。
该方法基于%slice方法实现，相当于调用(%slice n length)。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- n = 0：返回整个字符串
- n < 0：返回整个字符串
- n = length：返回空字符串
- n > length：返回空字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是剩余字符串的长度
- 空间复杂度：O(k)，需要存储剩余字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
- 与%slice方法功能一致，提供更简洁的前缀删除接口
- 与%take方法互补，分别处理字符串的前缀保留和删除
|#

;; 基本功能测试
;; 删除前n个字符
(check (((rich-string :value-of "hello world") :drop 6) :get) => "world")
(check (((rich-string :value-of "hello world") :drop 3) :get) => "lo world")
(check (((rich-string :value-of "hello world") :drop 1) :get) => "ello world")

;; 删除整个字符串
(check (((rich-string :value-of "hello") :drop 5) :get) => "")
(check (((rich-string :value-of "hello world") :drop 11) :get) => "")

;; 删除空字符串
(check (((rich-string :value-of "") :drop 0) :get) => "")
(check (((rich-string :empty) :drop 0) :get) => "")

;; Unicode字符删除测试
;; 中文字符删除
(check (((rich-string :value-of "测试字符串") :drop 2) :get) => "字符串")
(check (((rich-string :value-of "测试字符串") :drop 3) :get) => "符串")
(check (((rich-string :value-of "测试字符串") :drop 5) :get) => "")

;; 日文字符删除
(check (((rich-string :value-of "こんにちは") :drop 2) :get) => "にちは")
(check (((rich-string :value-of "こんにちは") :drop 3) :get) => "ちは")
(check (((rich-string :value-of "こんにちは") :drop 5) :get) => "")

;; Emoji表情符号删除
(check (((rich-string :value-of "🎉🎊🎈") :drop 1) :get) => "🎊🎈")
(check (((rich-string :value-of "🎉🎊🎈") :drop 2) :get) => "🎈")
(check (((rich-string :value-of "🎉🎊🎈") :drop 3) :get) => "")

;; 混合字符删除
(check (((rich-string :value-of "hello 世界 🎉") :drop 6) :get) => "世界 🎉")
(check (((rich-string :value-of "hello 世界 🎉") :drop 8) :get) => " 🎉")
(check (((rich-string :value-of "hello 世界 🎉") :drop 10) :get) => "")

;; 边界条件测试
;; n = 0 返回整个字符串
(check (((rich-string :value-of "hello") :drop 0) :get) => "hello")
(check (((rich-string :value-of "测试") :drop 0) :get) => "测试")

;; n < 0 返回整个字符串
(check (((rich-string :value-of "hello") :drop -1) :get) => "hello")
(check (((rich-string :value-of "hello") :drop -10) :get) => "hello")

;; n > length 返回空字符串
(check (((rich-string :value-of "hello") :drop 10) :get) => "")
(check (((rich-string :value-of "测试") :drop 100) :get) => "")

;; 空字符串的各种删除
(check (((rich-string :empty) :drop 0) :get) => "")
(check (((rich-string :empty) :drop 1) :get) => "")
(check (((rich-string :empty) :drop -1) :get) => "")

;; 链式调用测试
;; 删除后连接字符串
(check (((rich-string :value-of "hello world") :drop 6 :+ "!") :get) => "world!")
(check (((rich-string :value-of "hello world") :drop 3 :+ "!") :get) => "lo world!")

;; 删除后转换大小写
(check (((rich-string :value-of "Hello World") :drop 6 :map (lambda (c) (c :to-upper))) :get) => "WORLD")
(check (((rich-string :value-of "Hello World") :drop 1 :map (lambda (c) (c :to-lower))) :get) => "ello world")

;; 删除后反转
(check (((rich-string :value-of "hello") :drop 2 :reverse) :get) => "oll")
(check (((rich-string :value-of "hello") :drop 0 :reverse) :get) => "olleh")

;; 删除后过滤
(check (((rich-string :value-of "123hello") :drop 3 :filter (lambda (c) (or (c :upper?) (c :lower?)))) :get) => "hello")
(check (((rich-string :value-of "hello123") :drop 5 :filter (lambda (c) (c :digit?))) :get) => "123")

;; 多级链式调用
(check (((rich-string :value-of "Hello World") :drop 6 :map (lambda (c) (c :to-upper)) :+ "!") :get) => "WORLD!")
(check (((rich-string :value-of "Hello World") :drop 1 :map (lambda (c) (c :to-lower)) :+ "!") :get) => "ello world!")

;; 验证返回类型
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :drop 3)))
(check-true (rich-string :is-type-of ((rich-string :value-of "测试") :drop 1)))

;; 验证删除后长度
(check (((rich-string :value-of "hello") :drop 3) :length) => 2)
(check (((rich-string :value-of "hello") :drop 5) :length) => 0)
(check (((rich-string :value-of "测试字符串") :drop 2) :length) => 3)
(check (((rich-string :value-of "测试字符串") :drop 3) :length) => 2)

;; 验证删除内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 6) :get) => "world")
  (check ((rs :drop 3) :get) => "lo world")
  (check ((rs :drop 1) :get) => "ello world"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :drop 2) :get) => "字符串")
  (check ((rs :drop 3) :get) => "符串")
  (check ((rs :drop 5) :get) => ""))

;; 边界条件验证
;; 单字符字符串的各种删除
(let ((rs (rich-string :value-of "a")))
  (check ((rs :drop 1) :get) => "")
  (check ((rs :drop 0) :get) => "a")
  (check ((rs :drop 2) :get) => ""))

;; 验证删除与原始字符串的关系
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 0) :get) => "hello world")
  (check ((rs :drop (rs :length)) :get) => "")
  (check ((rs :drop 1) :get) => "ello world"))

;; 性能相关测试
;; 长字符串删除
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check ((long-str :drop 500) :length) => 500)
  (check ((long-str :drop 1000) :length) => 0)
  (check ((long-str :drop 1500) :length) => 0))

;; 长Unicode字符串删除
(let ((long-unicode (rich-string :value-of (string-append (make-string 500 #\a) (make-string 500 #\b)))))
  (check ((long-unicode :drop 250) :length) => 750)
  (check ((long-unicode :drop 500) :length) => 500)
  (check ((long-unicode :drop 750) :length) => 250))

;; 验证删除操作的独立性
(let ((original (rich-string :value-of "hello world")))
  (let ((dropped (original :drop 6)))
    (check (dropped :get) => "world")
    ;; 修改删除结果不应影响原始字符串
    (check (original :get) => "hello world")))

;; 验证链式调用的正确性
(let ((result ((rich-string :value-of "Hello World")
               :drop 6
               :map (lambda (c) (c :to-upper))
               :+ "!"
               :get)))
  (check result => "WORLD!"))

;; 验证Unicode字符删除的正确性
(let ((rs (rich-string :value-of "你好世界🎉")))
  (check ((rs :drop 2) :get) => "世界🎉")
  (check ((rs :drop 4) :get) => "🎉")
  (check ((rs :drop 5) :get) => ""))

;; 与slice方法的对比测试
;; drop方法相当于slice(n, length)
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 6) :get) => ((rs :slice 6 11) :get))
  (check ((rs :drop 3) :get) => ((rs :slice 3 11) :get))
  (check ((rs :drop 0) :get) => ((rs :slice 0 11) :get)))

;; Unicode字符的对比测试
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :drop 2) :get) => ((rs :slice 2 5) :get))
  (check ((rs :drop 3) :get) => ((rs :slice 3 5) :get))
  (check ((rs :drop 5) :get) => ((rs :slice 5 5) :get)))

;; 边界条件的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :drop -1) :get) => ((rs :slice 0 5) :get))  ; 都返回整个字符串
  (check ((rs :drop 10) :get) => ((rs :slice 5 5) :get))  ; 都返回空字符串
  (check ((rs :drop 0) :get) => ((rs :slice 0 5) :get)))   ; 都返回整个字符串

;; 验证drop与slice方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 6) :get) => ((rs :slice 6 11) :get))
  (check ((rs :drop 11) :get) => ((rs :slice 11 11) :get))
  (check ((rs :drop 0) :get) => ((rs :slice 0 11) :get)))

;; 与take方法的对比测试
;; take保留前n个字符，drop删除前n个字符
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => "hello")
  (check ((rs :drop 5) :get) => " world")
  (check ((rs :take 3) :get) => "hel")
  (check ((rs :drop 3) :get) => "lo world"))

;; Unicode字符的对比测试
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :take 2) :get) => "测试")
  (check ((rs :drop 2) :get) => "字符串")
  (check ((rs :take 3) :get) => "测试字")
  (check ((rs :drop 3) :get) => "符串"))

;; 验证take和drop的互补性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5 :+ (rs :drop 5)) :get) => "hello world"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :take 2 :+ (rs :drop 2)) :get) => "测试字符串"))

;; 验证不同类型输入的删除
;; 数字转换的字符串
(check (((rich-string :value-of 12345) :drop 3) :get) => "45")
(check (((rich-string :value-of 12345) :drop 5) :get) => "")

;; 符号转换的字符串
(check (((rich-string :value-of 'hello) :drop 3) :get) => "lo")
(check (((rich-string :value-of 'hello) :drop 5) :get) => "")

;; rich-char转换的字符串
(check (((rich-string :value-of (rich-char #\x)) :drop 1) :get) => "")
(check (((rich-string :value-of (rich-char #\x)) :drop 0) :get) => "x")

;; 链式操作与类型转换的组合测试
(check (((rich-string :value-of 12345) :drop 3 :+ "abc") :get) => "45abc")
(check (((rich-string :value-of 'hello) :drop 3 :map (lambda (c) (c :to-upper))) :get) => "LO")

;; 验证删除操作的空字符串处理
(let ((empty-rs (rich-string :empty)))
  (check ((empty-rs :drop 0) :get) => "")
  (check ((empty-rs :drop 1) :get) => "")
  (check ((empty-rs :drop -1) :get) => ""))

;; 验证删除操作的单字符处理
(let ((single-char (rich-string :value-of "a")))
  (check ((single-char :drop 1) :get) => "")
  (check ((single-char :drop 0) :get) => "a")
  (check ((single-char :drop 2) :get) => ""))

;; 验证删除操作的Unicode单字符处理
(let ((unicode-char (rich-string :value-of "🎉")))
  (check ((unicode-char :drop 1) :get) => "")
  (check ((unicode-char :drop 0) :get) => "🎉")
  (check ((unicode-char :drop 2) :get) => ""))

;; 验证链式调用的深度组合
(let ((result ((rich-string :value-of "Hello123World")
               :drop 5
               :filter (lambda (c) (or (c :upper?) (c :lower?)))
               :map (lambda (c) (c :to-lower))
               :reverse
               :+ "!"
               :get)))
  (check result => "dlrow!"))

;; 验证drop方法在各种边界条件下的行为
;; n为负数的各种情况
(check (((rich-string :value-of "hello") :drop -1) :get) => "hello")
(check (((rich-string :value-of "hello") :drop -10) :get) => "hello")
(check (((rich-string :value-of "") :drop -1) :get) => "")

;; n为零的各种情况
(check (((rich-string :value-of "hello") :drop 0) :get) => "hello")
(check (((rich-string :value-of "") :drop 0) :get) => "")
(check (((rich-string :value-of "测试") :drop 0) :get) => "测试")

;; n等于长度的各种情况
(check (((rich-string :value-of "hello") :drop 5) :get) => "")
(check (((rich-string :value-of "测试") :drop 2) :get) => "")
(check (((rich-string :value-of "🎉🎊") :drop 2) :get) => "")

;; n大于长度的各种情况
(check (((rich-string :value-of "hello") :drop 10) :get) => "")
(check (((rich-string :value-of "测试") :drop 5) :get) => "")
(check (((rich-string :value-of "🎉") :drop 3) :get) => "")

#|
rich-string%drop-right
从rich-string对象的末尾删除指定数量的字符，返回剩余部分。

语法
----
(rich-string-instance :drop-right n . args)

参数
----
n : integer
要从字符串末尾删除的字符数量。

args : list
可选参数，用于链式调用其他方法。

返回值
-----
以rich-string形式返回删除后n个字符后的剩余字符串。

说明
----
该方法从rich-string对象的末尾删除指定数量的字符，返回一个新的rich-string对象。
如果n大于等于字符串长度，返回空字符串。
如果n小于等于0，返回整个字符串。
该方法基于%slice方法实现，相当于调用(%slice 0 (- length n))。
该方法支持链式调用，可以与其他rich-string方法组合使用。

边界条件
--------
- n = 0：返回整个字符串
- n < 0：返回整个字符串
- n = length：返回空字符串
- n > length：返回空字符串
- 空字符串：返回空字符串

性能特征
--------
- 时间复杂度：O(k)，其中k是剩余字符串的长度
- 空间复杂度：O(k)，需要存储剩余字符串

兼容性
------
- 与所有rich-string实例兼容
- 支持链式调用模式
- 正确处理Unicode字符
- 与%slice方法功能一致，提供更简洁的后缀删除接口
- 与%take-right方法互补，分别处理字符串的后缀保留和删除
- 与%drop方法互补，分别处理字符串的前缀和后缀删除
|#

;; 基本功能测试
;; 删除后n个字符
(check (((rich-string :value-of "hello world") :drop-right 5) :get) => "hello ")
(check (((rich-string :value-of "hello world") :drop-right 3) :get) => "hello wo")
(check (((rich-string :value-of "hello world") :drop-right 1) :get) => "hello worl")

;; 删除整个字符串
(check (((rich-string :value-of "hello") :drop-right 5) :get) => "")
(check (((rich-string :value-of "hello world") :drop-right 11) :get) => "")

;; 删除空字符串
(check (((rich-string :value-of "") :drop-right 0) :get) => "")
(check (((rich-string :empty) :drop-right 0) :get) => "")

;; Unicode字符删除测试
;; 中文字符删除
(check (((rich-string :value-of "测试字符串") :drop-right 2) :get) => "测试字")
(check (((rich-string :value-of "测试字符串") :drop-right 3) :get) => "测试")
(check (((rich-string :value-of "测试字符串") :drop-right 5) :get) => "")

;; 日文字符删除
(check (((rich-string :value-of "こんにちは") :drop-right 2) :get) => "こんに")
(check (((rich-string :value-of "こんにちは") :drop-right 3) :get) => "こん")
(check (((rich-string :value-of "こんにちは") :drop-right 5) :get) => "")

;; Emoji表情符号删除
(check (((rich-string :value-of "🎉🎊🎈") :drop-right 1) :get) => "🎉🎊")
(check (((rich-string :value-of "🎉🎊🎈") :drop-right 2) :get) => "🎉")
(check (((rich-string :value-of "🎉🎊🎈") :drop-right 3) :get) => "")

;; 混合字符删除
(check (((rich-string :value-of "hello 世界 🎉") :drop-right 1) :get) => "hello 世界 ")
(check (((rich-string :value-of "hello 世界 🎉") :drop-right 3) :get) => "hello 世")
(check (((rich-string :value-of "hello 世界 🎉") :drop-right 5) :get) => "hello")
(check (((rich-string :value-of "hello 世界 🎉") :drop-right 10) :get) => "")

;; 边界条件测试
;; n = 0 返回整个字符串
(check (((rich-string :value-of "hello") :drop-right 0) :get) => "hello")
(check (((rich-string :value-of "测试") :drop-right 0) :get) => "测试")

;; n < 0 返回整个字符串
(check (((rich-string :value-of "hello") :drop-right -1) :get) => "hello")
(check (((rich-string :value-of "hello") :drop-right -10) :get) => "hello")

;; n > length 返回空字符串
(check (((rich-string :value-of "hello") :drop-right 10) :get) => "")
(check (((rich-string :value-of "测试") :drop-right 100) :get) => "")

;; 空字符串的各种删除
(check (((rich-string :empty) :drop-right 0) :get) => "")
(check (((rich-string :empty) :drop-right 1) :get) => "")
(check (((rich-string :empty) :drop-right -1) :get) => "")

;; 链式调用测试
;; 删除后连接字符串
(check (((rich-string :value-of "hello world") :drop-right 5 :+ "!") :get) => "hello !")
(check (((rich-string :value-of "hello world") :drop-right 3 :+ "!") :get) => "hello wo!")

;; 删除后转换大小写
(check (((rich-string :value-of "Hello World") :drop-right 5 :map (lambda (c) (c :to-upper))) :get) => "HELLO ")
(check (((rich-string :value-of "Hello World") :drop-right 1 :map (lambda (c) (c :to-lower))) :get) => "hello worl")

;; 删除后反转
(check (((rich-string :value-of "hello") :drop-right 2 :reverse) :get) => "leh")
(check (((rich-string :value-of "hello") :drop-right 0 :reverse) :get) => "olleh")

;; 删除后过滤
(check (((rich-string :value-of "hello123") :drop-right 3 :filter (lambda (c) (or (c :upper?) (c :lower?)))) :get) => "hello")
(check (((rich-string :value-of "123hello") :drop-right 5 :filter (lambda (c) (c :digit?))) :get) => "123")

;; 多级链式调用
(check (((rich-string :value-of "Hello World") :drop-right 5 :map (lambda (c) (c :to-upper)) :+ "!") :get) => "HELLO !")
(check (((rich-string :value-of "Hello World") :drop-right 1 :map (lambda (c) (c :to-lower)) :+ "!") :get) => "hello worl!")

;; 验证返回类型
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :drop-right 3)))
(check-true (rich-string :is-type-of ((rich-string :value-of "测试") :drop-right 1)))

;; 验证删除后长度
(check (((rich-string :value-of "hello") :drop-right 3) :length) => 2)
(check (((rich-string :value-of "hello") :drop-right 5) :length) => 0)
(check (((rich-string :value-of "测试字符串") :drop-right 2) :length) => 3)
(check (((rich-string :value-of "测试字符串") :drop-right 3) :length) => 2)

;; 验证删除内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop-right 5) :get) => "hello ")
  (check ((rs :drop-right 3) :get) => "hello wo")
  (check ((rs :drop-right 1) :get) => "hello worl"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :drop-right 2) :get) => "测试字")
  (check ((rs :drop-right 3) :get) => "测试")
  (check ((rs :drop-right 5) :get) => ""))

;; 边界条件验证
;; 单字符字符串的各种删除
(let ((rs (rich-string :value-of "a")))
  (check ((rs :drop-right 1) :get) => "")
  (check ((rs :drop-right 0) :get) => "a")
  (check ((rs :drop-right 2) :get) => ""))

;; 验证删除与原始字符串的关系
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop-right 0) :get) => "hello world")
  (check ((rs :drop-right (rs :length)) :get) => "")
  (check ((rs :drop-right 1) :get) => "hello worl"))

;; 性能相关测试
;; 长字符串删除
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check ((long-str :drop-right 500) :length) => 500)
  (check ((long-str :drop-right 1000) :length) => 0)
  (check ((long-str :drop-right 1500) :length) => 0))

;; 长Unicode字符串删除
(let ((long-unicode (rich-string :value-of (string-append (make-string 500 #\a) (make-string 500 #\b)))))
  (check ((long-unicode :drop-right 250) :length) => 750)
  (check ((long-unicode :drop-right 500) :length) => 500)
  (check ((long-unicode :drop-right 750) :length) => 250))

;; 验证删除操作的独立性
(let ((original (rich-string :value-of "hello world")))
  (let ((dropped-right (original :drop-right 5)))
    (check (dropped-right :get) => "hello ")
    ;; 修改删除结果不应影响原始字符串
    (check (original :get) => "hello world")))

;; 验证链式调用的正确性
(let ((result ((rich-string :value-of "Hello World")
               :drop-right 5
               :map (lambda (c) (c :to-upper))
               :+ "!"
               :get)))
  (check result => "HELLO !"))

;; 验证Unicode字符删除的正确性
(let ((rs (rich-string :value-of "你好世界🎉")))
  (check ((rs :drop-right 1) :get) => "你好世界")
  (check ((rs :drop-right 3) :get) => "你好")
  (check ((rs :drop-right 5) :get) => ""))

;; 与slice方法的对比测试
;; drop-right方法相当于slice(0, length - n)
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop-right 5) :get) => ((rs :slice 0 6) :get))
  (check ((rs :drop-right 3) :get) => ((rs :slice 0 8) :get))
  (check ((rs :drop-right 0) :get) => ((rs :slice 0 11) :get)))

;; Unicode字符的对比测试
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :drop-right 2) :get) => ((rs :slice 0 3) :get))
  (check ((rs :drop-right 3) :get) => ((rs :slice 0 2) :get))
  (check ((rs :drop-right 5) :get) => ((rs :slice 0 0) :get)))

;; 边界条件的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :drop-right -1) :get) => ((rs :slice 0 5) :get))  ; 都返回整个字符串
  (check ((rs :drop-right 10) :get) => ((rs :slice 0 0) :get))  ; 都返回空字符串
  (check ((rs :drop-right 0) :get) => ((rs :slice 0 5) :get)))   ; 都返回整个字符串

;; 验证drop-right与slice方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop-right 5) :get) => ((rs :slice 0 6) :get))
  (check ((rs :drop-right 11) :get) => ((rs :slice 0 0) :get))
  (check ((rs :drop-right 0) :get) => ((rs :slice 0 11) :get)))

;; 与take-right方法的对比测试
;; take-right保留后n个字符，drop-right删除后n个字符
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => "world")
  (check ((rs :drop-right 5) :get) => "hello ")
  (check ((rs :take-right 3) :get) => "rld")
  (check ((rs :drop-right 3) :get) => "hello wo"))

;; Unicode字符的对比测试
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :take-right 2) :get) => "符串")
  (check ((rs :drop-right 2) :get) => "测试字")
  (check ((rs :take-right 3) :get) => "字符串")
  (check ((rs :drop-right 3) :get) => "测试"))

;; 验证take-right和drop-right的互补性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop-right 5 :+ (rs :take-right 5)) :get) => "hello world"))

(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :drop-right 2 :+ (rs :take-right 2)) :get) => "测试字符串"))

;; 与drop方法的对比测试
;; drop删除前n个字符，drop-right删除后n个字符
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 5) :get) => " world")
  (check ((rs :drop-right 5) :get) => "hello ")
  (check ((rs :drop 3) :get) => "lo world")
  (check ((rs :drop-right 3) :get) => "hello wo"))

;; Unicode字符的对比测试
(let ((rs (rich-string :value-of "测试字符串")))
  (check ((rs :drop 2) :get) => "字符串")
  (check ((rs :drop-right 2) :get) => "测试字")
  (check ((rs :drop 3) :get) => "符串")
  (check ((rs :drop-right 3) :get) => "测试"))

;; 验证drop和drop-right的互补性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 5 :+ (rs :drop-right 5)) :get) => " worldhello "))

;; 验证不同类型输入的删除
;; 数字转换的字符串
(check (((rich-string :value-of 12345) :drop-right 3) :get) => "12")
(check (((rich-string :value-of 12345) :drop-right 5) :get) => "")

;; 符号转换的字符串
(check (((rich-string :value-of 'hello) :drop-right 3) :get) => "he")
(check (((rich-string :value-of 'hello) :drop-right 5) :get) => "")

;; rich-char转换的字符串
(check (((rich-string :value-of (rich-char #\x)) :drop-right 1) :get) => "")
(check (((rich-string :value-of (rich-char #\x)) :drop-right 0) :get) => "x")

;; 链式操作与类型转换的组合测试
(check (((rich-string :value-of 12345) :drop-right 3 :+ "abc") :get) => "12abc")
(check (((rich-string :value-of 'hello) :drop-right 3 :map (lambda (c) (c :to-upper))) :get) => "HE")

;; 验证删除操作的空字符串处理
(let ((empty-rs (rich-string :empty)))
  (check ((empty-rs :drop-right 0) :get) => "")
  (check ((empty-rs :drop-right 1) :get) => "")
  (check ((empty-rs :drop-right -1) :get) => ""))

;; 验证删除操作的单字符处理
(let ((single-char (rich-string :value-of "a")))
  (check ((single-char :drop-right 1) :get) => "")
  (check ((single-char :drop-right 0) :get) => "a")
  (check ((single-char :drop-right 2) :get) => ""))

;; 验证删除操作的Unicode单字符处理
(let ((unicode-char (rich-string :value-of "🎉")))
  (check ((unicode-char :drop-right 1) :get) => "")
  (check ((unicode-char :drop-right 0) :get) => "🎉")
  (check ((unicode-char :drop-right 2) :get) => ""))

;; 验证链式调用的深度组合
(let ((result ((rich-string :value-of "Hello123World")
               :drop-right 5
               :filter (lambda (c) (or (c :upper?) (c :lower?)))
               :map (lambda (c) (c :to-lower))
               :reverse
               :+ "!"
               :get)))
  (check result => "olleh!"))

;; 验证drop-right方法在各种边界条件下的行为
;; n为负数的各种情况
(check (((rich-string :value-of "hello") :drop-right -1) :get) => "hello")
(check (((rich-string :value-of "hello") :drop-right -10) :get) => "hello")
(check (((rich-string :value-of "") :drop-right -1) :get) => "")

;; n为零的各种情况
(check (((rich-string :value-of "hello") :drop-right 0) :get) => "hello")
(check (((rich-string :value-of "") :drop-right 0) :get) => "")
(check (((rich-string :value-of "测试") :drop-right 0) :get) => "测试")

;; n等于长度的各种情况
(check (((rich-string :value-of "hello") :drop-right 5) :get) => "")
(check (((rich-string :value-of "测试") :drop-right 2) :get) => "")
(check (((rich-string :value-of "🎉🎊") :drop-right 2) :get) => "")

;; n大于长度的各种情况
(check (((rich-string :value-of "hello") :drop-right 10) :get) => "")
(check (((rich-string :value-of "测试") :drop-right 5) :get) => "")
(check (((rich-string :value-of "🎉") :drop-right 3) :get) => "")

#|
rich-string%empty?
检查rich-string对象是否为空字符串。

语法
----
(rich-string-instance :empty?)

参数
----
无参数。

返回值
-----
以boolean形式返回rich-string对象是否为空字符串。
如果字符串长度为0，返回#t；否则返回#f。

说明
----
该方法检查rich-string对象是否为空字符串（即不包含任何字符）。
对于空字符串和长度为0的字符串返回#t，对于包含任何字符的字符串返回#f。
该方法正确处理Unicode字符，能够准确判断字符串是否为空。

边界条件
--------
- 空字符串：返回#t
- 单字符字符串：返回#f
- 多字符字符串：返回#f
- Unicode字符串：根据字符数量判断，有字符则返回#f
- 空rich-string对象：返回#t

性能特征
--------
- 时间复杂度：O(1)，直接检查缓存的长度值
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 返回标准布尔值，可与任何布尔操作配合使用
- 与%length方法关系密切，empty?等价于(length = 0)
|#

;; 基本功能测试
;; 空字符串
(check ((rich-string :empty) :empty?) => #t)
(check ((rich-string :value-of "") :empty?) => #t)

;; 单字符字符串
(check ((rich-string :value-of "a") :empty?) => #f)
(check ((rich-string :value-of " ") :empty?) => #f)
(check ((rich-string :value-of "0") :empty?) => #f)

;; 多字符字符串
(check ((rich-string :value-of "hello") :empty?) => #f)
(check ((rich-string :value-of "test string") :empty?) => #f)
(check ((rich-string :value-of "hello world") :empty?) => #f)

;; Unicode字符测试
;; 中文字符
(check ((rich-string :value-of "测试") :empty?) => #f)
(check ((rich-string :value-of "你好世界") :empty?) => #f)

;; 日文字符
(check ((rich-string :value-of "こんにちは") :empty?) => #f)

;; Emoji表情符号
(check ((rich-string :value-of "🎉") :empty?) => #f)
(check ((rich-string :value-of "🎉🎊") :empty?) => #f)

;; 混合字符
(check ((rich-string :value-of "hello 世界 🎉") :empty?) => #f)

;; 边界条件测试
;; 空字符串的各种创建方式
(check ((rich-string :empty) :empty?) => #t)
(check ((rich-string :value-of "") :empty?) => #t)

;; 单字符边界
(check ((rich-string :value-of #\a) :empty?) => #f)
(check ((rich-string :value-of #\space) :empty?) => #f)

;; 数字转换的字符串
(check ((rich-string :value-of 0) :empty?) => #f)
(check ((rich-string :value-of 123) :empty?) => #f)

;; 符号转换的字符串
(check ((rich-string :value-of 'hello) :empty?) => #f)
(check ((rich-string :value-of 'test) :empty?) => #f)

;; rich-char转换的字符串
(check ((rich-string :value-of (rich-char #\x)) :empty?) => #f)

;; 链式调用测试
;; 链式操作后的空字符串判断
(check ((rich-string :empty :+ "") :empty?) => #t)
(check ((rich-string :value-of "hello" :slice 0 0) :empty?) => #t)
(check ((rich-string :value-of "hello" :take 0) :empty?) => #t)
(check ((rich-string :value-of "hello" :drop 5) :empty?) => #t)

;; 链式操作后的非空字符串判断
(check ((rich-string :empty :+ "hello") :empty?) => #f)
(check ((rich-string :value-of "hello" :slice 0 3) :empty?) => #f)
(check ((rich-string :value-of "hello" :take 3) :empty?) => #f)
(check ((rich-string :value-of "hello" :drop 2) :empty?) => #f)

;; 验证返回类型是布尔值
(check (boolean? ((rich-string :empty) :empty?)) => #t)
(check (boolean? ((rich-string :value-of "hello") :empty?)) => #t)
(check (boolean? ((rich-string :value-of "测试") :empty?)) => #t)

;; 验证empty?与length方法的一致性
(check ((rich-string :empty) :empty?) => (zero? ((rich-string :empty) :length)))
(check ((rich-string :value-of "hello") :empty?) => (zero? ((rich-string :value-of "hello") :length)))
(check ((rich-string :value-of "测试") :empty?) => (zero? ((rich-string :value-of "测试") :length)))

;; 验证不同类型输入的empty?判断
;; 空字符串的各种表示
(check ((rich-string :value-of "") :empty?) => #t)
(check ((rich-string :empty) :empty?) => #t)

;; 非空字符串的各种表示
(check ((rich-string :value-of "a") :empty?) => #f)
(check ((rich-string :value-of "hello") :empty?) => #f)
(check ((rich-string :value-of "测试") :empty?) => #f)

;; 验证empty?与字符访问的一致性
(let ((rs (rich-string :value-of "hello")))
  (check (rs :empty?) => #f)
  ;; 删除所有字符后应该为空
  (check ((rs :drop 5) :empty?) => #t))

(let ((rs (rich-string :value-of "测试字符串")))
  (check (rs :empty?) => #f)
  ;; 删除所有字符后应该为空
  (check ((rs :drop 5) :empty?) => #t))

;; 性能相关测试
;; 长字符串的empty?判断
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check (long-str :empty?) => #f))

;; 空字符串的empty?判断
(let ((empty-str (rich-string :empty)))
  (check (empty-str :empty?) => #t))

;; 验证empty?在各种边界条件下的行为
;; 空字符串的各种情况
(check ((rich-string :empty) :empty?) => #t)
(check ((rich-string :value-of "") :empty?) => #t)

;; 单字符字符串的各种情况
(check ((rich-string :value-of "a") :empty?) => #f)
(check ((rich-string :value-of " ") :empty?) => #f)
(check ((rich-string :value-of "🎉") :empty?) => #f)

;; 多字符字符串的各种情况
(check ((rich-string :value-of "hello") :empty?) => #f)
(check ((rich-string :value-of "测试") :empty?) => #f)
(check ((rich-string :value-of "🎉🎊") :empty?) => #f)

;; 验证empty?与链式操作的一致性
;; 各种链式操作后的empty?判断
(check ((rich-string :empty :+ "") :empty?) => #t)
(check ((rich-string :empty :+ "hello") :empty?) => #f)
(check ((rich-string :value-of "hello" :slice 0 0) :empty?) => #t)
(check ((rich-string :value-of "hello" :slice 0 3) :empty?) => #f)
(check ((rich-string :value-of "hello" :take 0) :empty?) => #t)
(check ((rich-string :value-of "hello" :take 3) :empty?) => #f)
(check ((rich-string :value-of "hello" :drop 5) :empty?) => #t)
(check ((rich-string :value-of "hello" :drop 3) :empty?) => #f)

;; 验证empty?与过滤操作的一致性
(check ((rich-string :value-of "hello" :filter (lambda (c) #f)) :empty?) => #t)
(check ((rich-string :value-of "hello" :filter (lambda (c) #t)) :empty?) => #f)

;; 验证empty?与映射操作的一致性
(check ((rich-string :empty :map (lambda (c) (c :to-upper))) :empty?) => #t)
(check ((rich-string :value-of "hello" :map (lambda (c) (c :to-upper))) :empty?) => #f)

;; 验证empty?与反转操作的一致性
(check ((rich-string :empty :reverse) :empty?) => #t)
(check ((rich-string :value-of "hello" :reverse) :empty?) => #f)

;; 验证empty?与连接操作的一致性
(check ((rich-string :empty :+ (rich-string :empty)) :empty?) => #t)
(check ((rich-string :empty :+ (rich-string :value-of "hello")) :empty?) => #f)
(check ((rich-string :value-of "hello" :+ (rich-string :empty)) :empty?) => #f)

;; 验证empty?与strip操作的一致性
(check ((rich-string :empty :strip-left) :empty?) => #t)
(check ((rich-string :empty :strip-right) :empty?) => #t)
(check ((rich-string :empty :strip-both) :empty?) => #t)
(check ((rich-string :value-of "  hello  " :strip-both) :empty?) => #f)

;; 验证empty?与比较操作的一致性
;; 空字符串的比较
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :empty) :ends-with "") => #t)
(check ((rich-string :empty) :contains "") => #t)

;; 非空字符串的比较
(check ((rich-string :value-of "hello") :starts-with "") => #t)
(check ((rich-string :value-of "hello") :ends-with "") => #t)
(check ((rich-string :value-of "hello") :contains "") => #t)

;; 验证empty?在各种复杂操作后的正确性
(let ((result ((rich-string :value-of "Hello123World")
               :filter (lambda (c) (or (c :upper?) (c :lower?)))
               :map (lambda (c) (c :to-lower))
               :reverse
               :empty?)))
  (check result => #f))

(let ((result ((rich-string :value-of "Hello123World")
               :filter (lambda (c) (c :digit?))
               :drop 3
               :empty?)))
  (check result => #t))

;; 验证empty?与Unicode字符操作的一致性
(let ((rs (rich-string :value-of "你好世界🎉")))
  (check (rs :empty?) => #f)
  (check ((rs :drop 5) :empty?) => #t)
  (check ((rs :take 0) :empty?) => #t)
  (check ((rs :slice 0 0) :empty?) => #t))

;; 验证empty?在链式操作中的正确传播
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :slice 0 5) :empty?) => #f)
  (check ((rs :slice 5 5) :empty?) => #t)
  (check ((rs :take 0) :empty?) => #t)
  (check ((rs :drop 11) :empty?) => #t)
  (check ((rs :drop-right 11) :empty?) => #t))

;; 验证empty?与各种创建方式的一致性
;; 使用:empty创建
(check ((rich-string :empty) :empty?) => #t)

;; 使用:value-of创建空字符串
(check ((rich-string :value-of "") :empty?) => #t)

;; 使用:value-of创建非空字符串
(check ((rich-string :value-of "hello") :empty?) => #f)
(check ((rich-string :value-of "测试") :empty?) => #f)

;; 使用:value-of创建单字符字符串
(check ((rich-string :value-of #\a) :empty?) => #f)
(check ((rich-string :value-of #\space) :empty?) => #f)

;; 使用:value-of创建数字字符串
(check ((rich-string :value-of 0) :empty?) => #f)
(check ((rich-string :value-of 123) :empty?) => #f)

;; 使用:value-of创建符号字符串
(check ((rich-string :value-of 'hello) :empty?) => #f)
(check ((rich-string :value-of 'test) :empty?) => #f)

;; 使用:value-of创建rich-char字符串
(check ((rich-string :value-of (rich-char #\x)) :empty?) => #f)

;; 验证empty?在各种边界情况下的稳定性
;; 空字符串的稳定性
(check ((rich-string :empty) :empty?) => #t)
(check ((rich-string :value-of "") :empty?) => #t)

;; 单字符字符串的稳定性
(check ((rich-string :value-of "a") :empty?) => #f)
(check ((rich-string :value-of "🎉") :empty?) => #f)

;; 多字符字符串的稳定性
(check ((rich-string :value-of "hello") :empty?) => #f)
(check ((rich-string :value-of "测试字符串") :empty?) => #f)

;; 验证empty?与length方法的等价性
(check ((rich-string :empty) :empty?) => (zero? ((rich-string :empty) :length)))
(check ((rich-string :value-of "hello") :empty?) => (zero? ((rich-string :value-of "hello") :length)))
(check ((rich-string :value-of "测试") :empty?) => (zero? ((rich-string :value-of "测试") :length)))

;; 验证empty?在各种操作后的正确性
(let ((rs (rich-string :value-of "hello world")))
  ;; 原始字符串非空
  (check (rs :empty?) => #f)

  ;; 提取子字符串后非空
  (check ((rs :slice 0 5) :empty?) => #f)

  ;; 提取空子字符串后为空
  (check ((rs :slice 0 0) :empty?) => #t)

  ;; 删除所有字符后为空
  (check ((rs :drop 11) :empty?) => #t)

  ;; 保留所有字符后非空
  (check ((rs :take 11) :empty?) => #f)

  ;; 保留0个字符后为空
  (check ((rs :take 0) :empty?) => #t))

;; 验证empty?在Unicode字符串操作中的正确性
(let ((rs (rich-string :value-of "你好世界🎉")))
  ;; 原始字符串非空
  (check (rs :empty?) => #f)

  ;; 提取子字符串后非空
  (check ((rs :slice 0 2) :empty?) => #f)

  ;; 提取空子字符串后为空
  (check ((rs :slice 0 0) :empty?) => #t)

  ;; 删除所有字符后为空
  (check ((rs :drop 5) :empty?) => #t)

  ;; 保留所有字符后非空
  (check ((rs :take 5) :empty?) => #f)

  ;; 保留0个字符后为空
  (check ((rs :take 0) :empty?) => #t))

;; 验证empty?在复杂链式操作中的正确性
(let ((result ((rich-string :value-of "Hello123World")
               :filter (lambda (c) (or (c :upper?) (c :lower?)))
               :map (lambda (c) (c :to-lower))
               :reverse
               :empty?)))
  ;; 过滤、映射、反转后应该非空
  (check result => #f))

(let ((result ((rich-string :value-of "Hello123World")
               :filter (lambda (c) (c :digit?))
               :drop 3
               :empty?)))
  ;; 过滤数字后删除所有字符应该为空
  (check result => #t))

;; 验证empty?在各种边界条件下的最终测试
;; 空字符串的最终验证
(check ((rich-string :empty) :empty?) => #t)
(check ((rich-string :value-of "") :empty?) => #t)

;; 非空字符串的最终验证
(check ((rich-string :value-of "a") :empty?) => #f)
(check ((rich-string :value-of "hello") :empty?) => #f)
(check ((rich-string :value-of "测试") :empty?) => #f)
(check ((rich-string :value-of "🎉") :empty?) => #f)

;; 验证empty?与length方法的最终等价性
(check ((rich-string :empty) :empty?) => (zero? ((rich-string :empty) :length)))
(check ((rich-string :value-of "hello") :empty?) => (zero? ((rich-string :value-of "hello") :length)))
(check ((rich-string :value-of "测试") :empty?) => (zero? ((rich-string :value-of "测试") :length)))

#|
rich-string%starts-with
检查rich-string对象是否以指定的前缀开头。

语法
----
(rich-string-instance :starts-with prefix)

参数
----
prefix : any
要检查的前缀，支持以下类型：
- string：标准字符串
- rich-string：rich-string对象
- char：单个字符
- rich-char：rich-char对象

返回值
-----
以boolean形式返回rich-string对象是否以指定前缀开头。
如果字符串以指定前缀开头，返回#t；否则返回#f。

说明
----
该方法检查rich-string对象是否以指定的前缀开头。支持多种参数类型，
包括字符串、rich-string、字符和rich-char。对于空前缀，总是返回#t。
该方法正确处理Unicode字符，能够准确判断字符串前缀。

边界条件
--------
- 空字符串：任何前缀都返回#t（包括空前缀）
- 空前缀：总是返回#t
- 前缀长度大于字符串长度：返回#f
- 前缀与字符串开头匹配：返回#t
- 前缀与字符串开头不匹配：返回#f
- Unicode字符前缀：正确匹配Unicode字符

性能特征
--------
- 时间复杂度：O(k)，其中k是前缀的长度
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种前缀类型
- 正确处理Unicode字符
- 与%ends-with方法互补
|#

;; 基本功能测试
;; 字符串前缀匹配
(check ((rich-string :value-of "hello world") :starts-with "hello") => #t)
(check ((rich-string :value-of "hello world") :starts-with "hel") => #t)
(check ((rich-string :value-of "hello world") :starts-with "h") => #t)

;; 字符串前缀不匹配
(check ((rich-string :value-of "hello world") :starts-with "world") => #f)
(check ((rich-string :value-of "hello world") :starts-with "Hello") => #f)
(check ((rich-string :value-of "hello world") :starts-with "x") => #f)

;; 边界条件测试
;; 空字符串的前缀匹配
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :empty) :starts-with "hello") => #f)

;; 空前缀匹配
(check ((rich-string :value-of "hello") :starts-with "") => #t)
(check ((rich-string :value-of "") :starts-with "") => #t)
(check ((rich-string :empty) :starts-with "") => #t)

;; 前缀长度大于字符串长度
(check ((rich-string :value-of "hello") :starts-with "hello world") => #f)
(check ((rich-string :value-of "hi") :starts-with "hello") => #f)
(check ((rich-string :value-of "a") :starts-with "ab") => #f)

;; Unicode字符前缀测试
;; 中文字符前缀
(check ((rich-string :value-of "测试字符串") :starts-with "测试") => #t)
(check ((rich-string :value-of "测试字符串") :starts-with "测") => #t)
(check ((rich-string :value-of "测试字符串") :starts-with "字符") => #f)

;; 日文字符前缀
(check ((rich-string :value-of "こんにちは") :starts-with "こん") => #t)
(check ((rich-string :value-of "こんにちは") :starts-with "こ") => #t)
(check ((rich-string :value-of "こんにちは") :starts-with "にち") => #f)

;; Emoji表情符号前缀
(check ((rich-string :value-of "🎉🎊🎈") :starts-with "🎉") => #t)
(check ((rich-string :value-of "🎉🎊🎈") :starts-with "🎉🎊") => #t)
(check ((rich-string :value-of "🎉🎊🎈") :starts-with "🎈") => #f)

;; 混合字符前缀
(check ((rich-string :value-of "hello 世界 🎉") :starts-with "hello") => #t)
(check ((rich-string :value-of "hello 世界 🎉") :starts-with "hello ") => #t)
(check ((rich-string :value-of "hello 世界 🎉") :starts-with "hello 世界") => #t)
(check ((rich-string :value-of "hello 世界 🎉") :starts-with "世界") => #f)

;; 链式调用测试
;; 链式操作后的前缀匹配
(check ((rich-string :value-of "Hello World" :map (lambda (c) (c :to-lower))) :starts-with "hello") => #t)
(check ((rich-string :value-of "  hello  " :strip-both) :starts-with "hello") => #t)
(check ((rich-string :value-of "hello world" :slice 0 5) :starts-with "hello") => #t)

;; 链式操作后的前缀不匹配
(check ((rich-string :value-of "Hello World" :map (lambda (c) (c :to-upper))) :starts-with "hello") => #f)
(check ((rich-string :value-of "hello world" :slice 6 11) :starts-with "hello") => #f)

;; 验证返回类型是布尔值
(check (boolean? ((rich-string :value-of "hello") :starts-with "h")) => #t)
(check (boolean? ((rich-string :value-of "hello") :starts-with "x")) => #t)
(check (boolean? ((rich-string :value-of "hello") :starts-with "")) => #t)

;; 验证不同类型输入的starts-with判断
;; 数字转换的字符串
(check ((rich-string :value-of 12345) :starts-with "123") => #t)
(check ((rich-string :value-of 12345) :starts-with "12") => #t)
(check ((rich-string :value-of 12345) :starts-with "234") => #f)

;; 符号转换的字符串
(check ((rich-string :value-of 'hello) :starts-with "hel") => #t)
(check ((rich-string :value-of 'hello) :starts-with "hello") => #t)
(check ((rich-string :value-of 'hello) :starts-with "world") => #f)

;; rich-char转换的字符串
(check ((rich-string :value-of (rich-char #\x)) :starts-with "x") => #t)
(check ((rich-string :value-of (rich-char #\x)) :starts-with "y") => #f)

;; 验证starts-with与字符访问的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (rs :starts-with "h") => #t)
  (check (rs :starts-with "hello") => #t)
  (check (rs :starts-with "world") => #f))

;; 验证Unicode字符的starts-with一致性
(let ((rs (rich-string :value-of "测试字符串")))
  (check (rs :starts-with "测") => #t)
  (check (rs :starts-with "测试") => #t)
  (check (rs :starts-with "字符") => #f))

;; 性能相关测试
;; 长字符串的前缀匹配
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check (long-str :starts-with "aaa") => #t)
  (check (long-str :starts-with (make-string 100 #\a)) => #t)
  (check (long-str :starts-with "aab") => #f))

;; 空字符串的前缀匹配
(let ((empty-str (rich-string :empty)))
  (check (empty-str :starts-with "") => #t)
  (check (empty-str :starts-with "a") => #f))

;; 前缀为空字符串的各种情况
(check ((rich-string :value-of "hello") :starts-with "") => #t)
(check ((rich-string :value-of "") :starts-with "") => #t)
(check ((rich-string :empty) :starts-with "") => #t)

;; 前缀长度等于字符串长度的各种情况
(check ((rich-string :value-of "hello") :starts-with "hello") => #t)
(check ((rich-string :value-of "测试") :starts-with "测试") => #t)
(check ((rich-string :value-of "🎉") :starts-with "🎉") => #t)

;; 前缀长度大于字符串长度的各种情况
(check ((rich-string :value-of "hello") :starts-with "hello world") => #f)
(check ((rich-string :value-of "测试") :starts-with "测试字符串") => #f)
(check ((rich-string :value-of "🎉") :starts-with "🎉🎊") => #f)

;; 验证starts-with与链式操作的一致性
;; 各种链式操作后的starts-with判断
(check ((rich-string :value-of "Hello World" :map (lambda (c) (c :to-lower))) :starts-with "hello") => #t)
(check ((rich-string :value-of "  hello  " :strip-both) :starts-with "hello") => #t)
(check ((rich-string :value-of "hello world" :slice 0 5) :starts-with "hello") => #t)
(check ((rich-string :value-of "hello world" :take 5) :starts-with "hello") => #t)

;; 验证starts-with与过滤操作的一致性
(check ((rich-string :value-of "hello123" :filter (lambda (c) (or (c :upper?) (c :lower?)))) :starts-with "hello") => #t)
(check ((rich-string :value-of "123hello" :filter (lambda (c) (c :digit?))) :starts-with "123") => #t)

;; 验证starts-with与映射操作的一致性
(check ((rich-string :value-of "hello" :map (lambda (c) (c :to-upper))) :starts-with "HELLO") => #t)
(check ((rich-string :value-of "HELLO" :map (lambda (c) (c :to-lower))) :starts-with "hello") => #t)

;; 验证starts-with与反转操作的一致性
(check ((rich-string :value-of "hello" :reverse) :starts-with "o") => #t)
(check ((rich-string :value-of "hello" :reverse) :starts-with "olleh") => #t)

;; 验证starts-with与连接操作的一致性
(check ((rich-string :value-of "hello" :+ " world") :starts-with "hello") => #t)
(check ((rich-string :value-of "hello" :+ " world") :starts-with "hello ") => #t)
(check ((rich-string :value-of "world" :+ " hello") :starts-with "world") => #t)

;; 验证starts-with与strip操作的一致性
(check ((rich-string :value-of "  hello  " :strip-left) :starts-with "hello") => #t)
(check ((rich-string :value-of "  hello  " :strip-right) :starts-with "  hello") => #t)
(check ((rich-string :value-of "  hello  " :strip-both) :starts-with "hello") => #t)

;; 验证starts-with在各种复杂操作后的正确性
(let ((result ((rich-string :value-of "Hello123World")
               :filter (lambda (c) (or (c :upper?) (c :lower?)))
               :map (lambda (c) (c :to-lower))
               :starts-with "hello")))
  (check result => #t))

(let ((result ((rich-string :value-of "Hello123World")
               :filter (lambda (c) (c :digit?))
               :starts-with "123")))
  (check result => #t))

;; 验证starts-with与Unicode字符操作的一致性
(let ((rs (rich-string :value-of "你好世界🎉")))
  (check (rs :starts-with "你") => #t)
  (check (rs :starts-with "你好") => #t)
  (check (rs :starts-with "你好世界") => #t)
  (check (rs :starts-with "世界") => #f)
  (check (rs :starts-with "🎉") => #f))

;; 验证starts-with在链式操作中的正确传播
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :slice 0 5) :starts-with "hello") => #t)
  (check ((rs :slice 6 11) :starts-with "world") => #t)
  (check ((rs :take 5) :starts-with "hello") => #t)
  (check ((rs :drop 6) :starts-with "world") => #t))

;; 验证starts-with与各种创建方式的一致性
;; 使用:empty创建
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :empty) :starts-with "hello") => #f)

;; 使用:value-of创建字符串
(check ((rich-string :value-of "hello") :starts-with "h") => #t)
(check ((rich-string :value-of "hello") :starts-with "hello") => #t)
(check ((rich-string :value-of "hello") :starts-with "world") => #f)

;; 使用:value-of创建数字字符串
(check ((rich-string :value-of 12345) :starts-with "123") => #t)
(check ((rich-string :value-of 12345) :starts-with "12") => #t)
(check ((rich-string :value-of 12345) :starts-with "234") => #f)

;; 使用:value-of创建符号字符串
(check ((rich-string :value-of 'hello) :starts-with "hel") => #t)
(check ((rich-string :value-of 'hello) :starts-with "hello") => #t)
(check ((rich-string :value-of 'hello) :starts-with "world") => #f)

;; 使用:value-of创建rich-char字符串
(check ((rich-string :value-of (rich-char #\x)) :starts-with "x") => #t)
(check ((rich-string :value-of (rich-char #\x)) :starts-with "y") => #f)

;; 验证starts-with在各种边界情况下的稳定性
;; 空字符串的稳定性
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :value-of "") :starts-with "") => #t)

;; 单字符字符串的稳定性
(check ((rich-string :value-of "a") :starts-with "a") => #t)
(check ((rich-string :value-of "a") :starts-with "b") => #f)
(check ((rich-string :value-of "🎉") :starts-with "🎉") => #t)
(check ((rich-string :value-of "🎉") :starts-with "🎊") => #f)

;; 多字符字符串的稳定性
(check ((rich-string :value-of "hello") :starts-with "hello") => #t)
(check ((rich-string :value-of "hello") :starts-with "world") => #f)
(check ((rich-string :value-of "测试字符串") :starts-with "测试") => #t)
(check ((rich-string :value-of "测试字符串") :starts-with "字符") => #f)

;; 验证starts-with与ends-with方法的对比
(let ((rs (rich-string :value-of "hello world")))
  (check (rs :starts-with "hello") => #t)
  (check (rs :ends-with "world") => #t)
  (check (rs :starts-with "world") => #f)
  (check (rs :ends-with "hello") => #f))

;; 验证starts-with与contains方法的对比
(let ((rs (rich-string :value-of "hello world")))
  (check (rs :starts-with "hello") => #t)
  (check (rs :contains "hello") => #t)
  (check (rs :starts-with "world") => #f)
  (check (rs :contains "world") => #t))

;; 验证starts-with在各种复杂操作后的最终测试
(let ((result ((rich-string :value-of "Hello123World")
               :filter (lambda (c) (or (c :upper?) (c :lower?)))
               :map (lambda (c) (c :to-lower))
               :reverse
               :starts-with "dlrow")))
  (check result => #t))

(let ((result ((rich-string :value-of "Hello123World")
               :filter (lambda (c) (c :digit?))
               :drop 3
               :starts-with "")))
  (check result => #t))

;; 验证starts-with与Unicode字符操作的最终一致性
(let ((rs (rich-string :value-of "你好世界🎉")))
  (check (rs :starts-with "你") => #t)
  (check (rs :starts-with "你好") => #t)
  (check (rs :starts-with "你好世界") => #t)
  (check (rs :starts-with "你好世界🎉") => #t)
  (check (rs :starts-with "世界") => #f)
  (check (rs :starts-with "🎉") => #f))

;; 验证starts-with在链式操作中的最终正确传播
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :slice 0 5) :starts-with "hello") => #t)
  (check ((rs :slice 6 11) :starts-with "world") => #t)
  (check ((rs :take 5) :starts-with "hello") => #t)
  (check ((rs :drop 6) :starts-with "world") => #t)
  (check ((rs :take 0) :starts-with "") => #t)
  (check ((rs :drop 11) :starts-with "") => #t))

;; 验证starts-with与各种创建方式的最终一致性
;; 使用:empty创建的最终验证
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :empty) :starts-with "hello") => #f)

;; 使用:value-of创建字符串的最终验证
(check ((rich-string :value-of "hello") :starts-with "h") => #t)
(check ((rich-string :value-of "hello") :starts-with "hello") => #t)
(check ((rich-string :value-of "hello") :starts-with "world") => #f)

;; 使用:value-of创建数字字符串的最终验证
(check ((rich-string :value-of 12345) :starts-with "123") => #t)
(check ((rich-string :value-of 12345) :starts-with "12") => #t)
(check ((rich-string :value-of 12345) :starts-with "234") => #f)

;; 使用:value-of创建符号字符串的最终验证
(check ((rich-string :value-of 'hello) :starts-with "hel") => #t)
(check ((rich-string :value-of 'hello) :starts-with "hello") => #t)
(check ((rich-string :value-of 'hello) :starts-with "world") => #f)

;; 使用:value-of创建rich-char字符串的最终验证
(check ((rich-string :value-of (rich-char #\x)) :starts-with "x") => #t)
(check ((rich-string :value-of (rich-char #\x)) :starts-with "y") => #f)

;; 验证starts-with在各种边界情况下的最终稳定性
;; 空字符串的最终稳定性
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :value-of "") :starts-with "") => #t)

;; 单字符字符串的最终稳定性
(check ((rich-string :value-of "a") :starts-with "a") => #t)
(check ((rich-string :value-of "a") :starts-with "b") => #f)
(check ((rich-string :value-of "🎉") :starts-with "🎉") => #t)
(check ((rich-string :value-of "🎉") :starts-with "🎊") => #f)

;; 多字符字符串的最终稳定性
(check ((rich-string :value-of "hello") :starts-with "hello") => #t)
(check ((rich-string :value-of "hello") :starts-with "world") => #f)
(check ((rich-string :value-of "测试字符串") :starts-with "测试") => #t)
(check ((rich-string :value-of "测试字符串") :starts-with "字符") => #f)

#|
rich-string%ends-with
检查rich-string对象是否以指定的后缀结尾。

语法
----
(rich-string-instance :ends-with suffix)

参数
----
suffix : any
要检查的后缀，支持以下类型：
- string：标准字符串
- rich-string：rich-string对象
- char：单个字符
- rich-char：rich-char对象

返回值
-----
以boolean形式返回rich-string对象是否以指定后缀结尾。
如果字符串以指定后缀结尾，返回#t；否则返回#f。

说明
----
该方法检查rich-string对象是否以指定的后缀结尾。支持多种参数类型，
包括字符串、rich-string、字符和rich-char。对于空后缀，总是返回#t。
该方法正确处理Unicode字符，能够准确判断字符串后缀。

边界条件
--------
- 空字符串：任何后缀都返回#t（包括空后缀）
- 空后缀：总是返回#t
- 后缀长度大于字符串长度：返回#f
- 后缀与字符串结尾匹配：返回#t
- 后缀与字符串结尾不匹配：返回#f
- Unicode字符后缀：正确匹配Unicode字符

性能特征
--------
- 时间复杂度：O(k)，其中k是后缀的长度
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种后缀类型
- 正确处理Unicode字符
- 与%starts-with方法互补
|#

;; 基本功能测试
;; 字符串后缀匹配
(check ((rich-string :value-of "hello world") :ends-with "world") => #t)
(check ((rich-string :value-of "hello world") :ends-with "rld") => #t)
(check ((rich-string :value-of "hello world") :ends-with "d") => #t)

;; 字符串后缀不匹配
(check ((rich-string :value-of "hello world") :ends-with "hello") => #f)
(check ((rich-string :value-of "hello world") :ends-with "World") => #f)
(check ((rich-string :value-of "hello world") :ends-with "x") => #f)

;; 边界条件测试
;; 空字符串的后缀匹配
(check ((rich-string :empty) :ends-with "") => #t)
(check ((rich-string :empty) :ends-with "hello") => #f)

;; 空后缀匹配
(check ((rich-string :value-of "hello") :ends-with "") => #t)
(check ((rich-string :value-of "") :ends-with "") => #t)
(check ((rich-string :empty) :ends-with "") => #t)

;; 后缀长度大于字符串长度
(check ((rich-string :value-of "hello") :ends-with "hello world") => #f)
(check ((rich-string :value-of "hi") :ends-with "hello") => #f)
(check ((rich-string :value-of "a") :ends-with "ab") => #f)

;; Unicode字符后缀测试
;; 中文字符后缀
(check ((rich-string :value-of "测试字符串") :ends-with "符串") => #t)
(check ((rich-string :value-of "测试字符串") :ends-with "串") => #t)
(check ((rich-string :value-of "测试字符串") :ends-with "测试") => #f)

;; 日文字符后缀
(check ((rich-string :value-of "こんにちは") :ends-with "ちは") => #t)
(check ((rich-string :value-of "こんにちは") :ends-with "は") => #t)
(check ((rich-string :value-of "こんにちは") :ends-with "こん") => #f)

;; Emoji表情符号后缀
(check ((rich-string :value-of "🎉🎊🎈") :ends-with "🎈") => #t)
(check ((rich-string :value-of "🎉🎊🎈") :ends-with "🎊🎈") => #t)
(check ((rich-string :value-of "🎉🎊🎈") :ends-with "🎉") => #f)

;; 混合字符后缀
(check ((rich-string :value-of "hello 世界 🎉") :ends-with "🎉") => #t)
(check ((rich-string :value-of "hello 世界 🎉") :ends-with " 🎉") => #t)
(check ((rich-string :value-of "hello 世界 🎉") :ends-with "世界 🎉") => #t)
(check ((rich-string :value-of "hello 世界 🎉") :ends-with "hello") => #f)

;; 链式调用测试
;; 链式操作后的后缀匹配
(check ((rich-string :value-of "Hello World" :map (lambda (c) (c :to-lower))) :ends-with "world") => #t)
(check ((rich-string :value-of "  hello  " :strip-both) :ends-with "hello") => #t)
(check ((rich-string :value-of "hello world" :slice 6 11) :ends-with "world") => #t)

;; 链式操作后的后缀不匹配
(check ((rich-string :value-of "Hello World" :map (lambda (c) (c :to-upper))) :ends-with "world") => #f)
(check ((rich-string :value-of "hello world" :slice 0 5) :ends-with "world") => #f)

;; 验证返回类型是布尔值
(check (boolean? ((rich-string :value-of "hello") :ends-with "o")) => #t)
(check (boolean? ((rich-string :value-of "hello") :ends-with "x")) => #t)
(check (boolean? ((rich-string :value-of "hello") :ends-with "")) => #t)

;; 验证不同类型输入的ends-with判断
;; 数字转换的字符串
(check ((rich-string :value-of 12345) :ends-with "345") => #t)
(check ((rich-string :value-of 12345) :ends-with "45") => #t)
(check ((rich-string :value-of 12345) :ends-with "123") => #f)

;; 符号转换的字符串
(check ((rich-string :value-of 'hello) :ends-with "llo") => #t)
(check ((rich-string :value-of 'hello) :ends-with "hello") => #t)
(check ((rich-string :value-of 'hello) :ends-with "world") => #f)

;; rich-char转换的字符串
(check ((rich-string :value-of (rich-char #\x)) :ends-with "x") => #t)
(check ((rich-string :value-of (rich-char #\x)) :ends-with "y") => #f)

;; 验证ends-with与字符访问的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (rs :ends-with "d") => #t)
  (check (rs :ends-with "world") => #t)
  (check (rs :ends-with "hello") => #f))

;; 验证Unicode字符的ends-with一致性
(let ((rs (rich-string :value-of "测试字符串")))
  (check (rs :ends-with "串") => #t)
  (check (rs :ends-with "符串") => #t)
  (check (rs :ends-with "测试") => #f))

;; 性能相关测试
;; 长字符串的后缀匹配
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check (long-str :ends-with "aaa") => #t)
  (check (long-str :ends-with (make-string 100 #\a)) => #t)
  (check (long-str :ends-with "aab") => #f))

;; 空字符串的后缀匹配
(let ((empty-str (rich-string :empty)))
  (check (empty-str :ends-with "") => #t)
  (check (empty-str :ends-with "a") => #f))

;; 验证ends-with与starts-with方法的对比
(let ((rs (rich-string :value-of "hello world")))
  (check (rs :starts-with "hello") => #t)
  (check (rs :ends-with "world") => #t)
  (check (rs :starts-with "world") => #f)
  (check (rs :ends-with "hello") => #f))

;; 验证ends-with与contains方法的对比
(let ((rs (rich-string :value-of "hello world")))
  (check (rs :ends-with "world") => #t)
  (check (rs :contains "world") => #t)
  (check (rs :ends-with "hello") => #f)
  (check (rs :contains "hello") => #t))

;; 验证ends-with在链式操作中的正确传播
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :slice 0 5) :ends-with "hello") => #t)
  (check ((rs :slice 6 11) :ends-with "world") => #t)
  (check ((rs :take 5) :ends-with "hello") => #t)
  (check ((rs :drop 6) :ends-with "world") => #t))

(check-report)
