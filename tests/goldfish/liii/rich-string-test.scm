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

;; 验证类型正确性
(check-true (rich-string :is-type-of (rich-string :empty)))
(check-false (rich-string :is-type-of ""))

;; 链式调用测试
(check ((rich-string :empty :+ "hello") :get) => "hello")
(check ((rich-string :empty :strip-both) :get) => "")

;; 验证空字符串与其他方法的兼容性
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :empty) :ends-with "") => #t)
(check ((rich-string :empty) :contains "") => #t)

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
;; 数字类型
(check ((rich-string :value-of 123) :get) => "123")
;; 符号类型
(check ((rich-string :value-of 'hello) :get) => "hello")
;; 字符串类型
(check ((rich-string :value-of "hello") :get) => "hello")
(check ((rich-string :value-of "测试") :get) => "测试")
;; rich-char类型
(check ((rich-string :value-of (rich-char #\x)) :get) => "x")

;; 边界条件测试
(check ((rich-string :value-of "") :length) => 0)

;; 链式调用测试
(check ((rich-string :value-of "hello" :+ " world") :get) => "hello world")

;; 类型验证
(check-true (rich-string :is-type-of (rich-string :value-of "hello")))

;; 错误处理测试
(check-catch 'type-error (rich-string :value-of #t))

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
;; 多字符字符串
(check ((rich-string :value-of "hello") :get) => "hello")
;; Unicode字符串
(check ((rich-string :value-of "测试") :get) => "测试")
(check ((rich-string :value-of "🎉") :get) => "🎉")
;; 数字转换的字符串
(check ((rich-string :value-of 123) :get) => "123")
;; 符号转换的字符串
(check ((rich-string :value-of 'hello) :get) => "hello")

;; 边界条件测试
(check (string? ((rich-string :value-of "hello") :get)) => #t)
(check (string-length ((rich-string :value-of "hello") :get)) => 5)

;; 验证内容一致性
(check (equal? ((rich-string :value-of "hello") :get) "hello") => #t)

;; 链式操作后获取结果
(check ((rich-string :value-of "hello" :+ " world") :get) => "hello world")
(check ((rich-string :value-of "  hello  " :strip-both) :get) => "hello")

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
;; 多字符ASCII字符串
(check ((rich-string :value-of "hello") :length) => 5)
;; Unicode字符测试
(check ((rich-string :value-of "测试") :length) => 2)
(check ((rich-string :value-of "🎉🎊") :length) => 2)
;; 混合字符
(check ((rich-string :value-of "hello 世界 🎉") :length) => 10)

;; 边界条件测试
(check ((rich-string :value-of "") :length) => 0)
(check ((rich-string :value-of "中") :length) => 1)

;; 验证长度一致性
(check ((rich-string :value-of "hello") :length) => (string-length "hello"))

;; 链式操作后长度验证
(check ((rich-string :value-of "hello" :+ " world") :length) => 11)

;; 验证不同类型输入的字符数量
(check ((rich-string :value-of 123) :length) => 3)
(check ((rich-string :value-of 'hello) :length) => 5)

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
(check (((rich-string :value-of "hello") :char-at 4) :make-string) => "o")
;; Unicode字符访问测试
(check (((rich-string :value-of "测试") :char-at 0) :make-string) => "测")
(check (((rich-string :value-of "🎉🎊") :char-at 1) :make-string) => "🎊")

;; 边界条件测试
(check-catch 'out-of-range ((rich-string :empty) :char-at 0))
(check-catch 'out-of-range ((rich-string :value-of "hello") :char-at -1))
(check-catch 'out-of-range ((rich-string :value-of "hello") :char-at 5))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :char-at 0)))

;; 链式操作测试
(check ((((rich-string :value-of "hello") :char-at 0) :to-upper) :make-string) => "H")

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :char-at 0) :make-string) => "h"))

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
(check (((rich-string :value-of "hello") :apply 4) :make-string) => "o")
;; Unicode字符访问测试
(check (((rich-string :value-of "测试") :apply 0) :make-string) => "测")

;; 边界条件测试
(check-catch 'out-of-range ((rich-string :empty) :apply 0))
(check-catch 'out-of-range ((rich-string :value-of "hello") :apply -1))
(check-catch 'out-of-range ((rich-string :value-of "hello") :apply 5))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :apply 0)))

;; 链式操作测试
(check ((((rich-string :value-of "hello") :apply 0) :to-upper) :make-string) => "H")

;; 验证apply与char-at方法的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :apply 0) :make-string) => ((rs :char-at 0) :make-string)))

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
(check ((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get) :make-string) => "h")
;; 查找大写字母
(check ((((rich-string :value-of "Hello") :find (lambda (c) (c :upper?))) :get) :make-string) => "H")
;; 查找数字字符
(check ((((rich-string :value-of "abc123") :find (lambda (c) (c :digit?))) :get) :make-string) => "1")

;; 边界条件测试
(check-true (((rich-string :empty) :find (lambda (c) #t)) :empty?))
(check-true (((rich-string :value-of "hello") :find (lambda (c) (c :equals #\x))) :empty?))
(check ((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\l))) :get) :make-string) => "l")

;; Unicode字符查找测试
(check ((((rich-string :value-of "hello世界") :find (lambda (c) (string=? (c :make-string) "世"))) :get) :make-string) => "世")
(check ((((rich-string :value-of "hello🎉world") :find (lambda (c) (string=? (c :make-string) "🎉"))) :get) :make-string) => "🎉")

;; 复杂谓词测试
(check ((((rich-string :value-of "hello World") :find (lambda (c) (c :upper?))) :get) :make-string) => "W")

;; 验证返回类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h)))))
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get)))

;; 链式操作测试
(check (((((rich-string :value-of "hello") :find (lambda (c) (c :equals #\h))) :get) :to-upper) :make-string) => "H")

;; 验证查找结果与字符访问的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :find (lambda (c) (c :equals #\w))) :get) :make-string) => "w"))

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
(check ((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get) :make-string) => "o")
;; 查找大写字母（从后向前）
(check ((((rich-string :value-of "HeLLo") :find-last (lambda (c) (c :upper?))) :get) :make-string) => "L")
;; 查找数字字符（从后向前）
(check ((((rich-string :value-of "abc123") :find-last (lambda (c) (c :digit?))) :get) :make-string) => "3")

;; 边界条件测试
(check-true (((rich-string :empty) :find-last (lambda (c) #t)) :empty?))
(check-true (((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\x))) :empty?))
(check ((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\l))) :get) :make-string) => "l")

;; Unicode字符查找测试
(check ((((rich-string :value-of "hello世界") :find-last (lambda (c) (string=? (c :make-string) "界"))) :get) :make-string) => "界")
(check ((((rich-string :value-of "hello🎉world🎊") :find-last (lambda (c) (string=? (c :make-string) "🎊"))) :get) :make-string) => "🎊")

;; 复杂谓词测试
(check ((((rich-string :value-of "Hello World") :find-last (lambda (c) (c :upper?))) :get) :make-string) => "W")

;; 验证返回类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o)))))
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get)))

;; 链式操作测试
(check (((((rich-string :value-of "hello") :find-last (lambda (c) (c :equals #\o))) :get) :to-upper) :make-string) => "O")

;; 与find方法的对比测试
(let ((rs (rich-string :value-of "HeLLo")))
  (check (((rs :find (lambda (c) (c :upper?))) :get) :make-string) => "H")
  (check (((rs :find-last (lambda (c) (c :upper?))) :get) :make-string) => "L"))

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
;; 单字符字符串
(check (((rich-string :value-of "a") :head) :make-string) => "a")
;; Unicode字符测试
(check (((rich-string :value-of "测试") :head) :make-string) => "测")
(check (((rich-string :value-of "🎉") :head) :make-string) => "🎉")
;; 混合字符
(check (((rich-string :value-of "hello 世界 🎉") :head) :make-string) => "h")

;; 边界条件测试
(check-catch 'index-error ((rich-string :empty) :head))

;; 验证返回类型是rich-char
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :head)))

;; 链式操作测试
(check ((((rich-string :value-of "hello") :head) :to-upper) :make-string) => "H")
(check-true (((rich-string :value-of "hello") :head) :equals #\h))

;; 验证字符访问与字符串内容的一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :head) :make-string) => "h"))

;; 验证不同类型输入的字符访问
(check (((rich-string :value-of 123) :head) :make-string) => "1")

;; 验证head与char-at方法的一致性
(let ((rs (rich-string :value-of "hello world")))
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
(check ((((rich-string :value-of "hello") :head-option) :get) :make-string) => "h")
(check ((((rich-string :value-of "a") :head-option) :get) :make-string) => "a")

;; Unicode字符测试
(check ((((rich-string :value-of "测试") :head-option) :get) :make-string) => "测")
(check ((((rich-string :value-of "🎉🎊") :head-option) :get) :make-string) => "🎉")

;; 边界条件测试
(check-true (((rich-string :empty) :head-option) :empty?))
(check-true (((rich-string :value-of "") :head-option) :empty?))

;; 验证返回类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :head-option)))
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :head-option) :get)))

;; 链式操作测试
(check (((((rich-string :value-of "hello") :head-option) :get) :to-upper) :make-string) => "H")
(check (((((rich-string :value-of "hello") :head-option) :map (lambda (c) (c :to-upper))) :get) :make-string) => "H")

;; 与其他方法的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :head) :make-string) => (((rs :head-option) :get) :make-string)))

;; option类型操作测试
(check ((((rich-string :value-of "hello") :head-option) :get-or-else (rich-char #\x)) :make-string) => "h")
(check ((((rich-string :empty) :head-option) :get-or-else (rich-char #\x)) :make-string) => "x")

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
(check (((rich-string :value-of "hello") :last) :make-string) => "o")
(check (((rich-string :value-of "a") :last) :make-string) => "a")

;; Unicode字符测试
(check (((rich-string :value-of "测试") :last) :make-string) => "试")
(check (((rich-string :value-of "🎉🎊") :last) :make-string) => "🎊")

;; 边界条件测试
(check-catch 'index-error ((rich-string :empty) :last))

;; 验证返回类型
(check-true (rich-char :is-type-of ((rich-string :value-of "hello") :last)))

;; 链式操作测试
(check ((((rich-string :value-of "hello") :last) :to-upper) :make-string) => "O")

;; 与其他方法的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :last) :make-string) => ((rs :char-at (- (rs :length) 1)) :make-string)))

;; 验证字符访问一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :last) :make-string) => "d"))

;; 不同类型输入测试
(check (((rich-string :value-of 123) :last) :make-string) => "3")
(check (((rich-string :value-of 'hello) :last) :make-string) => "o")

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
(check ((((rich-string :value-of "hello") :last-option) :get) :make-string) => "o")
(check ((((rich-string :value-of "a") :last-option) :get) :make-string) => "a")

;; Unicode字符测试
(check ((((rich-string :value-of "测试") :last-option) :get) :make-string) => "试")
(check ((((rich-string :value-of "🎉🎊") :last-option) :get) :make-string) => "🎊")

;; 边界条件测试
(check-true (((rich-string :empty) :last-option) :empty?))

;; 验证返回类型
(check-true (option :is-type-of ((rich-string :value-of "hello") :last-option)))
(check-true (rich-char :is-type-of (((rich-string :value-of "hello") :last-option) :get)))

;; 链式操作测试
(check (((((rich-string :value-of "hello") :last-option) :get) :to-upper) :make-string) => "O")
(check (((((rich-string :value-of "hello") :last-option) :map (lambda (c) (c :to-upper))) :get) :make-string) => "O")

;; 与其他方法的对比测试
(let ((rs (rich-string :value-of "hello")))
  (check ((rs :last) :make-string) => (((rs :last-option) :get) :make-string)))

;; option类型操作测试
(check ((((rich-string :value-of "hello") :last-option) :get-or-else (rich-char #\x)) :make-string) => "o")
(check ((((rich-string :empty) :last-option) :get-or-else (rich-char #\x)) :make-string) => "x")

;; 验证字符访问一致性
(let ((rs (rich-string :value-of "hello world")))
  (check (((rs :last-option) :get) :make-string) => "d"))

;; 不同类型输入测试
(check ((((rich-string :value-of 123) :last-option) :get) :make-string) => "3")

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
(check (((rich-string :value-of "hello world") :slice 0 5) :get) => "hello")
(check (((rich-string :value-of "hello world") :slice 6 11) :get) => "world")

;; Unicode字符切片测试
(check (((rich-string :value-of "测试字符串") :slice 0 2) :get) => "测试")
(check (((rich-string :value-of "🎉🎊🎈") :slice 0 1) :get) => "🎉")

;; 边界条件测试
(check (((rich-string :value-of "hello") :slice 0 3) :get) => "hel")
(check (((rich-string :value-of "hello") :slice 2 5) :get) => "llo")
(check (((rich-string :value-of "hello") :slice 3 2) :get) => "")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :slice 0 5 :+ "!") :get) => "hello!")
(check (((rich-string :value-of "Hello World") :slice 0 5 :map (lambda (c) (c :to-lower))) :get) => "hello")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :slice 0 3)))
(check (((rich-string :value-of "hello") :slice 0 3) :length) => 3)

;; 验证切片内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :slice 0 5) :get) => "hello")
  (check ((rs :slice 6 11) :get) => "world"))

;; 边界条件验证
(check (((rich-string :empty) :slice 0 0) :get) => "")
(let ((rs (rich-string :value-of "a")))
  (check ((rs :slice 0 1) :get) => "a")
  (check ((rs :slice 0 0) :get) => ""))

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
(check (((rich-string :value-of "hello world") :take 5) :get) => "hello")
(check (((rich-string :value-of "hello") :take 5) :get) => "hello")

;; Unicode字符提取测试
(check (((rich-string :value-of "测试字符串") :take 2) :get) => "测试")
(check (((rich-string :value-of "🎉🎊🎈") :take 1) :get) => "🎉")

;; 边界条件测试
(check (((rich-string :value-of "hello") :take 0) :get) => "")
(check (((rich-string :value-of "hello") :take -1) :get) => "")
(check (((rich-string :value-of "hello") :take 10) :get) => "hello")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :take 5 :+ "!") :get) => "hello!")
(check (((rich-string :value-of "Hello World") :take 5 :map (lambda (c) (c :to-lower))) :get) => "hello")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :take 3)))
(check (((rich-string :value-of "hello") :take 3) :length) => 3)

;; 验证提取内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => "hello")
  (check ((rs :take 3) :get) => "hel"))

;; 边界条件验证
(let ((rs (rich-string :value-of "a")))
  (check ((rs :take 1) :get) => "a")
  (check ((rs :take 0) :get) => ""))

;; 与slice方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => ((rs :slice 0 5) :get)))

;; 不同类型输入测试
(check (((rich-string :value-of 12345) :take 3) :get) => "123")
(check (((rich-string :value-of 'hello) :take 3) :get) => "hel")

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
(check (((rich-string :value-of "hello world") :take-right 5) :get) => "world")
(check (((rich-string :value-of "hello") :take-right 5) :get) => "hello")

;; Unicode字符提取测试
(check (((rich-string :value-of "测试字符串") :take-right 2) :get) => "符串")
(check (((rich-string :value-of "🎉🎊🎈") :take-right 1) :get) => "🎈")

;; 边界条件测试
(check (((rich-string :value-of "hello") :take-right 0) :get) => "")
(check (((rich-string :value-of "hello") :take-right -1) :get) => "")
(check (((rich-string :value-of "hello") :take-right 10) :get) => "hello")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :take-right 5 :+ "!") :get) => "world!")
(check (((rich-string :value-of "Hello World") :take-right 5 :map (lambda (c) (c :to-upper))) :get) => "WORLD")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :take-right 3)))
(check (((rich-string :value-of "hello") :take-right 3) :length) => 3)

;; 验证提取内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => "world")
  (check ((rs :take-right 3) :get) => "rld"))

;; 边界条件验证
(let ((rs (rich-string :value-of "a")))
  (check ((rs :take-right 1) :get) => "a")
  (check ((rs :take-right 0) :get) => ""))

;; 与slice方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => ((rs :slice 6 11) :get)))

;; 与take方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => "hello")
  (check ((rs :take-right 5) :get) => "world"))

;; 不同类型输入测试
(check (((rich-string :value-of 12345) :take-right 3) :get) => "345")
(check (((rich-string :value-of 'hello) :take-right 3) :get) => "llo")

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
(check (((rich-string :value-of "hello world") :drop 6) :get) => "world")
(check (((rich-string :value-of "hello") :drop 5) :get) => "")

;; Unicode字符删除测试
(check (((rich-string :value-of "测试字符串") :drop 2) :get) => "字符串")
(check (((rich-string :value-of "🎉🎊🎈") :drop 1) :get) => "🎊🎈")

;; 边界条件测试
(check (((rich-string :value-of "hello") :drop 0) :get) => "hello")
(check (((rich-string :value-of "hello") :drop -1) :get) => "hello")
(check (((rich-string :value-of "hello") :drop 10) :get) => "")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :drop 6 :+ "!") :get) => "world!")
(check (((rich-string :value-of "Hello World") :drop 6 :map (lambda (c) (c :to-upper))) :get) => "WORLD")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :drop 3)))
(check (((rich-string :value-of "hello") :drop 3) :length) => 2)

;; 验证删除内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 6) :get) => "world")
  (check ((rs :drop 3) :get) => "lo world"))

;; 边界条件验证
(let ((rs (rich-string :value-of "a")))
  (check ((rs :drop 1) :get) => "")
  (check ((rs :drop 0) :get) => "a"))

;; 与slice方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 6) :get) => ((rs :slice 6 11) :get)))

;; 与take方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take 5) :get) => "hello")
  (check ((rs :drop 5) :get) => " world"))

;; 不同类型输入测试
(check (((rich-string :value-of 12345) :drop 3) :get) => "45")
(check (((rich-string :value-of 'hello) :drop 3) :get) => "lo")

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
(check (((rich-string :value-of "hello world") :drop-right 5) :get) => "hello ")
(check (((rich-string :value-of "hello") :drop-right 5) :get) => "")

;; Unicode字符删除测试
(check (((rich-string :value-of "测试字符串") :drop-right 2) :get) => "测试字")
(check (((rich-string :value-of "🎉🎊🎈") :drop-right 1) :get) => "🎉🎊")

;; 边界条件测试
(check (((rich-string :value-of "hello") :drop-right 0) :get) => "hello")
(check (((rich-string :value-of "hello") :drop-right -1) :get) => "hello")
(check (((rich-string :value-of "hello") :drop-right 10) :get) => "")

;; 链式调用测试
(check (((rich-string :value-of "hello world") :drop-right 5 :+ "!") :get) => "hello !")
(check (((rich-string :value-of "Hello World") :drop-right 5 :map (lambda (c) (c :to-upper))) :get) => "HELLO ")

;; 验证返回类型和长度
(check-true (rich-string :is-type-of ((rich-string :value-of "hello") :drop-right 3)))
(check (((rich-string :value-of "hello") :drop-right 3) :length) => 2)

;; 验证删除内容一致性
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop-right 5) :get) => "hello ")
  (check ((rs :drop-right 3) :get) => "hello wo"))

;; 边界条件验证
(let ((rs (rich-string :value-of "a")))
  (check ((rs :drop-right 1) :get) => "")
  (check ((rs :drop-right 0) :get) => "a"))

;; 与slice方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop-right 5) :get) => ((rs :slice 0 6) :get)))

;; 与take-right方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :take-right 5) :get) => "world")
  (check ((rs :drop-right 5) :get) => "hello "))

;; 与drop方法的对比测试
(let ((rs (rich-string :value-of "hello world")))
  (check ((rs :drop 5) :get) => " world")
  (check ((rs :drop-right 5) :get) => "hello "))

;; 不同类型输入测试
(check (((rich-string :value-of 12345) :drop-right 3) :get) => "12")
(check (((rich-string :value-of 'hello) :drop-right 3) :get) => "he")

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

;; 多字符字符串
(check ((rich-string :value-of "hello") :empty?) => #f)

;; Unicode字符测试
(check ((rich-string :value-of "测试") :empty?) => #f)

;; 链式操作后的空字符串判断
(check ((rich-string :value-of "hello" :slice 0 0) :empty?) => #t)

;; 验证返回类型是布尔值
(check (boolean? ((rich-string :empty) :empty?)) => #t)

;; 验证empty?与length方法的一致性
(check ((rich-string :empty) :empty?) => (zero? ((rich-string :empty) :length)))

;; 验证empty?与字符访问的一致性
(let ((rs (rich-string :value-of "hello")))
  (check (rs :empty?) => #f)
  ;; 删除所有字符后应该为空
  (check ((rs :drop 5) :empty?) => #t))

;; 性能相关测试
(let ((long-str (rich-string :value-of (make-string 1000 #\a))))
  (check (long-str :empty?) => #f))

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

;; 基本功能测试 - 保留10个核心测试用例
;; 字符串前缀匹配
(check ((rich-string :value-of "hello world") :starts-with "hello") => #t)
(check ((rich-string :value-of "hello world") :starts-with "h") => #t)

;; 字符串前缀不匹配
(check ((rich-string :value-of "hello world") :starts-with "world") => #f)
(check ((rich-string :value-of "hello world") :starts-with "Hello") => #f)

;; 边界条件测试
;; 空字符串的前缀匹配
(check ((rich-string :empty) :starts-with "") => #t)
(check ((rich-string :empty) :starts-with "hello") => #f)

;; 空前缀匹配
(check ((rich-string :value-of "hello") :starts-with "") => #t)

;; 前缀长度大于字符串长度
(check ((rich-string :value-of "hello") :starts-with "hello world") => #f)

;; Unicode字符前缀测试
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

;; 基本功能测试 - 保留10个核心测试用例
;; 字符串后缀匹配
(check ((rich-string :value-of "hello world") :ends-with "world") => #t)
(check ((rich-string :value-of "hello world") :ends-with "d") => #t)

;; 字符串后缀不匹配
(check ((rich-string :value-of "hello world") :ends-with "hello") => #f)
(check ((rich-string :value-of "hello world") :ends-with "World") => #f)

;; 边界条件测试
;; 空字符串的后缀匹配
(check ((rich-string :empty) :ends-with "") => #t)
(check ((rich-string :empty) :ends-with "hello") => #f)

;; 空后缀匹配
(check ((rich-string :value-of "hello") :ends-with "") => #t)

;; 后缀长度大于字符串长度
(check ((rich-string :value-of "hello") :ends-with "hello world") => #f)

;; Unicode字符后缀测试
(check ((rich-string :value-of "测试字符串") :ends-with "符串") => #t)
(check ((rich-string :value-of "测试字符串") :ends-with "测试") => #f)

(check-report)
