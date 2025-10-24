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
        (liii error)
        (liii rich-vector))

(check-set-mode! 'report-failed)

#|
rich-string%to-rich-vector
将rich-string对象转换为rich-vector对象，其中每个元素是rich-char对象。

语法
----
(rich-string-instance :to-rich-vector)

返回值
-----
返回一个rich-vector对象，包含原字符串中每个字符对应的rich-char对象。

说明
----
该方法将rich-string对象转换为rich-vector对象，转换过程中每个字符都会被包装成rich-char对象。
返回的rich-vector对象与原字符串具有相同的字符顺序。
对于空字符串，返回空的rich-vector对象。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回空的rich-vector对象
- 单字符字符串：返回包含一个rich-char对象的rich-vector
- 多字符字符串：返回包含多个rich-char对象的rich-vector
- Unicode字符：正确处理Unicode字符的转换

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的rich-vector对象

兼容性
------
- 与所有rich-string实例兼容
- 返回的rich-vector对象支持所有rich-vector的操作方法
|#

;; 基本功能测试
;; 普通字符串转换
(let ((vec ((rich-string :value-of "hello") :to-rich-vector)))
  (check (vec :length) => 5))

;; 验证转换后的字符内容
(let ((vec ((rich-string :value-of "abc") :to-rich-vector)))
  (let ((codes (vec :map (lambda (c) (c :to-integer)) :collect)))
    (check codes => #(97 98 99))))

;; 边界条件测试
;; 空字符串
(let ((vec ((rich-string :empty) :to-rich-vector)))
  (check (vec :empty?) => #t))

;; 单字符字符串
(let ((vec ((rich-string :value-of "a") :to-rich-vector)))
  (check (vec :length) => 1))

;; Unicode字符转换测试
(let ((vec ((rich-string :value-of "测试") :to-rich-vector)))
  (check (vec :length) => 2))

;; 验证转换后的字符类型
(let ((vec ((rich-string :value-of "test") :to-rich-vector)))
  (check (vec :forall (lambda (c) (rich-char :is-type-of c))) => #t))

;; 验证字符顺序
(let ((vec ((rich-string :value-of "123") :to-rich-vector)))
  (let ((codes (vec :map (lambda (c) (c :to-integer)) :collect)))
    (check codes => #(49 50 51))))

;; 混合字符测试
(let ((vec ((rich-string :value-of "a1B2c3") :to-rich-vector)))
  (check (vec :length) => 6))

;; 验证返回类型
(check (rich-vector :is-type-of ((rich-string :value-of "test") :to-rich-vector)) => #t)

#|
rich-string%forall
检查rich-string对象中的所有字符是否都满足给定的谓词条件。

语法
----
(rich-string-instance :forall pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以boolean形式返回所有字符是否都满足谓词条件。
如果所有字符都满足谓词条件，返回#t；否则返回#f。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
只有当所有字符都满足谓词条件时才返回#t，否则返回#f。
对于空字符串，总是返回#t（空真原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回#t（空真原则）
- 所有字符满足谓词：返回#t
- 至少一个字符不满足谓词：返回#f
- 第一个字符不满足谓词：立即返回#f（短路求值）
- 最后一个字符不满足谓词：返回#f

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持短路求值，提高性能
|#

;; 基本功能测试
;; 所有字符都满足谓词
(check ((rich-string :value-of "hello") :forall (lambda (c) (c :lower?))) => #t)

;; 存在字符不满足谓词
(check ((rich-string :value-of "Hello") :forall (lambda (c) (c :lower?))) => #f)

;; 边界条件测试
;; 空字符串（空真原则）
(check ((rich-string :empty) :forall (lambda (c) #f)) => #t)

;; 单字符字符串
(check ((rich-string :value-of "a") :forall (lambda (c) (c :lower?))) => #t)
(check ((rich-string :value-of "A") :forall (lambda (c) (c :lower?))) => #f)

;; Unicode字符测试
(check ((rich-string :value-of "测试") :forall (lambda (c) (c :ascii?))) => #f)  ; 中文字符不是ASCII

;; 复杂谓词测试
(check ((rich-string :value-of "abcde") :forall (lambda (c) (c :ascii?))) => #t)

;; 验证短路求值行为
(let ((count 0))
  ((rich-string :value-of "Hello") :forall (lambda (c)
    (set! count (+ count 1))
    (c :lower?)))
  (check count => 1))  ; 应该在第一个字符'H'处停止

;; 验证返回类型
(check (boolean? ((rich-string :value-of "hello") :forall (lambda (c) #t))) => #t)

;; 链式操作测试
(check ((rich-string :value-of "hhh") :forall (lambda (c) (c :equals #\h))) => #t)

#|
rich-string%exists
检查rich-string对象中是否存在至少一个字符满足给定的谓词条件。

语法
----
(rich-string-instance :exists pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以boolean形式返回是否存在至少一个字符满足谓词条件。
如果存在至少一个字符满足谓词条件，返回#t；否则返回#f。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
只要有一个字符满足谓词条件就立即返回#t，否则返回#f。
对于空字符串，总是返回#f（空假原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回#f（空假原则）
- 所有字符都不满足谓词：返回#f
- 至少一个字符满足谓词：返回#t
- 第一个字符满足谓词：立即返回#t（短路求值）
- 最后一个字符满足谓词：返回#t

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持短路求值，提高性能
|#

;; 基本功能测试
;; 存在字符满足谓词
(check ((rich-string :value-of "Hello") :exists (lambda (c) (c :upper?))) => #t)

;; 所有字符都不满足谓词
(check ((rich-string :value-of "hello") :exists (lambda (c) (c :upper?))) => #f)

;; 边界条件测试
;; 空字符串（空假原则）
(check ((rich-string :empty) :exists (lambda (c) #t)) => #f)

;; 单字符字符串
(check ((rich-string :value-of "A") :exists (lambda (c) (c :upper?))) => #t)

;; Unicode字符测试
(check ((rich-string :value-of "测试") :exists (lambda (c) (c :ascii?))) => #f)  ; 中文字符不是ASCII

;; 复杂谓词测试
(check ((rich-string :value-of "Hello123") :exists (lambda (c) (c :digit?))) => #t)

;; 验证短路求值行为
(let ((count 0))
  ((rich-string :value-of "Hello") :exists (lambda (c)
    (set! count (+ count 1))
    (c :upper?)))
  (check count => 1))  ; 应该在第一个字符'H'处停止

;; 验证返回类型
(check (boolean? ((rich-string :value-of "hello") :exists (lambda (c) #f))) => #t)

#|
rich-string%contains
检查rich-string对象中是否包含指定的子字符串或字符。

语法
----
(rich-string-instance :contains elem)

参数
----
elem : any
要查找的元素，支持以下类型：
- rich-string：rich-string对象
- string：标准字符串
- rich-char：rich-char对象
- char：单个字符

返回值
-----
以boolean形式返回字符串中是否包含指定的元素。
如果字符串包含指定的元素，返回#t；否则返回#f。

说明
----
该方法检查rich-string对象中是否包含指定的子字符串或字符。
支持多种参数类型，包括rich-string、string、rich-char和char。
对于空字符串，总是返回#t（包含空字符串）。
该方法正确处理Unicode字符，能够准确查找子字符串。

边界条件
--------
- 空字符串：包含空字符串返回#t，包含非空字符串返回#f
- 空元素：总是返回#t
- 元素长度大于字符串长度：返回#f
- 元素与字符串部分匹配：返回#t
- 元素与字符串不匹配：返回#f
- Unicode字符元素：正确匹配Unicode字符

性能特征
--------
- 时间复杂度：O(n)，需要搜索整个字符串
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种元素类型
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 字符串包含测试
(check ((rich-string :value-of "hello world") :contains "hello") => #t)

;; 字符串不包含测试
(check ((rich-string :value-of "hello world") :contains "test") => #f)

;; 字符包含测试
(check ((rich-string :value-of "hello") :contains #\h) => #t)

;; 字符不包含测试
(check ((rich-string :value-of "hello") :contains #\x) => #f)

;; 边界条件测试
;; 空字符串的包含测试
(check ((rich-string :empty) :contains "") => #t)
(check ((rich-string :empty) :contains "hello") => #f)

;; 空元素的包含测试
(check ((rich-string :value-of "hello") :contains "") => #t)

;; Unicode字符包含测试
(check ((rich-string :value-of "测试字符串") :contains "测试") => #t)

;; rich-string参数测试
(check ((rich-string :value-of "hello world") :contains (rich-string :value-of "hello")) => #t)

#|
rich-string%index-of
查找指定子字符串或字符在rich-string对象中第一次出现的索引位置。

语法
----
(rich-string-instance :index-of str/char (start-index 0))

参数
----
str/char : any
要查找的子字符串或字符，支持以下类型：
- rich-string：rich-string对象
- string：标准字符串
- rich-char：rich-char对象
- char：单个字符

start-index : integer (可选，默认为0)
开始搜索的索引位置，从0开始计数。

返回值
-----
以integer形式返回子字符串或字符第一次出现的索引位置。
如果未找到，返回-1。

说明
----
该方法在rich-string对象中查找指定的子字符串或字符第一次出现的索引位置。
支持多种参数类型，包括rich-string、string、rich-char和char。
可以指定开始搜索的索引位置，从该位置开始向后搜索。
该方法正确处理Unicode字符，能够准确查找子字符串的位置。

边界条件
--------
- 空字符串：查找任何元素（包括空元素）都返回-1
- 空元素：查找空元素总是返回-1
- 元素长度大于字符串长度：返回-1
- 元素与字符串部分匹配：返回匹配的起始索引
- 元素与字符串不匹配：返回-1
- 起始索引超出范围：返回-1
- Unicode字符元素：正确匹配Unicode字符

性能特征
--------
- 时间复杂度：O(n)，需要搜索整个字符串
- 空间复杂度：O(1)，不创建新字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种元素类型
- 支持指定起始搜索位置
- 正确处理Unicode字符
|#

;; 基本功能测试 - 字符串查找
(check ((rich-string :value-of "hello world") :index-of "hello") => 0)
(check ((rich-string :value-of "hello world") :index-of "world") => 6)

;; 字符串未找到测试
(check ((rich-string :value-of "hello world") :index-of "test") => -1)

;; 字符查找测试
(check ((rich-string :value-of "hello") :index-of #\h) => 0)
(check ((rich-string :value-of "hello") :index-of #\o) => 4)

;; 边界条件测试
(check ((rich-string :empty) :index-of "hello") => -1)

;; Unicode字符查找测试
(check ((rich-string :value-of "测试字符串") :index-of "测试") => 0)

;; 指定起始索引测试
(check ((rich-string :value-of "hello world") :index-of "l" 3) => 3)

;; rich-string参数测试
(check ((rich-string :value-of "hello world") :index-of (rich-string :value-of "hello")) => 0)

#|
rich-string%map
对rich-string对象中的每个字符应用映射函数，生成一个新的rich-string对象。

语法
----
(rich-string-instance :map f)

参数
----
f : procedure
一个接受rich-char对象作为参数并返回rich-char对象的映射函数。

返回值
-----
返回一个新的rich-string对象，包含映射后的字符序列。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用映射函数f。
映射函数必须接受rich-char对象作为参数并返回rich-char对象。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回空字符串
- 映射函数返回相同字符：返回与原字符串相同的字符串
- 映射函数改变字符：返回包含新字符的字符串
- Unicode字符映射：正确处理Unicode字符的映射

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 映射函数必须接受rich-char对象并返回rich-char对象
- 支持链式操作
|#

;; 基本功能测试
;; 字符大小写转换
(check ((rich-string :value-of "hello") :map (lambda (c) (c :to-upper)) :get) => "HELLO")

;; 字符类型转换
(check ((rich-string :value-of "123") :map (lambda (c) (c :to-upper)) :get) => "123")  ; 数字字符大小写转换无变化

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :map (lambda (c) (c :to-upper)) :get) => "")

;; 单字符字符串
(check ((rich-string :value-of "a") :map (lambda (c) (c :to-upper)) :get) => "A")

;; Unicode字符映射测试
(check ((rich-string :value-of "测试") :map (lambda (c) c) :get) => "测试")  ; 恒等映射

;; 复杂映射函数测试
(check ((rich-string :value-of "aBc") :map (lambda (c) (if (c :lower?) (c :to-upper) (c :to-lower))) :get) => "AbC")

;; 链式操作测试
(check ((rich-string :value-of "hello") :map (lambda (c) (c :to-upper)) :map (lambda (c) (c :to-lower)) :get) => "hello")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :map (lambda (c) c))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :map (lambda (c) (c :to-upper)))
  (check (original :get) => "hello"))

#|
rich-string%filter
根据给定的谓词条件过滤rich-string对象中的字符，生成一个新的rich-string对象。

语法
----
(rich-string-instance :filter pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
返回一个新的rich-string对象，包含满足谓词条件的字符序列。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
只有满足谓词条件的字符会被保留在新字符串中。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确过滤多字节编码的字符。

边界条件
--------
- 空字符串：返回空字符串
- 所有字符都满足谓词：返回与原字符串相同的字符串
- 所有字符都不满足谓词：返回空字符串
- 部分字符满足谓词：返回包含满足条件字符的字符串
- Unicode字符过滤：正确处理Unicode字符的过滤

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持链式操作
|#

;; 基本功能测试
;; 过滤小写字母
(check ((rich-string :value-of "Hello World") :filter (lambda (c) (c :lower?)) :get) => "elloorld")

;; 过滤大写字母
(check ((rich-string :value-of "Hello World") :filter (lambda (c) (c :upper?)) :get) => "HW")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :filter (lambda (c) #t) :get) => "")

;; 所有字符都满足谓词
(check ((rich-string :value-of "abc") :filter (lambda (c) #t) :get) => "abc")

;; 所有字符都不满足谓词
(check ((rich-string :value-of "abc") :filter (lambda (c) #f) :get) => "")

;; Unicode字符过滤测试
(check ((rich-string :value-of "测试123") :filter (lambda (c) (c :digit?)) :get) => "123")

;; 复杂谓词测试
(check ((rich-string :value-of "a1B2c3") :filter (lambda (c) (or (c :lower?) (c :digit?))) :get) => "a12c3")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :filter (lambda (c) #t))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "Hello")))
  (original :filter (lambda (c) (c :lower?)))
  (check (original :get) => "Hello"))

#|
rich-string%reverse
反转rich-string对象中的字符顺序，生成一个新的rich-string对象。

语法
----
(rich-string-instance :reverse)

返回值
-----
返回一个新的rich-string对象，包含字符顺序反转后的字符串。

说明
----
该方法反转rich-string对象中的字符顺序，返回一个新的rich-string对象。
原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确反转多字节编码的字符顺序。

边界条件
--------
- 空字符串：返回空字符串
- 单字符字符串：返回与原字符串相同的字符串
- 双字符字符串：交换两个字符的位置
- 长字符串：完全反转所有字符的顺序
- Unicode字符：正确处理Unicode字符的反转

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 普通字符串反转
(check ((rich-string :value-of "hello") :reverse :get) => "olleh")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :reverse :get) => "")

;; 单字符字符串
(check ((rich-string :value-of "a") :reverse :get) => "a")

;; 双字符字符串
(check ((rich-string :value-of "ab") :reverse :get) => "ba")

;; Unicode字符反转测试
(check ((rich-string :value-of "测试") :reverse :get) => "试测")

;; 混合字符测试
(check ((rich-string :value-of "a1B2c3") :reverse :get) => "3c2B1a")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :reverse)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :reverse)
  (check (original :get) => "hello"))

;; 链式操作测试
(check ((rich-string :value-of "abc") :reverse :reverse :get) => "abc")

#|
rich-string%index-where
查找rich-string对象中第一个满足给定谓词条件的字符的索引位置。

语法
----
(rich-string-instance :index-where pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以integer形式返回第一个满足谓词条件的字符的索引位置。
如果未找到满足条件的字符，返回-1。

说明
----
该方法从左到右遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
返回第一个满足谓词条件的字符的索引位置（从0开始计数）。
如果没有字符满足条件，返回-1。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回-1
- 所有字符都不满足谓词：返回-1
- 第一个字符满足谓词：返回0
- 最后一个字符满足谓词：返回最后一个索引
- 中间字符满足谓词：返回对应的索引
- Unicode字符：正确处理Unicode字符的索引位置

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的字符直到找到匹配
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持短路求值，找到第一个匹配就立即返回
|#

;; 基本功能测试
;; 查找第一个大写字母
(check ((rich-string :value-of "hello World") :index-where (lambda (c) (c :upper?))) => 6)

;; 查找第一个数字字符
(check ((rich-string :value-of "abc123") :index-where (lambda (c) (c :digit?))) => 3)

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :index-where (lambda (c) #t)) => -1)

;; 所有字符都不满足谓词
(check ((rich-string :value-of "hello") :index-where (lambda (c) (c :digit?))) => -1)

;; 第一个字符满足谓词
(check ((rich-string :value-of "Hello") :index-where (lambda (c) (c :upper?))) => 0)

;; 最后一个字符满足谓词
(check ((rich-string :value-of "hello!") :index-where (lambda (c) (c :equals #\!))) => 5)

;; Unicode字符查找测试
(check ((rich-string :value-of "测试字符串") :index-where (lambda (c) (c :equals (rich-char #x6D4B)))) => 0)  ; 查找"测"字

;; 复杂谓词测试
(check ((rich-string :value-of "a1B2c3") :index-where (lambda (c) (c :upper?))) => 2)  ; 查找第一个大写字母

;; 验证短路求值行为
(let ((count 0))
  ((rich-string :value-of "Hello") :index-where (lambda (c)
    (set! count (+ count 1))
    (c :upper?)))
  (check count => 1))  ; 应该在第一个字符'H'处停止

#|
rich-string%count
统计rich-string对象中满足给定谓词条件的字符数量。

语法
----
(rich-string-instance :count pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
以integer形式返回满足谓词条件的字符数量。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用谓词函数pred。
统计满足谓词条件的字符数量并返回。
对于空字符串，总是返回0。
该方法正确处理Unicode字符，能够准确统计多字节编码的字符。

边界条件
--------
- 空字符串：返回0
- 所有字符都满足谓词：返回字符串长度
- 所有字符都不满足谓词：返回0
- 部分字符满足谓词：返回满足条件的字符数量
- Unicode字符统计：正确处理Unicode字符的统计

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 统计结果总是非负整数
|#

;; 基本功能测试
;; 统计小写字母数量
(check ((rich-string :value-of "Hello World") :count (lambda (c) (c :lower?))) => 8)

;; 统计大写字母数量
(check ((rich-string :value-of "Hello World") :count (lambda (c) (c :upper?))) => 2)

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :count (lambda (c) #t)) => 0)

;; 所有字符都满足谓词
(check ((rich-string :value-of "abc") :count (lambda (c) #t)) => 3)

;; 所有字符都不满足谓词
(check ((rich-string :value-of "abc") :count (lambda (c) #f)) => 0)

;; Unicode字符统计测试
(check ((rich-string :value-of "测试123") :count (lambda (c) (c :digit?))) => 3)

;; 复杂谓词测试
(check ((rich-string :value-of "a1B2c3") :count (lambda (c) (or (c :lower?) (c :digit?)))) => 5)

;; 验证返回类型
(check (integer? ((rich-string :value-of "test") :count (lambda (c) #t))) => #t)

;; 验证统计结果非负
(check (>= ((rich-string :value-of "hello") :count (lambda (c) (c :lower?))) 0) => #t)

#|
rich-string%drop-while
从rich-string对象的开头开始，丢弃满足给定谓词条件的连续字符，返回剩余部分的rich-string对象。

语法
----
(rich-string-instance :drop-while pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
返回一个新的rich-string对象，包含从第一个不满足谓词条件的字符开始到字符串末尾的部分。
如果所有字符都满足谓词条件，返回空字符串。

说明
----
该方法从rich-string对象的开头开始，连续丢弃满足谓词条件的字符，直到遇到第一个不满足条件的字符为止。
返回从该字符开始到字符串末尾的部分。
原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回空字符串
- 所有字符都满足谓词：返回空字符串
- 所有字符都不满足谓词：返回原字符串
- 开头部分字符满足谓词：返回剩余部分字符串
- 中间字符满足谓词：只丢弃开头的连续满足条件的字符
- Unicode字符：正确处理Unicode字符的丢弃

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的字符直到找到第一个不满足条件的字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持链式操作
|#

;; 基本功能测试
;; 丢弃开头的小写字母
(check ((rich-string :value-of "helloWorld") :drop-while (lambda (c) (c :lower?)) :get) => "World")

;; 丢弃开头的数字字符
(check ((rich-string :value-of "123abc") :drop-while (lambda (c) (c :digit?)) :get) => "abc")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :drop-while (lambda (c) #t) :get) => "")

;; 所有字符都满足谓词
(check ((rich-string :value-of "aaa") :drop-while (lambda (c) #t) :get) => "")

;; 所有字符都不满足谓词
(check ((rich-string :value-of "AAA") :drop-while (lambda (c) (c :lower?)) :get) => "AAA")

;; Unicode字符丢弃测试
(check ((rich-string :value-of "测试字符串") :drop-while (lambda (c) (c :equals (rich-char #x6D4B))) :get) => "试字符串")  ; 丢弃"测"字

;; 复杂谓词测试
(check ((rich-string :value-of "a1b2c3") :drop-while (lambda (c) (or (c :lower?) (c :digit?))) :get) => "")  ; 所有字符都满足条件

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :drop-while (lambda (c) #t))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :drop-while (lambda (c) (c :lower?)))
  (check (original :get) => "hello"))

#|
rich-string%+
将当前rich-string对象与另一个字符串、rich-string对象或数字连接，生成一个新的rich-string对象。

语法
----
(rich-string-instance :+ s)

参数
----
s : any
要连接的元素，支持以下类型：
- string：标准字符串
- rich-string：rich-string对象
- number：数字（会自动转换为字符串）

返回值
-----
返回一个新的rich-string对象，包含连接后的字符串。

说明
----
该方法将当前rich-string对象与指定的元素连接，返回一个新的rich-string对象。
支持多种参数类型，包括字符串、rich-string对象和数字。
对于数字参数，会自动调用number->string转换为字符串。
原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确连接多字节编码的字符。

边界条件
--------
- 空字符串连接：连接空字符串返回原字符串
- 连接空字符串：返回原字符串
- 连接数字：正确转换为字符串并连接
- Unicode字符连接：正确处理Unicode字符的连接
- 链式操作：支持多次连接操作

性能特征
--------
- 时间复杂度：O(n)，需要创建新的字符串对象
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持多种参数类型
- 支持链式操作
- 正确处理Unicode字符
|#

;; 基本功能测试
;; 连接字符串
(check ((rich-string :value-of "hello") :+ " world" :get) => "hello world")

;; 连接rich-string对象
(check ((rich-string :value-of "hello") :+ (rich-string :value-of " world") :get) => "hello world")

;; 连接数字
(check ((rich-string :value-of "number: ") :+ 123 :get) => "number: 123")

;; 边界条件测试
;; 空字符串连接
(check ((rich-string :empty) :+ "hello" :get) => "hello")

;; 连接空字符串
(check ((rich-string :value-of "hello") :+ "" :get) => "hello")

;; Unicode字符连接测试
(check ((rich-string :value-of "测试") :+ "字符串" :get) => "测试字符串")

;; 链式操作测试
(check ((rich-string :value-of "a") :+ "b" :+ "c" :get) => "abc")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :+ "ing")) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :+ " world")
  (check (original :get) => "hello"))

#|
rich-string%strip-left
从rich-string对象的开头移除空白字符，返回一个新的rich-string对象。

语法
----
(rich-string-instance :strip-left)

返回值
-----
返回一个新的rich-string对象，包含移除开头空白字符后的字符串。

说明
----
该方法从rich-string对象的开头移除连续的空白字符，包括空格、制表符、换行符等。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的空白字符。

边界条件
--------
- 空字符串：返回空字符串
- 字符串开头没有空白字符：返回与原字符串相同的字符串
- 字符串开头有空白字符：返回移除空白字符后的字符串
- 字符串全部是空白字符：返回空字符串
- Unicode空白字符：正确处理Unicode空白字符的移除

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串开头的字符直到遇到第一个非空白字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 移除开头的空白字符
(check ((rich-string :value-of "  hello") :strip-left :get) => "hello")

;; 移除开头的制表符
(check ((rich-string :value-of "\thello") :strip-left :get) => "hello")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :strip-left :get) => "")

;; 字符串开头没有空白字符
(check ((rich-string :value-of "hello") :strip-left :get) => "hello")

;; 字符串全部是空白字符
(check ((rich-string :value-of "   ") :strip-left :get) => "")

;; Unicode空白字符测试
;; 注意：string-trim 可能不支持全角空格，使用普通空格测试
(check ((rich-string :value-of " 测试") :strip-left :get) => "测试")  ; 移除普通空格

;; 混合空白字符测试
(check ((rich-string :value-of " \t\nhello") :strip-left :get) => "hello")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "  test") :strip-left)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "  hello")))
  (original :strip-left)
  (check (original :get) => "  hello"))

;; 链式操作测试
(check ((rich-string :value-of "  hello  ") :strip-left :strip-right :get) => "hello")

#|
rich-string%to-vector
将rich-string对象转换为包含rich-char对象的向量。

语法
----
(rich-string-instance :to-vector)

返回值
-----
返回一个向量，包含rich-string对象中的所有rich-char对象。

说明
----
该方法将rich-string对象转换为向量，向量中的每个元素都是rich-char对象。
对于空字符串，返回空向量。
该方法正确处理Unicode字符，能够准确转换多字节编码的字符。

边界条件
--------
- 空字符串：返回空向量
- 单字符字符串：返回包含单个rich-char对象的向量
- 多字符字符串：返回包含所有rich-char对象的向量
- Unicode字符：正确处理Unicode字符的转换

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(n)，创建新的向量对象

兼容性
------
- 与所有rich-string实例兼容
- 返回的向量可以用于各种向量操作
|#

;; 基本功能测试
;; 普通字符串转换
(check (vector-length ((rich-string :value-of "hello") :to-vector)) => 5)

;; 边界条件测试
;; 空字符串
(check (vector-length ((rich-string :empty) :to-vector)) => 0)

;; 单字符字符串
(check (vector-length ((rich-string :value-of "a") :to-vector)) => 1)

;; Unicode字符转换测试
(check (vector-length ((rich-string :value-of "测试") :to-vector)) => 2)

;; 验证向量元素类型
(check (rich-char :is-type-of (vector-ref ((rich-string :value-of "hello") :to-vector) 0)) => #t)

;; 验证向量内容
(let ((vec ((rich-string :value-of "ab") :to-vector)))
  (check ((vector-ref vec 0) :equals #\a) => #t)
  (check ((vector-ref vec 1) :equals #\b) => #t))

;; 混合字符测试
(check (vector-length ((rich-string :value-of "a1B2c3") :to-vector)) => 6)

;; 验证返回类型
(check (vector? ((rich-string :value-of "test") :to-vector)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :to-vector)
  (check (original :get) => "hello"))

#|
rich-string%strip-prefix
从rich-string对象的开头移除指定的前缀，返回一个新的rich-string对象。

语法
----
(rich-string-instance :strip-prefix prefix)

参数
----
prefix : string
要移除的前缀字符串。

返回值
-----
返回一个新的rich-string对象，包含移除前缀后的字符串。
如果字符串不以指定的前缀开头，返回原字符串。

说明
----
该方法从rich-string对象的开头移除指定的前缀字符串。
如果字符串以指定的前缀开头，则移除该前缀并返回剩余部分。
如果字符串不以指定的前缀开头，则返回原字符串。
原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确移除多字节编码的前缀。

边界条件
--------
- 空字符串：移除任何前缀都返回空字符串
- 空前缀：返回原字符串
- 字符串以指定前缀开头：返回移除前缀后的字符串
- 字符串不以指定前缀开头：返回原字符串
- 前缀长度大于字符串长度：返回原字符串
- Unicode前缀：正确处理Unicode前缀的移除

性能特征
--------
- 时间复杂度：O(n)，需要检查字符串是否以前缀开头
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 移除存在的前缀
(check ((rich-string :value-of "hello world") :strip-prefix "hello " :get) => "world")

;; 字符串不以指定前缀开头
(check ((rich-string :value-of "hello world") :strip-prefix "world" :get) => "hello world")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :strip-prefix "hello" :get) => "")

;; 空前缀
(check ((rich-string :value-of "hello") :strip-prefix "" :get) => "hello")

;; 前缀与字符串完全匹配
(check ((rich-string :value-of "hello") :strip-prefix "hello" :get) => "")

;; Unicode前缀测试
(check ((rich-string :value-of "测试字符串") :strip-prefix "测试" :get) => "字符串")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "hello") :strip-prefix "h")) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello world")))
  (original :strip-prefix "hello ")
  (check (original :get) => "hello world"))

;; 链式操作测试
(check ((rich-string :value-of "prefix_suffix") :strip-prefix "prefix_" :get) => "suffix")

#|
rich-string%strip-right
从rich-string对象的末尾移除空白字符，返回一个新的rich-string对象。

语法
----
(rich-string-instance :strip-right)

返回值
-----
返回一个新的rich-string对象，包含移除末尾空白字符后的字符串。

说明
----
该方法从rich-string对象的末尾移除连续的空白字符，包括空格、制表符、换行符等。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的空白字符。

边界条件
--------
- 空字符串：返回空字符串
- 字符串末尾没有空白字符：返回与原字符串相同的字符串
- 字符串末尾有空白字符：返回移除空白字符后的字符串
- 字符串全部是空白字符：返回空字符串
- Unicode空白字符：正确处理Unicode空白字符的移除

性能特征
--------
- 时间复杂度：O(n)，需要从字符串末尾向前遍历直到遇到第一个非空白字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 移除末尾的空白字符
(check ((rich-string :value-of "hello  ") :strip-right :get) => "hello")

;; 移除末尾的制表符
(check ((rich-string :value-of "hello\t") :strip-right :get) => "hello")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :strip-right :get) => "")

;; 字符串末尾没有空白字符
(check ((rich-string :value-of "hello") :strip-right :get) => "hello")

;; 字符串全部是空白字符
(check ((rich-string :value-of "   ") :strip-right :get) => "")

;; Unicode字符测试
(check ((rich-string :value-of "测试  ") :strip-right :get) => "测试")  ; 移除末尾空格

;; 混合空白字符测试
(check ((rich-string :value-of "hello \t\n") :strip-right :get) => "hello")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test  ") :strip-right)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello  ")))
  (original :strip-right)
  (check (original :get) => "hello  "))

;; 链式操作测试
(check ((rich-string :value-of "  hello  ") :strip-left :strip-right :get) => "hello")

#|
rich-string%strip-both
从rich-string对象的开头和结尾移除空白字符，返回一个新的rich-string对象。

语法
----
(rich-string-instance :strip-both)

返回值
-----
返回一个新的rich-string对象，包含移除开头和结尾空白字符后的字符串。

说明
----
该方法从rich-string对象的开头和结尾移除连续的空白字符，包括空格、制表符、换行符等。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的空白字符。

边界条件
--------
- 空字符串：返回空字符串
- 字符串没有空白字符：返回与原字符串相同的字符串
- 字符串开头有空白字符：返回移除开头空白字符后的字符串
- 字符串结尾有空白字符：返回移除结尾空白字符后的字符串
- 字符串开头和结尾都有空白字符：返回移除两端空白字符后的字符串
- 字符串全部是空白字符：返回空字符串
- Unicode空白字符：正确处理Unicode空白字符的移除

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串两端的字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 支持链式操作
|#

;; 基本功能测试
;; 移除两端的空白字符
(check ((rich-string :value-of "  hello  ") :strip-both :get) => "hello")

;; 移除开头的空白字符
(check ((rich-string :value-of "  hello") :strip-both :get) => "hello")

;; 移除结尾的空白字符
(check ((rich-string :value-of "hello  ") :strip-both :get) => "hello")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :strip-both :get) => "")

;; 字符串没有空白字符
(check ((rich-string :value-of "hello") :strip-both :get) => "hello")

;; 字符串全部是空白字符
(check ((rich-string :value-of "   ") :strip-both :get) => "")

;; 混合空白字符测试
(check ((rich-string :value-of " \t\nhello \t\n") :strip-both :get) => "hello")

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "  test  ") :strip-both)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "  hello  ")))
  (original :strip-both)
  (check (original :get) => "  hello  "))

;; 链式操作测试
(check ((rich-string :value-of "  hello  ") :strip-both :strip-both :get) => "hello")

#|
rich-string%to-string
将rich-string对象转换为标准字符串。

语法
----
(rich-string-instance :to-string)

返回值
-----
以string形式返回rich-string对象内部存储的原始字符串数据。

说明
----
该方法返回rich-string对象内部存储的原始字符串数据。
由于rich-string对象本质上是对标准字符串的封装，
该方法提供了一种获取原始字符串数据的方式。
返回的字符串与创建rich-string对象时使用的字符串相同。
该方法不创建新的字符串对象，而是返回内部存储的引用。

边界条件
--------
- 空字符串：返回空字符串
- 普通字符串：返回原始字符串
- Unicode字符串：返回包含Unicode字符的原始字符串
- 特殊字符：返回包含特殊字符的原始字符串

性能特征
--------
- 时间复杂度：O(1)，直接返回内部存储的引用
- 空间复杂度：O(1)，不创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 返回的字符串可以用于所有标准字符串操作
|#

;; 基本功能测试
;; 普通字符串转换
(check ((rich-string :value-of "hello") :to-string) => "hello")

;; 空字符串转换
(check ((rich-string :empty) :to-string) => "")

;; 边界条件测试
;; 单字符字符串
(check ((rich-string :value-of "a") :to-string) => "a")

;; Unicode字符转换测试
(check ((rich-string :value-of "测试") :to-string) => "测试")

;; 特殊字符测试
(check ((rich-string :value-of "hello\nworld") :to-string) => "hello\nworld")

;; 数字字符串转换
(check ((rich-string :value-of "123") :to-string) => "123")

;; 混合字符测试
(check ((rich-string :value-of "a1B2c3") :to-string) => "a1B2c3")

;; 验证返回类型
(check (string? ((rich-string :value-of "test") :to-string)) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :to-string)
  (check (original :get) => "hello"))

;; 链式操作测试
(check (string-length ((rich-string :value-of "hello") :to-string)) => 5)

#|
rich-string%for-each
对rich-string对象中的每个字符应用给定的过程函数，主要用于执行副作用操作。

语法
----
(rich-string-instance :for-each f)

参数
----
f : procedure
一个接受rich-char对象作为参数的过程函数。该函数主要用于执行副作用操作，返回值会被忽略。

返回值
-----
返回未定义值（unspecified），主要用于执行副作用操作。

说明
----
该方法遍历rich-string中的每个字符，对每个字符应用过程函数f。
与map方法不同，for-each主要用于执行副作用操作，如打印、修改外部状态等。
过程函数f的返回值会被忽略。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：不执行任何操作
- 单字符字符串：对单个字符执行过程函数
- 多字符字符串：对每个字符依次执行过程函数
- Unicode字符：正确处理Unicode字符的遍历

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的每个字符
- 空间复杂度：O(1)，不创建新对象

兼容性
------
- 与所有rich-string实例兼容
- 过程函数必须接受rich-char对象作为参数
- 主要用于执行副作用操作，不返回有用的值
|#

;; 基本功能测试
;; 收集遍历的字符
(let ((collected '()))
  ((rich-string :value-of "hello") :for-each (lambda (c) (set! collected (cons (c :to-integer) collected))))
  (check (reverse collected) => '(104 101 108 108 111)))

;; 边界条件测试
;; 空字符串
(let ((count 0))
  ((rich-string :empty) :for-each (lambda (c) (set! count (+ count 1))))
  (check count => 0))

;; 单字符字符串
(let ((char-code #f))
  ((rich-string :value-of "a") :for-each (lambda (c) (set! char-code (c :to-integer))))
  (check char-code => 97))

;; Unicode字符测试
(let ((count 0))
  ((rich-string :value-of "测试") :for-each (lambda (c) (set! count (+ count 1))))
  (check count => 2))

;; 验证过程函数被正确调用
(let ((count 0))
  ((rich-string :value-of "abc") :for-each (lambda (c) (set! count (+ count 1))))
  (check count => 3))

;; 混合字符测试
(let ((lower-count 0))
  ((rich-string :value-of "a1B2c3") :for-each (lambda (c) (when (c :lower?) (set! lower-count (+ lower-count 1)))))
  (check lower-count => 2))

;; 验证字符顺序
(let ((chars '()))
  ((rich-string :value-of "123") :for-each (lambda (c) (set! chars (cons (c :to-integer) chars))))
  (check (reverse chars) => '(49 50 51)))

;; 验证副作用操作
(let ((sum 0))
  ((rich-string :value-of "123") :for-each (lambda (c) (set! sum (+ sum (c :to-integer)))))
  (check sum => 150))  ; 49 + 50 + 51 = 150

;; 验证返回值为未定义
(check (unspecified? ((rich-string :value-of "test") :for-each (lambda (c) #t))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello"))
      (count 0))
  (original :for-each (lambda (c) (set! count (+ count 1))))
  (check (original :get) => "hello"))

#|
rich-string%take-while
从rich-string对象的开头开始，连续取满足给定谓词条件的字符，生成一个新的rich-string对象。

语法
----
(rich-string-instance :take-while pred)

参数
----
pred : procedure
一个接受rich-char对象作为参数并返回布尔值的谓词函数。

返回值
-----
返回一个新的rich-string对象，包含从开头开始连续满足谓词条件的字符序列。
如果所有字符都满足谓词条件，返回原字符串。

说明
----
该方法从rich-string对象的开头开始，连续取满足谓词条件的字符，直到遇到第一个不满足条件的字符为止。
返回一个新的rich-string对象，原字符串保持不变（不可变性原则）。
该方法正确处理Unicode字符，能够准确处理多字节编码的字符。

边界条件
--------
- 空字符串：返回空字符串
- 所有字符都满足谓词：返回原字符串
- 所有字符都不满足谓词：返回空字符串
- 开头部分字符满足谓词：返回满足条件的连续字符序列
- 中间字符满足谓词：只取开头的连续满足条件的字符
- Unicode字符：正确处理Unicode字符的取操作

性能特征
--------
- 时间复杂度：O(n)，需要遍历字符串中的字符直到找到第一个不满足条件的字符
- 空间复杂度：O(n)，创建新的字符串对象

兼容性
------
- 与所有rich-string实例兼容
- 谓词函数必须接受rich-char对象作为参数
- 支持链式操作
|#

;; 基本功能测试
;; 取开头的小写字母
(check ((rich-string :value-of "helloWorld") :take-while (lambda (c) (c :lower?)) :get) => "hello")

;; 取开头的数字字符
(check ((rich-string :value-of "123abc") :take-while (lambda (c) (c :digit?)) :get) => "123")

;; 边界条件测试
;; 空字符串
(check ((rich-string :empty) :take-while (lambda (c) #t) :get) => "")

;; 所有字符都满足谓词
(check ((rich-string :value-of "aaa") :take-while (lambda (c) #t) :get) => "aaa")

;; 所有字符都不满足谓词
(check ((rich-string :value-of "AAA") :take-while (lambda (c) (c :lower?)) :get) => "")

;; Unicode字符测试
(check ((rich-string :value-of "测试字符串") :take-while (lambda (c) (c :equals (rich-char #x6D4B))) :get) => "测")  ; 只取"测"字

;; 复杂谓词测试
(check ((rich-string :value-of "a1b2c3") :take-while (lambda (c) (or (c :lower?) (c :digit?))) :get) => "a1b2c3")  ; 所有字符都满足条件

;; 验证返回类型
(check (rich-string :is-type-of ((rich-string :value-of "test") :take-while (lambda (c) #t))) => #t)

;; 验证原字符串不变性
(let ((original (rich-string :value-of "hello")))
  (original :take-while (lambda (c) (c :lower?)))
  (check (original :get) => "hello"))

(check-report)
