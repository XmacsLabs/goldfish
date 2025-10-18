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
        (liii unicode)
        (liii base))

(check-set-mode! 'report-failed)

(check (utf8->string (bytevector #x48 #x65 #x6C #x6C #x6F)) => "Hello")
(check (utf8->string #u8(#xC3 #xA4)) => "ä")
(check (utf8->string #u8(#xE4 #xB8 #xAD)) => "中")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D)) => "👍")

;; UTF-8 错误处理测试
(check-catch 'value-error (utf8->string (bytevector #xFF #x65 #x6C #x6C #x6F)))


(check (string->utf8 "Hello") => (bytevector #x48 #x65 #x6C #x6C #x6F))
(check (string->utf8 "ä") => #u8(#xC3 #xA4))
(check (string->utf8 "中") => #u8(#xE4 #xB8 #xAD))
(check (string->utf8 "👍") => #u8(#xF0 #x9F #x91 #x8D))
(check (string->utf8 "") => #u8())

;; UTF-8 边界错误处理测试
(check-catch 'out-of-range (string->utf8 "Hello" 2 6))
(check-catch 'out-of-range (string->utf8 "汉字书写" 4))


(check (utf8->string (string->utf8 "Hello" 1 2)) => "e")
(check (utf8->string (string->utf8 "Hello" 0 2)) => "He")
(check (utf8->string (string->utf8 "Hello" 2)) => "llo")
(check (utf8->string (string->utf8 "Hello" 2 5)) => "llo")

(check (utf8->string (string->utf8 "Hello")) => "Hello")
(check (utf8->string (string->utf8 "你好")) => "你好")
(check (utf8->string (string->utf8 "Hello 你好")) => "Hello 你好")

(check (utf8->string (string->utf8 "汉字书写")) => "汉字书写")
(check (utf8->string (string->utf8 "汉字书写" 1)) => "字书写")
(check (utf8->string (string->utf8 "汉字书写" 2)) => "书写")
(check (utf8->string (string->utf8 "汉字书写" 3)) => "写")

#|
u8-string-length
计算 UTF-8 编码字符串的 Unicode 字符数量（码点数量）。

函数签名
----
(u8-string-length string) → integer

参数
----
string : string
UTF-8 编码的字符串

返回值
----
integer
字符串中的 Unicode 字符数量（码点数量）

描述
----
`u8-string-length` 用于计算 UTF-8 编码字符串中的 Unicode 字符数量，与 `string-length` 不同，
它返回的是 Unicode 码点（code point）的数量，而不是字节数量。

行为特征
------
- 对于纯 ASCII 字符串，结果与 `string-length` 相同
- 对于包含多字节 UTF-8 字符的字符串，返回实际的 Unicode 字符数量
- 正确处理各种 Unicode 字符，包括基本多文种平面（BMP）和辅助平面字符
- 支持所有有效的 UTF-8 编码序列

与 string-length 的区别
-------------------
- `string-length` : 返回字符串的字节数量
- `u8-string-length` : 返回字符串的 Unicode 字符数量


错误处理
------
- 如果字符串包含无效的 UTF-8 编码序列，会抛出 `value-error` 异常
- 参数必须是字符串类型，否则会抛出 `type-error` 异常

实现原理
------
函数通过遍历字符串的 UTF-8 编码字节序列，使用 `bytevector-advance-u8` 函数
逐个识别完整的 UTF-8 字符，并统计字符数量。

相关函数
--------
- `string-length` : 获取字符串的字节长度
- `u8-substring` : 基于 Unicode 字符位置提取子字符串
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
|#

(check (u8-string-length "") => 0)
(check (u8-string-length "Hello") => 5)
(check (u8-string-length "你好") => 2)
(check (u8-string-length "Hello 你好") => 8)
(check (u8-string-length "👍") => 1)
(check (u8-string-length "🚀") => 1)
(check (u8-string-length "🎉") => 1)
(check (u8-string-length "Hello 👍 World") => 13)
(check (u8-string-length "你好 🚀 测试") => 7)

#|
u8-substring
基于 Unicode 字符位置提取子字符串

函数签名
----
(u8-substring string [start [end]]) → string

参数
----
string : string
UTF-8 编码的字符串

start : integer (可选，默认 0)
起始字符位置（基于 Unicode 字符计数）

end : integer (可选，默认字符串末尾)
结束字符位置（基于 Unicode 字符计数）

返回值
----
string
从 start 到 end 的子字符串

描述
----
`u8-substring` 用于从 UTF-8 编码的字符串中提取子字符串，与 `string-substring` 不同，
它基于 Unicode 字符位置而非字节位置进行提取。

行为特征
------
- 对于纯 ASCII 字符串，行为与 `string-substring` 相同
- 对于包含多字节 UTF-8 字符的字符串，基于 Unicode 字符位置进行提取
- 正确处理各种 Unicode 字符，包括基本多文种平面（BMP）和辅助平面字符
- 支持所有有效的 UTF-8 编码序列
- 支持可选参数，start 默认为 0，end 默认为字符串末尾

与 string-substring 的区别
-------------------
- `string-substring` : 基于字节位置提取子字符串
- `u8-substring` : 基于 Unicode 字符位置提取子字符串

错误处理
------
- 如果字符串包含无效的 UTF-8 编码序列，会抛出 `value-error` 异常
- 如果 start 或 end 超出字符串范围，会抛出 `out-of-range` 异常
- 参数必须是正确的类型，否则会抛出 `type-error` 异常

实现原理
------
函数通过 `string->utf8` 将字符串转换为 UTF-8 字节向量，基于 Unicode 字符位置
进行截取，然后使用 `utf8->string` 将字节向量转换回字符串。

相关函数
--------
- `u8-string-length` : 获取字符串的 Unicode 字符数量
- `string-substring` : 基于字节位置提取子字符串
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
|#

(check (u8-substring "Hello 你好" 0 5) => "Hello")
(check (u8-substring "Hello 你好" 6 8) => "你好")

(check (u8-substring "汉字书写" 0 1) => "汉")
(check (u8-substring "汉字书写" 0 4) => "汉字书写")
(check (u8-substring "汉字书写" 0) => "汉字书写")

;; u8-substring 边界条件测试
(check (u8-substring "" 0) => "")
(check (u8-substring "" 0 0) => "")

;; u8-substring 复杂 Unicode 字符测试
(check (u8-substring "Hello 👍 World" 6 7) => "👍")
(check (u8-substring "你好 🚀 测试" 3 4) => "🚀")
(check (u8-substring "🎉🎊🎈" 0 2) => "🎉🎊")
(check (u8-substring "🎉🎊🎈" 1 3) => "🎊🎈")

;; u8-substring 默认参数行为测试
(check (u8-substring "Hello World") => "Hello World")
(check (u8-substring "Hello World" 6) => "World")
(check (u8-substring "Hello World" 0 5) => "Hello")

;; u8-substring 混合字符测试
(check (u8-substring "Hello 你好 🚀" 6 8) => "你好")
(check (u8-substring "Hello 你好 🚀" 9 10) => "🚀")
(check (u8-substring "🎉Hello🎊" 1 6) => "Hello")

;; u8-substring 单字符提取测试
(check (u8-substring "汉字" 0 1) => "汉")
(check (u8-substring "汉字" 1 2) => "字")
(check (u8-substring "👍" 0 1) => "👍")

;; u8-substring 错误处理测试（通过 string->utf8 间接测试）
(check-catch 'out-of-range (u8-substring "Hello" 0 6))
(check-catch 'out-of-range (u8-substring "汉字" 0 3))

(check unicode-max-codepoint => #x10FFFF)
(check unicode-replacement-char => #xFFFD)

(check-report)
