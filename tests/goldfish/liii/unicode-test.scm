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

#|
utf8->string
将 UTF-8 编码的字节向量转换为字符串

函数签名
----
(utf8->string bytevector) → string

参数
----
bytevector : bytevector
包含 UTF-8 编码字节的字节向量

返回值
----
string
转换后的字符串

描述
----
`utf8->string` 用于将 UTF-8 编码的字节向量转换为字符串。
该函数遵循 R7RS 标准，支持所有有效的 Unicode 字符编码。

行为特征
------
- 支持所有有效的 Unicode 字符，包括基本多文种平面（BMP）和辅助平面字符
- 正确处理 ASCII 字符（单字节编码）
- 正确处理多字节 UTF-8 字符序列
- 空字节向量返回空字符串
- 与 `string->utf8` 函数形成互逆操作

编码规则
------
- ASCII 字符 (U+0000 到 U+007F): 1 字节编码
- 基本多文种平面字符 (U+0080 到 U+07FF): 2 字节编码
- 其他 BMP 字符 (U+0800 到 U+FFFF): 3 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码

错误处理
------
- 如果字节向量包含无效的 UTF-8 编码序列，会抛出 `value-error` 异常
- 参数必须是字节向量类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (liii base) 库中重新导出
- 与 `string->utf8` 函数形成互逆操作对

相关函数
--------
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
- `u8-string-length` : 获取字符串的 Unicode 字符数量
- `u8-substring` : 基于 Unicode 字符位置提取子字符串
|#

(check (utf8->string (bytevector #x48 #x65 #x6C #x6C #x6F)) => "Hello")
(check (utf8->string #u8(#xC3 #xA4)) => "ä")
(check (utf8->string #u8(#xE4 #xB8 #xAD)) => "中")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D)) => "👍")

;; UTF-8 错误处理测试
(check-catch 'value-error (utf8->string (bytevector #xFF #x65 #x6C #x6C #x6F)))

;; utf8->string 边界条件测试
(check (utf8->string #u8()) => "")
(check (utf8->string #u8(#x48)) => "H")
(check (utf8->string #u8(#x48 #x65)) => "He")

;; utf8->string 复杂 Unicode 字符测试
(check (utf8->string #u8(#xF0 #x9F #x9A #x80)) => "🚀")
(check (utf8->string #u8(#xF0 #x9F #x8E #x89)) => "🎉")
(check (utf8->string #u8(#xF0 #x9F #x8E #x8A)) => "🎊")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D #xF0 #x9F #x9A #x80)) => "👍🚀")

;; utf8->string 混合字符测试
(check (utf8->string #u8(#x48 #x65 #x6C #x6C #x6F #x20 #xF0 #x9F #x9A #x80 #x20 #x57 #x6F #x72 #x6C #x64)) => "Hello 🚀 World")
(check (utf8->string #u8(#xE4 #xBD #xA0 #xE5 #xA5 #xBD #x20 #xF0 #x9F #x8E #x89 #x20 #xE6 #xB5 #x8B #xE8 #xAF #x95)) => "你好 🎉 测试")

;; utf8->string 错误处理测试 - 更多无效 UTF-8 序列
(check-catch 'value-error (utf8->string (bytevector #x80)))
(check-catch 'value-error (utf8->string (bytevector #xF8 #x80 #x80 #x80 #x80)))
(check-catch 'value-error (utf8->string (bytevector #xFC #x80 #x80 #x80 #x80 #x80)))

;; utf8->string 与 string->utf8 互逆操作验证
(check (utf8->string (string->utf8 "")) => "")
(check (utf8->string (string->utf8 "H")) => "H")
(check (utf8->string (string->utf8 "Hello")) => "Hello")
(check (utf8->string (string->utf8 "ä")) => "ä")
(check (utf8->string (string->utf8 "中")) => "中")
(check (utf8->string (string->utf8 "👍")) => "👍")
(check (utf8->string (string->utf8 "🚀")) => "🚀")
(check (utf8->string (string->utf8 "汉字书写")) => "汉字书写")
(check (utf8->string (string->utf8 "Hello 你好 👍")) => "Hello 你好 👍")

;; utf8->string 单字符提取测试
(check (utf8->string #u8(#xE6 #xB1 #x89)) => "汉")
(check (utf8->string #u8(#xE5 #xAD #x97)) => "字")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D)) => "👍")


#|
string->utf8
将字符串转换为 UTF-8 编码的字节向量

函数签名
----
(string->utf8 string [start [end]]) → bytevector

参数
----
string : string
要转换的字符串

start : integer (可选，默认 0)
起始字符位置（基于字符计数）

end : integer (可选，默认字符串末尾)
结束字符位置（基于字符计数）

返回值
----
bytevector
包含 UTF-8 编码字节的字节向量

描述
----
`string->utf8` 用于将字符串转换为 UTF-8 编码的字节向量。
该函数遵循 R7RS 标准，支持所有有效的 Unicode 字符编码。

行为特征
------
- 支持所有有效的 Unicode 字符，包括基本多文种平面（BMP）和辅助平面字符
- 正确处理 ASCII 字符（单字节编码）
- 正确处理多字节 UTF-8 字符序列
- 支持可选参数 start 和 end 来指定字符串范围
- 空字符串返回空的字节向量

编码规则
------
- ASCII 字符 (U+0000 到 U+007F): 1 字节编码
- 基本多文种平面字符 (U+0080 到 U+07FF): 2 字节编码
- 其他 BMP 字符 (U+0800 到 U+FFFF): 3 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码

错误处理
------
- 如果 start 或 end 超出字符串范围，会抛出 `out-of-range` 异常
- 参数必须是正确的类型，否则会抛出 `type-error` 异常
- 如果字符串包含无效的 Unicode 字符，行为取决于具体实现

实现说明
------
- 函数在 R7RS 标准库中定义，在 (liii base) 库中重新导出
- 支持与 `utf8->string` 函数的互逆操作

相关函数
--------
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
- `u8-string-length` : 获取字符串的 Unicode 字符数量
- `u8-substring` : 基于 Unicode 字符位置提取子字符串
|#


(check (string->utf8 "Hello") => (bytevector #x48 #x65 #x6C #x6C #x6F))
(check (string->utf8 "ä") => #u8(#xC3 #xA4))
(check (string->utf8 "中") => #u8(#xE4 #xB8 #xAD))
(check (string->utf8 "👍") => #u8(#xF0 #x9F #x91 #x8D))
(check (string->utf8 "") => #u8())

;; UTF-8 边界错误处理测试
(check-catch 'out-of-range (string->utf8 "Hello" 2 6))
(check-catch 'out-of-range (string->utf8 "汉字书写" 4))

;; string->utf8 更多边界测试
(check (string->utf8 "Hello" 0 0) => #u8())
(check (string->utf8 "Hello" 1 1) => #u8())
(check (string->utf8 "Hello" 2 3) => #u8(#x6C))  ; "l"
(check (string->utf8 "Hello" 3 5) => #u8(#x6C #x6F))  ; "lo"

;; string->utf8 复杂 Unicode 字符测试
(check (string->utf8 "🚀") => #u8(#xF0 #x9F #x9A #x80))
(check (string->utf8 "🎉") => #u8(#xF0 #x9F #x8E #x89))
(check (string->utf8 "🎊") => #u8(#xF0 #x9F #x8E #x8A))

;; string->utf8 混合字符测试
(check (string->utf8 "Hello 🚀 World") => #u8(#x48 #x65 #x6C #x6C #x6F #x20 #xF0 #x9F #x9A #x80 #x20 #x57 #x6F #x72 #x6C #x64))
(check (string->utf8 "你好 🎉 测试") => #u8(#xE4 #xBD #xA0 #xE5 #xA5 #xBD #x20 #xF0 #x9F #x8E #x89 #x20 #xE6 #xB5 #x8B #xE8 #xAF #x95))

;; string->utf8 默认参数行为测试
(check (string->utf8 "Hello") => (bytevector #x48 #x65 #x6C #x6C #x6F))
(check (string->utf8 "Hello" 2) => #u8(#x6C #x6C #x6F))  ; "llo"
(check (string->utf8 "Hello" 0 3) => #u8(#x48 #x65 #x6C))  ; "Hel"

;; string->utf8 单字符提取测试
(check (string->utf8 "汉") => #u8(#xE6 #xB1 #x89))
(check (string->utf8 "字") => #u8(#xE5 #xAD #x97))
(check (string->utf8 "👍") => #u8(#xF0 #x9F #x91 #x8D))


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

#|
codepoint->utf8
将 Unicode 码点转换为 UTF-8 编码的字节向量

函数签名
----
(codepoint->utf8 codepoint) → bytevector

参数
----
codepoint : integer
Unicode 码点值

返回值
----
bytevector
包含 UTF-8 编码字节的字节向量

描述
----
`codepoint->utf8` 用于将 Unicode 码点转换为 UTF-8 编码的字节序列。

编码规则
------
- ASCII 字符 (U+0000 到 U+007F): 1 字节编码
- 基本多文种平面字符 (U+0080 到 U+07FF): 2 字节编码
- 其他 BMP 字符 (U+0800 到 U+FFFF): 3 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码

错误处理
------
- 如果码点超出 Unicode 范围 (0-0x10FFFF)，会抛出 `value-error` 异常
- 参数必须是整数类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数根据码点大小自动选择合适的 UTF-8 编码长度
- 返回字节向量便于与 `string->utf8` 保持一致

相关函数
--------
- `utf8->codepoint` : 将 UTF-8 字节向量转换为 Unicode 码点
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
|#

;; codepoint->utf8 ASCII 字符测试 (1字节编码)
(check (codepoint->utf8 #x48) => (bytevector #x48))  ; "H"
(check (codepoint->utf8 #x65) => (bytevector #x65))  ; "e"
(check (codepoint->utf8 #x6C) => (bytevector #x6C))  ; "l"
(check (codepoint->utf8 #x6F) => (bytevector #x6F))  ; "o"
(check (codepoint->utf8 #x20) => (bytevector #x20))  ; 空格
(check (codepoint->utf8 #x0A) => (bytevector #x0A))  ; 换行符

;; codepoint->utf8 基本多文种平面字符测试 (2字节编码)
(check (codepoint->utf8 #xA4) => #u8(#xC2 #xA4))  ; "¤" (CURRENCY SIGN)
(check (codepoint->utf8 #xE4) => #u8(#xC3 #xA4))  ; "ä"
(check (codepoint->utf8 #xE9) => #u8(#xC3 #xA9))  ; "é"
(check (codepoint->utf8 #xF6) => #u8(#xC3 #xB6))  ; "ö"
(check (codepoint->utf8 #xFC) => #u8(#xC3 #xBC))  ; "ü"

;; codepoint->utf8 其他 BMP 字符测试 (3字节编码)
(check (codepoint->utf8 #x4E2D) => #u8(#xE4 #xB8 #xAD))  ; "中"
(check (codepoint->utf8 #x6C49) => #u8(#xE6 #xB1 #x89))  ; "汉"
(check (codepoint->utf8 #x5B57) => #u8(#xE5 #xAD #x97))  ; "字"
(check (codepoint->utf8 #x5199) => #u8(#xE5 #x86 #x99))  ; "写"

;; codepoint->utf8 辅助平面字符测试 (4字节编码)
(check (codepoint->utf8 #x1F44D) => #u8(#xF0 #x9F #x91 #x8D))  ; "👍"
(check (codepoint->utf8 #x1F680) => #u8(#xF0 #x9F #x9A #x80))  ; "🚀"
(check (codepoint->utf8 #x1F389) => #u8(#xF0 #x9F #x8E #x89))  ; "🎉"
(check (codepoint->utf8 #x1F38A) => #u8(#xF0 #x9F #x8E #x8A))  ; "🎊"

;; codepoint->utf8 边界值测试
(check (codepoint->utf8 0) => (bytevector #x00))  ; 最小码点
(check (codepoint->utf8 127) => (bytevector #x7F))  ; ASCII 最大
(check (codepoint->utf8 128) => #u8(#xC2 #x80))  ; 2字节编码最小
(check (codepoint->utf8 2047) => #u8(#xDF #xBF))  ; 2字节编码最大
(check (codepoint->utf8 2048) => #u8(#xE0 #xA0 #x80))  ; 3字节编码最小
(check (codepoint->utf8 65535) => #u8(#xEF #xBF #xBF))  ; 3字节编码最大
(check (codepoint->utf8 65536) => #u8(#xF0 #x90 #x80 #x80))  ; 4字节编码最小
(check (codepoint->utf8 #x10FFFF) => #u8(#xF4 #x8F #xBF #xBF))  ; Unicode 最大码点

;; codepoint->utf8 错误处理测试
(check-catch 'value-error (codepoint->utf8 -1))  ; 负码点
(check-catch 'value-error (codepoint->utf8 #x110000))  ; 超出 Unicode 范围
(check-catch 'value-error (codepoint->utf8 #x200000))  ; 远超出范围

(check unicode-max-codepoint => #x10FFFF)
(check unicode-replacement-char => #xFFFD)

(check-report)
