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

(import (liii check)
        (scheme char))

(check-set-mode! 'report-failed)

#|
char-upcase
将字符转换为大写形式

函数签名
----
(char-upcase char) → char

参数
----
char : character
要转换的字符

返回值
----
character
转换后的大写字符

描述
----
`char-upcase` 用于将字符转换为大写形式。如果字符已经是大写或不是字母，则返回原字符。

行为特征
------
- 对于小写字母，返回对应的大写字母
- 对于大写字母，返回原字符
- 对于非字母字符，返回原字符
- 遵循 R7RS 标准规范


错误处理
------
- 参数必须是字符类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- char-upcase 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `char-downcase` : 将字符转换为小写形式
- `char-upper-case?` : 判断字符是否为大写字母
- `char-lower-case?` : 判断字符是否为小写字母
- `char-foldcase` : 执行大小写折叠
|#

(check (char-upcase #\z) => #\Z)
(check (char-upcase #\a) => #\A)

(check (char-upcase #\A) => #\A)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\$) => #\$)
(check (char-upcase #\.) => #\.)
(check (char-upcase #\\) => #\\)
(check (char-upcase #\5) => #\5)
(check (char-upcase #\)) => #\))
(check (char-upcase #\%) => #\%)
(check (char-upcase #\0) => #\0)
(check (char-upcase #\_) => #\_)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\space) => #\space)
(check (char-upcase #\newline) => #\newline)
(check (char-upcase #\null) => #\null)

;; Test char-upcase error handling
(check-catch 'type-error (char-upcase "a"))
(check-catch 'type-error (char-upcase 65))
(check-catch 'type-error (char-upcase 'a))

#|
char-downcase
将字符转换为小写形式

函数签名
----
(char-downcase char) → char

参数
----
char : character
要转换的字符

返回值
----
character
转换后的小写字符

描述
----
`char-downcase` 用于将字符转换为小写形式。如果字符已经是小写或不是字母，则返回原字符。

行为特征
------
- 对于大写字母，返回对应的小写字母
- 对于小写字母，返回原字符
- 对于非字母字符，返回原字符
- 遵循 R7RS 标准规范

错误处理
------
- 参数必须是字符类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- char-downcase 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `char-upcase` : 将字符转换为大写形式
- `char-upper-case?` : 判断字符是否为大写字母
- `char-lower-case?` : 判断字符是否为小写字母
- `char-foldcase` : 执行大小写折叠
|#

(check (char-downcase #\A) => #\a)
(check (char-downcase #\Z) => #\z)

(check (char-downcase #\a) => #\a)

;; Test char-downcase error handling
(check-catch 'type-error (char-downcase "A"))
(check-catch 'type-error (char-downcase 65))
(check-catch 'type-error (char-downcase 'A))

(check-true (char-upper-case? #\A))
(check-true (char-upper-case? #\Z))

(check-false (char-upper-case? #\a))
(check-false (char-upper-case? #\z))

(check-true (char-lower-case? #\a))
(check-true (char-lower-case? #\z))

(check-false (char-lower-case? #\A))
(check-false (char-lower-case? #\Z))

#|
digit-value
获取数字字符的数值

函数签名
----
(digit-value char) → integer | #f

参数
----
char : character
要获取数值的字符

返回值
----
integer | #f
如果字符是数字字符，返回对应的整数值（0-9）；否则返回 #f

描述
----
`digit-value` 用于获取数字字符对应的整数值。该函数只处理基本的 ASCII 数字字符（0-9）。

行为特征
------
- 对于数字字符 #\0 到 #\9，返回对应的整数值 0 到 9
- 对于非数字字符，返回 #f
- 遵循 R7RS 标准规范

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- 使用 char-numeric? 判断字符是否为数字
- 通过字符编码的差值计算数值

相关函数
--------
- `char-numeric?` : 判断字符是否为数字字符
- `char->integer` : 获取字符的整数编码
- `integer->char` : 将整数转换为字符
|#

;; Test digit-value with numeric characters
(check (digit-value #\0) => 0)
(check (digit-value #\1) => 1)
(check (digit-value #\2) => 2)
(check (digit-value #\3) => 3)
(check (digit-value #\4) => 4)
(check (digit-value #\5) => 5)
(check (digit-value #\6) => 6)
(check (digit-value #\7) => 7)
(check (digit-value #\8) => 8)
(check (digit-value #\9) => 9)

;; Test digit-value with non-numeric characters
(check (digit-value #\a) => #f)
(check (digit-value #\c) => #f)
(check (digit-value #\A) => #f)
(check (digit-value #\Z) => #f)
(check (digit-value #\space) => #f)
(check (digit-value #\newline) => #f)
(check (digit-value #\null) => #f)
(check (digit-value #\.) => #f)
(check (digit-value #\,) => #f)
(check (digit-value #\!) => #f)
(check (digit-value #\@) => #f)
(check (digit-value #\$) => #f)
(check (digit-value #\%) => #f)
(check (digit-value #\^) => #f)
(check (digit-value #\&) => #f)
(check (digit-value #\*) => #f)
(check (digit-value #\( ) => #f)
(check (digit-value #\)) => #f)
(check (digit-value #\_) => #f)
(check (digit-value #\+) => #f)
(check (digit-value #\-) => #f)
(check (digit-value #\=) => #f)
(check (digit-value #\[) => #f)
(check (digit-value #\]) => #f)
(check (digit-value #\{) => #f)
(check (digit-value #\}) => #f)
(check (digit-value #\|) => #f)
(check (digit-value #\\) => #f)
(check (digit-value #\:) => #f)
(check (digit-value #\;) => #f)
(check (digit-value #\") => #f)
(check (digit-value #\') => #f)
(check (digit-value #\<) => #f)
(check (digit-value #\>) => #f)
(check (digit-value #\?) => #f)
(check (digit-value #\/) => #f)

(check-report)

