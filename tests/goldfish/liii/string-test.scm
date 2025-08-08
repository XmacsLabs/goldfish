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
        (liii base)
        (liii string)
        (srfi srfi-13)
        (liii error))


(check-set-mode! 'report-failed)

#|
string-join
将一个字符串列表通过指定的分隔符连接起来。

语法
----
(string-join string-list)
(string-join string-list delimiter)
(string-join string-list delimiter grammar)

参数
----
string-list : list
一个字符串列表，可以包含零个或多个字符串元素。

delimiter : string
用作分隔符的字符串，默认值为空字符串""（等价于不使用分隔符）。

grammar : symbol
指定连接语法模式，可选值包括：
- 'infix（或省略）：在中缀模式下，分隔符放在每对相邻元素之间
- 'suffix：在后缀模式下，分隔符放在每个元素（包括最后一个）之后
- 'prefix：在前缀模式下，分隔符放在每个元素（包括第一个）之前
- 'strict-infix：严格中缀模式，要求string-list不能为空，否则会抛错

返回值
----
string
返回由string-list中的字符串按指定语法模式连接而成的字符串。

注意
----
当string-list为空列表时：
- 中缀模式 ('infix) 和省略语法参数：返回空字符串""
- 后缀模式 ('suffix) 返回空字符串""
- 前缀模式 ('prefix) 返回空字符串""
- 严格中缀模式 ('strict-infix) 抛出value-error异常

错误处理
----
value-error 当语法模式为'strict-infix且string-list为空列表时
value-error 当提供了无效的语法模式时
type-error  当提供了无效的参数类型时
wrong-number-of-args 当参数数量不正确时
|#

(check (string-join '("a" "b" "c")) => "abc")

(check (string-join '("a" "b" "c") ":") => "a:b:c")
(check (string-join '("a" "b" "c") ":" 'infix) => "a:b:c")
(check (string-join '("a" "b" "c") ":" 'suffix) => "a:b:c:")
(check (string-join '("a" "b" "c") ":" 'prefix) => ":a:b:c")

(check (string-join '() ":") => "")
(check (string-join '() ":" 'infix) => "")
(check (string-join '() ":" 'prefix) => "")
(check (string-join '() ":" 'suffix) => "")

(check-catch 'value-error (string-join '() ":" 'strict-infix))
(check-catch 'type-error (string-join '() ":" 2))
(check-catch 'value-error (string-join '() ":" 'no-such-grammer))
(check-catch 'wrong-number-of-args (string-join '() ":" 1 2 3))

#|
string-null?
判断一个字符串是否为空字符串。

语法
----
(string-null? str)

参数
----
str : string?
要检查的字符串。可以是s7字符串或其它自动转换为字符串的对象。

返回值
----
boolean
如果str是空字符串("")则返回#t，否则返回#f。

注意
----
string-null?主要用于测试字符串是否为零长度。字符串为空字符串的标准是
其长度为0。字符串非字符串类型的参数会引发错误。

示例
----
(string-null? "") => #t
(string-null? "a") => #f
(string-null? " ") => #f

错误处理
----
type-error 当str不是字符串类型时
|#

(check-true (string-null? ""))
(check-true (string-null? (make-string 0)))

(check-false (string-null? "a"))
(check-false (string-null? " "))
(check-false (string-null? (string #\null)))
(check-false (string-null? "aa"))
(check-false (string-null? "中文"))
(check-false (string-null? "123"))
(check-false (string-null? "MathAgape"))

(check-catch 'type-error (string-null? 'not-a-string))
(check-catch 'type-error (string-null? 123))
(check-catch 'type-error (string-null? #\a))
(check-catch 'type-error (string-null? (list "a")))

#|
string-every
检查字符串中的每个字符是否都满足给定的条件。

语法
----
(string-every char/pred? str)
(string-every char/pred? str start)
(string-every char/pred? str start end)

参数
----
char/pred? : char 或 procedure?
- 字符(char)：检查字符串中的每个字符是否等于该字符
- 谓词(procedure)：接受单个字符作为参数，返回布尔值

str : string?
要检查的字符串

start : integer? 可选
检查的起始位置(包含)，默认为0

end : integer? 可选
检查的结束位置(不包含)，默认为字符串长度

返回值
----
boolean
如果字符串中的每个字符都满足条件则返回#t，否则返回#f。
对于空字符串或空范围(如start=end)始终返回#t。

注意
----
string-every支持多种类型的参数作为char/pred?，包括字符和谓词函数。
当使用start/end参数时，检查对应子字符串的范围。
空字符串或空范围会返回#t，因为没有任何字符违反条件。

示例
----
(string-every #\x "xxxxxx") => #t
(string-every #\x "xxx0xx") => #f
(string-every char-numeric? "012345") => #t
(string-every char-numeric? "012d45") => #f
(string-every char-alphabetic? "abc") => #t
(string-every char-alphabetic? "abc123") => #f

错误处理
----
wrong-type-arg 当char/pred?不是字符或谓词时
out-of-range 当start/end超出字符串索引范围时
wrong-type-arg 当str不是字符串时
|#

(check-true (string-every #\x "xxxxxx"))
(check-false (string-every #\x "xxx0xx"))

(check-true (string-every char-numeric? "012345"))
(check-false (string-every char-numeric? "012d45"))

(check-true (string-every char-alphabetic? "abc"))
(check-false (string-every char-alphabetic? "abc123"))
(check-true (string-every char-upper-case? "ABC"))
(check-false (string-every char-upper-case? "AbC"))

(check-true (string-every char-whitespace? "   "))
(check-false (string-every char-whitespace? "  a "))

(check-true (string-every #\a ""))
(check-true (string-every char-numeric? ""))

(check-catch 'wrong-type-arg (string-every 1 "012345"))
(check-catch 'wrong-type-arg (string-every #\012345 "012345"))
(check-catch 'wrong-type-arg (string-every "012345" "012345"))

(check-true (string-every char-numeric? "012345"))
(check-false (string-every number? "012345"))

(check-true (string-every char-numeric? "ab2345" 2))
(check-false (string-every char-numeric? "ab2345" 1))
(check-false (string-every  char-numeric? "ab234f" 2))
(check-true (string-every char-numeric? "ab234f" 2 4))
(check-true (string-every char-numeric? "ab234f" 2 2))
(check-false (string-every char-numeric? "ab234f" 1 4))
(check-true (string-every char-numeric? "ab234f" 2 5))
(check-false (string-every char-numeric? "ab234f" 2 6))

(check-true (string-every #\a "aabbcc" 0 1))
(check-false (string-every #\a "aabbcc" 1 3))
(check-true (string-every char-lower-case? "abcABC" 0 3))
(check-false (string-every char-lower-case? "abcABC" 3 6))

(check-catch 'out-of-range (string-every char-numeric? "ab234f" 2 7))
(check-catch 'out-of-range (string-every char-numeric? "ab234f" 2 1))

#|
string-any
检查字符串中的任意字符是否满足给定的条件。

语法
----
(string-any char/pred? str)
(string-any char/pred? str start)
(string-any char/pred? str start end)

参数
----
char/pred? : char 或 procedure?
- 字符(char)：检查字符串中是否存在与该字符相等的字符
- 谓词(procedure)：接受单个字符作为参数，返回布尔值

str : string?
要检查的字符串

start : integer? 可选
检查的起始位置(包含)，默认为0

end : integer? 可选
检查的结束位置(不包含)，默认为字符串长度

返回值
----
boolean
- 如果字符串中至少有一个字符满足条件则返回#t，否则返回#f
- 对于空字符串或空范围始终返回#f

注意
----
string-any是string-every的对偶函数。与检查每个字符是否满足条件的string-every不同，string-any只需要找到至少一个满足条件的字符即可返回真值。
该函数也支持start和end参数来限定检查范围。
空字符串或空范围会返回#f，因为没有任何字符满足条件。

示例
----
(string-any char-numeric? "abc123") => #t
(string-any char-numeric? "hello") => #f
(string-any char-alphabetic? "12345a") => #t
(string-any char-alphabetic? "12345") => #f
(string-any char-upper-case? "abC12") => #t
(string-any char-whitespace? "hello") => #f
(string-any #\a "zebra") => #\a
(string-any #\z "apple") => #f

错误处理
----
wrong-type-arg 当char/pred?不是字符或谓词时
out-of-range 当start/end超出字符串索引范围时
wrong-type-arg 当str不是字符串时
|#

; Basic functionality tests for character parameter
(check-true (string-any #\a "abcde"))
(check-false (string-any #\z "abcde"))
(check-false (string-any #\a "xyz"))
(check-true (string-any #\x "abcxdef"))

; Basic functionality tests for predicate parameter
(check-true (string-any char-numeric? "abc123"))
(check-false (string-any char-numeric? "hello"))
(check-true (string-any char-alphabetic? "12345a"))
(check-false (string-any char-alphabetic? "12345"))
(check-true (string-any char-upper-case? "hello World"))
(check-false (string-any char-upper-case? "hello world"))

; Empty string handling
(check-false (string-any #\a ""))
(check-false (string-any char-numeric? ""))

; Single character strings
(check-true (string-any #\a "a"))
(check-false (string-any #\b "a"))
(check-true (string-any char-numeric? "1"))
(check-false (string-any char-numeric? "a"))

; Whitespace and special characters
(check-true (string-any char-whitespace? "hello world"))
(check-false (string-any char-whitespace? "hello"))
(check-true (string-any (lambda (c) (char=? c #\h)) "hello"))
(check-true (string-any (lambda (c) (char=? c #\!)) "hello!"))

; Complex character tests
(check-true (string-any char-alphabetic? "HELLO"))
(check-true (string-any char-numeric? "123abc"))

; Original legacy tests
(check-true (string-any #\0 "xxx0xx"))
(check-false (string-any #\0 "xxxxxx"))
(check-true (string-any char-numeric? "xxx0xx"))
(check-false (string-any char-numeric? "xxxxxx"))

; Start/end parameter tests
(check-true (string-any char-alphabetic? "01c345" 2))
(check-false (string-any char-alphabetic? "01c345" 3))
(check-true (string-any char-alphabetic? "01c345" 2 4))
(check-false (string-any char-alphabetic? "01c345" 2 2))
(check-false (string-any char-alphabetic? "01c345" 3 4))
(check-true (string-any char-alphabetic? "01c345" 2 6))

; Additional comprehensive tests for start/end parameters
(check-true (string-any #\a "012a34" 0))
(check-false (string-any #\a "012345" 0 2))
(check-true (string-any #\0 "012345" 0 1))
(check-false (string-any #\a "bbbccc" 1 3))
(check-true (string-any char-alphabetic? "1a23bc" 1 4))
(check-false (string-any char-alphabetic? "123456" 0 3))

; Edge cases
(check-true (string-any char-alphabetic? "abc" 0 3))
(check-false (string-any char-alphabetic? "123" 0 3))
(check-true (string-any #\a "aab" 1 2))
(check-false (string-any #\a "bbc" 1 2))
(check-true (string-any char-alphabetic? "a" 0 1))
(check-false (string-any char-alphabetic? "" 0 0))

; Custom predicate tests
(check-true (string-any (lambda (c) (char=? c #\x)) "hello x there"))
(check-false (string-any (lambda (c) (char=? c #\z)) "hello w there"))
(check-true (string-any char-alphabetic? "HELLO"))
(check-true (string-any char-alphabetic? "123a"))

(check
  (catch 'out-of-range
    (lambda () 
      (string-any 
        char-alphabetic?
        "01c345"
        2
        7))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () 
      (string-any 
        char-alphabetic?
        "01c345"
        2
        1))
    (lambda args #t))
  =>
  #t)

; Error handling tests for string-any
(check-catch 'wrong-type-arg (string-any 123 "hello"))
(check-catch 'wrong-type-arg (string-any "a" "hello"))
(check-catch 'wrong-type-arg (string-any '(a b) "hello"))
(check-catch 'wrong-type-arg (string-any (lambda (n) (= n 0)) "hello"))
(check-catch 'wrong-type-arg (string-any char-alphabetic? 123))
(check-catch 'wrong-type-arg (string-any char-alphabetic? "hello" "0"))
(check-catch 'wrong-type-arg (string-any char-alphabetic? "hello" 1.5))
(check-catch 'wrong-type-arg (string-any char-alphabetic? "hello" 'a))

; Out of range tests
(check-catch 'out-of-range (string-any char-alphabetic? "hello" -1))
(check-catch 'out-of-range (string-any char-alphabetic? "hello" 0 6))
(check-catch 'out-of-range (string-any char-alphabetic? "hello" 5 1))
(check-catch 'out-of-range (string-any char-alphabetic? "hello" 10))

(define original-string "MathAgape")
(define copied-string (string-copy original-string))

(check-true (equal? original-string copied-string))
(check-false (eq? original-string copied-string))

(check-true
  (equal? (string-copy "MathAgape" 4)
          (string-copy "MathAgape" 4)))

(check-false
  (eq? (string-copy "MathAgape" 4)
       (string-copy "MathAgape" 4)))

(check-true
  (equal? (string-copy "MathAgape" 4 9)
          (string-copy "MathAgape" 4 9)))

(check-false
  (eq? (string-copy "MathAgape" 4 9)
       (string-copy "MathAgape" 4 9)))

#|
string-take
从字符串开头提取指定数量的字符。

语法
----
(string-take str k)

参数
----
str : string?
源字符串，从中提取字符。

k : integer?
要提取的字符数量，必须是非负整数且不超过字符串长度。

返回值
----
string
包含源字符串前k个字符的新字符串。

注意
----
string-take等价于(substring str 0 k)，但提供了更语义化的名称。
对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。

示例
----
(string-take "MathAgape" 4) => "Math"
(string-take "Hello" 0) => ""
(string-take "abc" 2) => "ab"

错误处理
----
out-of-range 当k大于字符串长度或k为负数时
wrong-type-arg 当str不是字符串类型或k不是整数类型时
|#
(check (string-take "MathAgape" 4) => "Math")
(check (string-take "MathAgape" 0) => "")
(check (string-take "MathAgape" 9) => "MathAgape")
(check (string-take "" 0) => "")
(check (string-take "a" 1) => "a")
(check (string-take "Hello" 1) => "H")
(check (string-take "abc" 2) => "ab")
(check (string-take "test123" 4) => "test")
(check (string-take "中文测试" 6) => "中文")
(check (string-take "🌟🎉" 4) => "🌟")
(check-catch 'out-of-range (string-take "MathAgape" 20))
(check-catch 'out-of-range (string-take "" 1))
(check-catch 'out-of-range (string-take "Hello" -1))
(check-catch 'wrong-type-arg (string-take 123 4))
(check-catch 'wrong-type-arg (string-take "MathAgape" "4"))
(check-catch 'wrong-type-arg (string-take "MathAgape" 4.5))
(check-catch 'wrong-type-arg (string-take "MathAgape" 'a))

(check (string-take-right "MathAgape" 0) => "")
(check (string-take-right "MathAgape" 1) => "e")
(check (string-take-right "MathAgape" 9) => "MathAgape")

#|
string-take-right
从字符串末尾提取指定数量的字符。

语法
----
(string-take-right str k)

参数
----
str : string?
源字符串，从中提取字符。

k : integer?
要提取的字符数量，必须是非负整数且不超过字符串长度。

返回值
----
string
包含源字符串最后k个字符的新字符串。

注意
----
string-take-right等价于(substring str (- (string-length str) k) (string-length str))，但提供了更语义化的名称。
对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。

示例
----
(string-take-right "MathAgape" 4) => "gape"
(string-take-right "Hello" 0) => ""
(string-take-right "abc" 2) => "bc"

错误处理
----
out-of-range 当k大于字符串长度或k为负数时
wrong-type-arg 当str不是字符串类型或k不是整数类型时
|#
(check (string-take-right "MathAgape" 4) => "gape")
(check (string-take-right "MathAgape" 0) => "")
(check (string-take-right "MathAgape" 9) => "MathAgape")
(check (string-take-right "" 0) => "")
(check (string-take-right "a" 1) => "a")
(check (string-take-right "Hello" 1) => "o")
(check (string-take-right "abc" 2) => "bc")
(check (string-take-right "test123" 3) => "123")
(check (string-take-right "中文测试" 6) => "测试")
(check (string-take-right "🌟🎉" 4) => "🎉")

(check-catch 'out-of-range (string-take-right "MathAgape" 20))
(check-catch 'out-of-range (string-take-right "" 1))
(check-catch 'out-of-range (string-take-right "Hello" -1))
(check-catch 'wrong-type-arg (string-take-right 123 4))
(check-catch 'wrong-type-arg (string-take-right "MathAgape" "4"))
(check-catch 'wrong-type-arg (string-take-right "MathAgape" 4.5))
(check-catch 'wrong-type-arg (string-take-right "MathAgape" 'a))

#|
string-drop
从字符串开头移除指定数量的字符。

语法
----
(string-drop str k)

参数
----
str : string?
源字符串，从中移除字符。

k : integer?
要移除的字符数量，必须是非负整数且不超过字符串长度。

返回值
----
string
返回一个新的字符串，包含源字符串从位置k开始的所有字符。

注意
----
string-drop等价于(substring str k (string-length str))，但提供了更语义化的名称。
对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。

示例
----
(string-drop "MathAgape" 4) => "Agape"
(string-drop "Hello" 0) => "Hello"
(string-drop "abc" 2) => "c"
(string-drop "test123" 4) => "123"

错误处理
----
out-of-range 当k大于字符串长度或k为负数时
wrong-type-arg 当str不是字符串类型或k不是整数类型时
|#
(check (string-drop "MathAgape" 4) => "Agape")
(check (string-drop "MathAgape" 0) => "MathAgape")
(check (string-drop "MathAgape" 9) => "")
(check (string-drop "MathAgape" 8) => "e")
(check (string-drop "MathAgape" 1) => "athAgape")
(check (string-drop "MathAgape" 2) => "thAgape")
(check (string-drop "MathAgape" 3) => "hAgape")
(check (string-drop "MathAgape" 5) => "gape")
(check (string-drop "MathAgape" 6) => "ape")
(check (string-drop "MathAgape" 7) => "pe")
(check (string-drop "" 0) => "")
(check (string-drop "a" 1) => "")
(check (string-drop "Hello" 1) => "ello")
(check (string-drop "Hello" 5) => "")
(check (string-drop "Hello" 0) => "Hello")
(check (string-drop "abc" 2) => "c")
(check (string-drop "abc" 1) => "bc")
(check (string-drop "test123" 4) => "123")
(check (string-drop "test123" 3) => "t123")
(check (string-drop "test123" 6) => "3")
(check (string-drop "test123" 7) => "")
(check (string-drop "中文测试" 6) => "测试")
(check (string-drop "中文测试" 3) => "文测试")
(check (string-drop "中文测试" 12) => "")
(check (string-drop "🌟🎉" 4) => "🎉")
(check (string-drop "🌟🎉" 8) => "")

(check-catch 'out-of-range (string-drop "MathAgape" 20))
(check-catch 'out-of-range (string-drop "" 1))
(check-catch 'out-of-range (string-drop "Hello" -1))
(check-catch 'wrong-type-arg (string-drop 123 4))
(check-catch 'wrong-type-arg (string-drop "MathAgape" "4"))
(check-catch 'wrong-type-arg (string-drop "MathAgape" 4.5))
(check-catch 'wrong-type-arg (string-drop "MathAgape" 'a))

(check (string-drop "MathAgape" 8) => "e")
(check (string-drop "MathAgape" 9) => "")
(check (string-drop "MathAgape" 0) => "MathAgape")

(check-catch 'out-of-range (string-drop "MahtAgape" -1))
(check-catch 'out-of-range (string-drop "MathAgape" 20))

#|
string-drop-right
从字符串末尾移除指定数量的字符。

语法
----
(string-drop-right str k)

参数
----
str : string?
源字符串，从中移除字符。

k : integer?
要移除的字符数量，必须是非负整数且不超过字符串长度。

返回值
----
string
返回一个新的字符串，包含源字符串从开始位置到(len-k)的所有字符，其中len为字符串长度。

注意
----
string-drop-right等价于(substring str 0 (- len k))，但提供了更语义化的名称。
对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。

示例
----
(string-drop-right "MathAgape" 4) => "Math"
(string-drop-right "Hello" 0) => "Hello"
(string-drop-right "abc" 2) => "a"
(string-drop-right "test123" 3) => "test"

错误处理
----
out-of-range 当k大于字符串长度或k为负数时
wrong-type-arg 当str不是字符串类型或k不是整数类型时
|#
(check (string-drop-right "MathAgape" 4) => "MathA")
(check (string-drop-right "MathAgape" 0) => "MathAgape")
(check (string-drop-right "MathAgape" 9) => "")
(check (string-drop-right "MathAgape" 8) => "M")
(check (string-drop-right "MathAgape" 1) => "MathAgap")
(check (string-drop-right "MathAgape" 2) => "MathAga")
(check (string-drop-right "MathAgape" 3) => "MathAg")
(check (string-drop-right "MathAgape" 5) => "Math")
(check (string-drop-right "MathAgape" 6) => "Mat")
(check (string-drop-right "MathAgape" 7) => "Ma")
(check (string-drop-right "" 0) => "")
(check (string-drop-right "a" 1) => "")
(check (string-drop-right "Hello" 1) => "Hell")
(check (string-drop-right "Hello" 5) => "")
(check (string-drop-right "Hello" 0) => "Hello")
(check (string-drop-right "abc" 2) => "a")
(check (string-drop-right "abc" 1) => "ab")
(check (string-drop-right "test123" 3) => "test")
(check (string-drop-right "test123" 4) => "tes")
(check (string-drop-right "test123" 6) => "t")
(check (string-drop-right "test123" 7) => "")
(check (string-drop-right "中文测试" 6) => "中文")
(check (string-drop-right "中文测试" 3) => "中文测")
(check (string-drop-right "中文测试" 12) => "")
(check (string-drop-right "🌟🎉" 4) => "🌟")
(check (string-drop-right "🌟🎉" 8) => "")

(check-catch 'out-of-range (string-drop-right "MathAgape" 20))
(check-catch 'out-of-range (string-drop-right "" 1))
(check-catch 'out-of-range (string-drop-right "Hello" -1))
(check-catch 'wrong-type-arg (string-drop-right 123 4))
(check-catch 'wrong-type-arg (string-drop-right "MathAgape" "4"))
(check-catch 'wrong-type-arg (string-drop-right "MathAgape" 4.5))
(check-catch 'wrong-type-arg (string-drop-right "MathAgape" 'a))

(check (string-drop-right "MathAgape" 5) => "Math")
(check (string-drop-right "MathAgape" 9) => "")
(check (string-drop-right "MathAgape" 0) => "MathAgape")

(check-catch 'out-of-range (string-drop-right "MathAgape" -1))
(check-catch 'out-of-range (string-drop-right "MathAgape" 20))

(check (string-pad-right "MathAgape" 15) => "MathAgape      ")
(check (string-pad-right "MathAgape" 12 #\1) => "MathAgape111")
(check (string-pad-right "MathAgape" 6 #\1 0 4) => "Math11")
(check (string-pad-right "MathAgape" 9) => "MathAgape")
(check (string-pad-right "MathAgape" 9 #\1) => "MathAgape")
(check (string-pad-right "MathAgape" 4) => "Math")
(check (string-pad "MathAgape" 2 #\1 0 4) => "th")

(check-catch 'out-of-range (string-pad-right "MathAgape" -1))

#|
string-pad
在字符串左侧填充字符以达到指定长度。

语法
----
(string-pad str len)
(string-pad str len char)
(string-pad str len char start)
(string-pad str len char start end)

参数
----
str : string?
要填充的源字符串。

len : integer?
目标字符串长度，必须为非负整数。

char : char? 可选
要使用的填充字符，默认为空格字符(#\ )。

start : integer? 可选
子字符串起始位置（包含），默认为0。

end : integer? 可选
子字符串结束位置（不包含），默认为字符串长度。

返回值
----
string
一个新的字符串。
- 当源字符串长度小于len时，在左侧添加指定填充字符以达到len长度。
- 当源字符串长度大于len时，返回从右侧截取的len长度子串。
- 当源字符串长度等于len时，返回源字符串或其子串的副本。

注意
----
string-pad是左填充(left padding)函数，填充字符添加在字符串前面。
对于多字节Unicode字符，操作基于字节位置而非字符位置。

示例
----
(string-pad "abc" 6) => "   abc"
(string-pad "abc" 6 #\0) => "000abc"
(string-pad "abcdef" 3) => "def"
(string-pad "" 5) => "     "
(string-pad "a" 1) => "a"

错误处理
----
out-of-range 当len为负数时
wrong-type-arg 当str不是字符串类型时
|#

(check (string-pad "MathAgape" 15) => "      MathAgape")
(check (string-pad "MathAgape" 12 #\1) => "111MathAgape")
(check (string-pad "MathAgape" 6 #\1 0 4) => "11Math")
(check (string-pad "MathAgape" 9) => "MathAgape")
(check (string-pad "MathAgape" 5) => "Agape")
(check (string-pad "MathAgape" 2 #\1 0 4) => "th")

(check-catch 'out-of-range (string-pad "MathAgape" -1))


; 基本功能测试 - string-pad
(check (string-pad "abc" 6) => "   abc")
(check (string-pad "abc" 6 #\0) => "000abc")
(check (string-pad "abcdef" 3) => "def")
(check (string-pad "abcdef" 3 #\0) => "def")
(check (string-pad "" 5) => "     ")
(check (string-pad "" 5 #\0) => "00000")
(check (string-pad "a" 1) => "a")
(check (string-pad "abc" 3) => "abc")

; 边界情况测试
(check (string-pad "abc" 0) => "")
(check (string-pad "abc" 2) => "bc")
(check (string-pad "abc" 1) => "c")

; 多字节字符测试
(check (string-pad "中文" 6) => "中文")

; 子字符串范围参数测试
(check (string-pad "HelloWorld" 12 #\!) => "!!HelloWorld")
(check (string-pad "HelloWorld" 7 #\! 0 5) => "!!Hello")
(check (string-pad "HelloWorld" 8 #\! 1 6) => "!!!elloW")
(check (string-pad "HelloWorld" 5 #\x 3 5) => "xxxlo")
(check (string-pad "HelloWorld" 0 #\! 3 3) => "")

; 多种填充字符测试
(check (string-pad "abc" 10 #\*) => "*******abc")
(check (string-pad "test" 8 #\-) => "----test")
(check (string-pad "123" 7 #\0) => "0000123")

#|
string-pad-right
在字符串右侧填充字符以达到指定长度。

语法
----
(string-pad-right str len)
(string-pad-right str len char)
(string-pad-right str len char start)
(string-pad-right str len char start end)

参数
----
str : string?
要填充的源字符串。

len : integer?
目标字符串长度，必须为非负整数。

char : char? 可选
要使用的填充字符，默认为空格字符(#\ )。

start : integer? 可选
子字符串起始位置（包含），默认为0。

end : integer? 可选
子字符串结束位置（不包含），默认为字符串长度。

返回值
----
string
一个新的字符串。
- 当源字符串长度小于len时，在右侧添加指定填充字符以达到len长度。
- 当源字符串长度大于len时，返回左侧截取的len长度子串。
- 当源字符串长度等于len时，返回源字符串或其子串的副本。

注意
----
string-pad-right是右填充(right padding)函数，填充字符添加在字符串后面。
对于多字节Unicode字符，操作基于字节位置而非字符位置。

示例
----
(string-pad-right "abc" 6) => "abc   "
(string-pad-right "abc" 6 #\0) => "abc000"
(string-pad-right "abcdef" 3) => "abc"
(string-pad-right "" 5) => "     "
(string-pad-right "a" 1) => "a"

错误处理
----
out-of-range 当len为负数时
wrong-type-arg 当str不是字符串类型时
|#

; 基本功能测试 - string-pad-right
(check (string-pad-right "abc" 6) => "abc   ")
(check (string-pad-right "abc" 6 #\0) => "abc000")
(check (string-pad-right "abcdef" 3) => "abc")
(check (string-pad-right "abcdef" 3 #\0) => "abc")
(check (string-pad-right "" 5) => "     ")
(check (string-pad-right "" 5 #\0) => "00000")
(check (string-pad-right "a" 1) => "a")
(check (string-pad-right "abc" 3) => "abc")

; 边界情况测试
(check (string-pad-right "abc" 0) => "")
(check (string-pad-right "abc" 2) => "ab")
(check (string-pad-right "abc" 1) => "a")

; 多字节字符测试
(check (string-pad-right "中文" 6) => "中文")

; 子字符串范围参数测试
(check (string-pad-right "HelloWorld" 12 #\!) => "HelloWorld!!")
(check (string-pad-right "HelloWorld" 7 #\! 0 5) => "Hello!!")
(check (string-pad-right "HelloWorld" 8 #\! 1 6) => "elloW!!!")
(check (string-pad-right "HelloWorld" 5 #\x 3 5) => "loxxx")
(check (string-pad-right "HelloWorld" 0 #\! 3 3) => "")

; 多种填充字符测试
(check (string-pad-right "abc" 10 #\*) => "abc*******")
(check (string-pad-right "test" 8 #\-) => "test----")
(check (string-pad-right "123" 7 #\0) => "1230000")

; 错误处理测试
(check-catch 'out-of-range (string-pad "abc" -1))
(check-catch 'out-of-range (string-pad-right "abc" -1))

#|
string-trim
从字符串开头移除指定的字符/空白字符。

语法
----
(string-trim str)
(string-trim str char)
(string-trim str pred?)
(string-trim str char/pred? start)
(string-trim str char/pred? start end)

参数
----
str : string?
要处理的源字符串。

char/pred? : char? 或 procedure?
- 字符(char)：指定要从开头移除的字符
- 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值
- 省略时默认为字符空白字符空格(#\ )

start : integer? 可选
起始位置索引（包含），默认为0。

end : integer? 可选
结束位置索引（不包含），默认为字符串长度。

返回值
----
string
一个新的字符串，从开头移除所有连续的指定字符。

注意
----
string-trim会从字符串的左侧（开头）开始移除字符，直到遇到第一个不匹配指定条件的字符为止。
当使用谓词参数时，所有使谓词返回#t的连续字符都会被移除。

对于空字符串，始终返回空字符串。
当字符串以不匹配的字符开头，或字符串为空字符串时，返回原字符串的副本。

示例
----
(string-trim "  hello  ") => "hello  "
(string-trim "---hello---" #\-) => "hello---" 
(string-trim "   hello   ") => "hello   "
(string-trim "123hello123" char-numeric?) => "hello123"
(string-trim "hello") => "hello"
(string-trim "") => ""

错误处理
----
wrong-type-arg 当str不是字符串类型时
wrong-type-arg 当char/pred?不是字符或谓词时
out-of-range 当start/end超出字符串索引范围时
|#

(check (string-trim "  hello  ") => "hello  ")
(check (string-trim "---hello---" #\-) => "hello---")
(check (string-trim "123hello123" char-numeric?) => "hello123")
(check (string-trim "   ") => "")
(check (string-trim "") => "")
(check (string-trim "hello" #\-) => "hello")
(check (string-trim "abcABC123" char-upper-case?) => "abcABC123")
(check (string-trim "  hello  " #\space 2 7) => "hello")
(check (string-trim "   hello   " #\space 3) => "hello   ")
(check (string-trim "   hello   " #\space 3 8) => "hello")
(check (string-trim "---hello---" #\- 3 8) => "hello")
(check (string-trim "123hello123" char-numeric? 3 8) => "hello")
(check (string-trim "123hello123" char-numeric? 3) => "hello123")

#|
string-trim-right
从字符串末尾移除指定的字符/空白字符。

语法
----
(string-trim-right str)
(string-trim-right str char)
(string-trim-right str pred?)
(string-trim-right str char/pred? start)
(string-trim-right str char/pred? start end)

参数
----
str : string?
要处理的源字符串。

char/pred? : char? 或 procedure?
- 字符(char)：指定要从末尾移除的字符
- 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值  
- 省略时默认为字符空白字符空格(#\ )

start : integer? 可选
起始位置索引（包含），默认为0。

end : integer? 可选
结束位置索引（不包含），默认为字符串长度。

返回值
----
string
一个新的字符串，从末尾移除所有连续的指定字符。

注意
----
string-trim-right会从字符串的右侧（末尾）开始移除字符，直到遇到第一个不匹配指定条件的字符为止。
当使用谓词参数时，所有使谓词返回#t的连续字符都会被移除。

对于空字符串，始终返回空字符串。
当字符串以不匹配的字符结尾，或字符串为空字符串时，返回原字符串的副本。

示例
----
(string-trim-right "  hello  ") => "  hello"
(string-trim-right "---hello---" #\-) => "---hello"
(string-trim-right "123hello123" char-numeric?) => "123hello"
(string-trim-right "   ") => ""
(string-trim-right "hello") => "hello"
(string-trim-right "") => ""

错误处理
----
wrong-type-arg 当str不是字符串类型时
wrong-type-arg 当char/pred?不是字符或谓词时
out-of-range 当start/end超出字符串索引范围时
|#

(check (string-trim-right "  hello  ") => "  hello")
(check (string-trim-right "---hello---" #\-) => "---hello")
(check (string-trim-right "123hello123" char-numeric?) => "123hello")
(check (string-trim-right "   ") => "")
(check (string-trim-right "") => "")
(check (string-trim-right "hello" #\-) => "hello")
(check (string-trim-right "abcABC123" char-upper-case?) => "abcABC123")
(check (string-trim-right "  hello  " #\space 2 7) => "hello")
(check (string-trim-right "   hello   " #\space 3) => "hello")
(check (string-trim-right "   hello   " #\space 3 8) => "hello")
(check (string-trim-right "---hello---" #\- 3 8) => "hello")
(check (string-trim-right "123hello123" char-numeric? 3 8) => "hello")
(check (string-trim-right "123hello123" char-numeric? 3) => "hello")

#|
string-trim-both
从字符串开头和末尾同时移除指定的字符/空白字符。

语法
----
(string-trim-both str)
(string-trim-both str char)
(string-trim-both str pred?)
(string-trim-both str char/pred? start)
(string-trim-both str char/pred? start end)

参数
----
str : string?
要处理的源字符串。

char/pred? : char? 或 procedure?
- 字符(char)：指定要从开头和末尾移除的字符
- 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值
- 省略时默认为字符空白字符空格(#\ )

start : integer? 可选
起始位置索引（包含），默认为0。

end : integer? 可选
结束位置索引（不包含），默认为字符串长度。

返回值
----
string
一个新的字符串，从开头和末尾同时移除所有连续的指定字符。

注意
----
string-trim-both会同时从字符串的左侧（开头）和右侧（末尾）移除字符，是string-trim和string-trim-right的组合功能。

当使用谓词参数时，所有使谓词返回#t的连续字符都会被移除。

对于空字符串，始终返回空字符串。

示例
----
(string-trim-both "  hello  ") => "hello"
(string-trim-both "---hello---" #\-) => "hello"
(string-trim-both "123hello123" char-numeric?) => "hello"
(string-trim-both "   ") => ""
(string-trim-both "hello") => "hello"
(string-trim-both "") => ""

错误处理
----
wrong-type-arg 当str不是字符串类型时
wrong-type-arg 当char/pred?不是字符或谓词时
out-of-range 当start/end超出字符串索引范围时
|#

(check (string-trim-both "  hello  ") => "hello")
(check (string-trim-both "---hello---" #\-) => "hello")
(check (string-trim-both "123hello123" char-numeric?) => "hello")
(check (string-trim-both "   ") => "")
(check (string-trim-both "") => "")
(check (string-trim-both "hello" #\-) => "hello")
(check (string-trim-both "abcABC123" char-upper-case?) => "abcABC123")
(check (string-trim-both "  hello  " #\space 2 7) => "hello")
(check (string-trim-both "   hello   " #\space 3) => "hello")
(check (string-trim-both "   hello   " #\space 3 8) => "hello")
(check (string-trim-both "---hello---" #\- 3 8) => "hello")
(check (string-trim-both "123hello123" char-numeric? 3 8) => "hello")
(check (string-trim-both "123hello123" char-numeric? 3) => "hello")

(check (string-prefix? "he" "hello") => #t)
(check (string-prefix? "hello" "hello") => #t)
(check (string-prefix? "" "hello") => #t)
(check (string-prefix? "" "") => #t)
(check (string-prefix? "helloo" "hello") => #f)
(check (string-prefix? "ello" "hello") => #f)

(check (string-suffix? "ello" "hello") => #t)
(check (string-suffix? "hello" "hello") => #t)
(check (string-suffix? "" "hello") => #t)
(check (string-suffix? "" "") => #t)
(check (string-suffix? "helloo" "hello") => #f)
(check (string-suffix? "hhello" "hello") => #f)
(check (string-suffix? "hell" "hello") => #f)

#|
string-index
在字符串中查找指定字符或满足条件的第一个字符的位置。

语法
----
(string-index str char/pred?)
(string-index str char/pred? start)
(string-index str char/pred? start end)

参数
----
str : string?
要搜索的源字符串。

char/pred? : char? 或 procedure?
- 字符(char)：要查找的目标字符
- 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值指示是否匹配

start : integer? 可选
搜索的起始位置(包含)，默认为0。

end : integer? 可选
搜索的结束位置(不包含)，默认为字符串长度。

返回值
----
integer 或 #f
- 如果找到匹配的字符，返回其索引位置(从0开始计数)
- 如果未找到匹配的字符，返回#f

注意
----
string-index从字符串的左侧(开头)开始搜索，返回第一个匹配字符的索引位置。
搜索范围由start和end参数限定。空字符串或未找到匹配项时返回#f。

该函数支持使用字符和谓词两种方式进行查找:
- 字符匹配：查找与指定字符相等的字符
- 谓词匹配：查找使谓词返回#t的第一个字符

示例
----
(string-index "hello" #\e) => 1  (字符'e'在索引1处)
(string-index "hello" #\z) => #f (没有找到字符'z')
(string-index "abc123" char-numeric?) => 3 (第一个数字'1'在索引3处)
(string-index "hello" char-alphabetic?) => 0 (第一个字母'h'在索引0处)
(string-index "hello" #\l 2) => 3 (从索引2开始找前字符'l')
(string-index "hello" #\l 0 2) => #f (在0到2范围内没有找到'l')
(string-index "" #\x) => #f (空字符串返回#f)

错误处理
----
wrong-type-arg 当str不是字符串类型时
wrong-type-arg 当char/pred?不是字符或谓词时
out-of-range 当start/end超出字符串索引范围时
|#

; Basic functionality tests for string-index
(check (string-index "hello" #\e) => 1)
(check (string-index "hello" #\z) => #f)
(check (string-index "hello" #\l) => 2)
(check (string-index "hello" #\l 3) => 3)
(check (string-index "abc123" char-numeric?) => 3)
(check (string-index "abc123" char-alphabetic?) => 0)
(check (string-index "" #\x) => #f)

; Character parameter tests
(check (string-index "0123456789" #\2) => 2)
(check (string-index "0123456789" #\2 2) => 2)
(check (string-index "0123456789" #\2 3) => #f)
(check (string-index "01x3456789" char-alphabetic?) => 2)

; Extended comprehensive string-index tests
(check (string-index "hello" #\h) => 0)
(check (string-index "hello" #\o) => 4)
(check (string-index "hello hello" #\space) => 5)
(check (string-index "hello" #\H) => #f) ; case-sensitive
(check (string-index "" #\a) => #f)
(check (string-index "a" #\a) => 0)
(check (string-index "aaaa" #\a) => 0)
(check (string-index "0123456789" #\0) => 0)
(check (string-index "0123456789" #\9) => 9)

; Predicate parameter tests
(check (string-index "0123456789" char-numeric?) => 0)
(check (string-index "abc123" char-numeric?) => 3)
(check (string-index "123abc" char-alphabetic?) => 3)
(check (string-index "Hello123" char-upper-case?) => 0)
(check (string-index "hello123" char-upper-case?) => #f)
(check (string-index "123!@#" char-alphabetic?) => #f)
(check (string-index " 	
" char-whitespace?) => 0)
(check (string-index "hello" (lambda (c) (char=? c #\l))) => 2)

; Single character edge cases
(check (string-index "a" #\a) => 0)
(check (string-index "a" #\b) => #f)
(check (string-index " " #\space) => 0)
(check (string-index "\t" char-whitespace?) => 0)

; Start and end parameter tests
(check (string-index "hello" #\l 0) => 2)
(check (string-index "hello" #\l 1) => 2)
(check (string-index "hello" #\l 2) => 2)
(check (string-index "hello" #\l 3) => 3)
(check (string-index "hello" #\l 4) => #f)
(check (string-index "hello" #\l 5) => #f)
(check (string-index "hello" #\l 0 3) => 2)
(check (string-index "hello" #\l 0 2) => #f)
(check (string-index "hello" #\l 1 4) => 2)
(check (string-index "hello" #\l 2 4) => 2)
(check (string-index "hello" #\l 3 4) => 3)
(check (string-index "hello" #\l 3 3) => #f)

; Special characters and edge cases
(check (string-index "_test" #\_) => 0)
(check (string-index "a@b" #\@) => 1)
(check (string-index "hello,world" #\,) => 5)
(check (string-index "a-b-c" #\-) => 1)

; Complex predicates
(check (string-index "123abc!@#" (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))) => 0)
(check (string-index "!@#abc123" (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))) => 3)
(check (string-index "abc123" char-upper-case?) => #f)
(check (string-index "ABC123" char-upper-case?) => 0)
(check (string-index "abcABC" char-upper-case?) => 3)

; Empty string and boundary conditions
(check (string-index "" char-alphabetic?) => #f)
(check (string-index "" char-numeric?) => #f)
(check (string-index "abc" char-whitespace?) => #f)
(check (string-index "12345" char-alphabetic?) => #f)

; Error handling tests for string-index
(check-catch 'wrong-type-arg (string-index 123 #\a))
(check-catch 'wrong-type-arg (string-index "hello" "a"))
(check-catch 'wrong-type-arg (string-index "hello" 123))
(check-catch 'wrong-type-arg (string-index "hello" '(a)))
(check-catch 'out-of-range (string-index "hello" #\a -1))
(check-catch 'out-of-range (string-index "hello" #\a 0 6))
(check-catch 'out-of-range (string-index "hello" #\a 3 2))
(check-catch 'out-of-range (string-index "" #\a 1))
(check-catch 'out-of-range (string-index "abc" #\a 5))


(check-true (string-contains "0123456789" "3"))
(check-true (string-contains "0123456789" "34"))
(check-false (string-contains "0123456789" "24"))

#|
string-count
统计字符串中满足指定条件的字符数量。

语法
----
(string-count str char/pred?)
(string-count str char/pred? start)
(string-count str char/pred? start end)

参数
----
str : string?
要搜索的源字符串。

char/pred? : char? 或 procedure?
- 字符(char)：统计字符串中与该字符相等的字符数量
- 谓词(procedure)：接受单个字符作为参数，返回布尔值的函数，统计使谓词返回#t的字符数量

start : integer? 可选
搜索的起始位置(包含)，默认为0。

end : integer? 可选
搜索的结束位置(不包含)，默认为字符串长度。

返回值
----
integer
返回在指定范围内满足条件的字符数量。

注意
----
string-count会从字符串中统计符合指定条件的字符总数，支持单一字符匹配和谓词函数匹配两种模式。
该函数支持start和end参数来限定搜索范围。
对于空字符串或空范围会返回0。

示例
----
(string-count "hello" #\l) => 2            ('l'字符出现2次)
(string-count "hello" char-lower-case?) => 5 (所有字符都是小写)
(string-count "abc123" char-numeric?) => 3   (数字字符出现3次)
(string-count "" #\a) => 0                  (空字符串返回0)
(string-count "hello" #\l 0 3) => 1         (前3个字符中只有1个'l')

错误处理
----
type-error 当str不是字符串类型时
wrong-type-arg 当char/pred?不是字符或谓词时
out-of-range 当start/end超出字符串索引范围时
|#

;; 基本功能测试 - 字符参数
(check (string-count "hello" #\l) => 2)
(check (string-count "hello" #\e) => 1)
(check (string-count "hello" #\z) => 0)
(check (string-count "" #\a) => 0)
(check (string-count "a" #\a) => 1)
(check (string-count "aaa" #\a) => 3)
(check (string-count "aAa" #\a) => 2)  ; case-sensitive
(check (string-count "xyz" #\x) => 1)

;; 谓词参数测试
(check (string-count "123abc" char-numeric?) => 3)
(check (string-count "123abc" char-alphabetic?) => 3)
(check (string-count "hello" char-lower-case?) => 5)
(check (string-count "HELLO" char-upper-case?) => 5)
(check (string-count "Hello1221World" char-upper-case?) => 2)
(check (string-count "Hello1221World" char-lower-case?) => 8)
(check (string-count "   " char-whitespace?) => 3)
(check (string-count "hello world" char-whitespace?) => 1)
(check (string-count "abc123!@#" char-alphabetic?) => 3)
(check (string-count "!@#$%" char-alphabetic?) => 0)

;; 边界条件测试
(check (string-count "" char-numeric?) => 0)
(check (string-count "" char-alphabetic?) => 0)
(check (string-count "" char-whitespace?) => 0)

;; 单个字符边界测试
(check (string-count "a" char-alphabetic?) => 1)
(check (string-count "1" char-numeric?) => 1)
(check (string-count " " char-whitespace?) => 1)

;; 复杂字符组合测试
(check (string-count "a1 b2 c3" char-alphabetic?) => 3)
(check (string-count "a1 b2 c3" char-numeric?) => 3)
(check (string-count "a1 b2 c3" char-whitespace?) => 2)
(check (string-count "method123_doSomething456" char-alphabetic?) => 17)
(check (string-count "method123_doSomething456" char-numeric?) => 6)

;; 特殊字符测试
(check (string-count "特殊abc" char-alphabetic?) => 3)  ; ASCII letters

;; start/end 范围参数测试
(check (string-count "hello world" #\l 0) => 3)
(check (string-count "hello world" #\l 6) => 1)
(check (string-count "hello world" #\l 0 5) => 2)   ; "hello"
(check (string-count "hello world" #\l 6 11) => 1) ; "world"
(check (string-count "hello world" #\l 0 3) => 1)  ; "hel"
(check (string-count "hello world" #\l 4 8) => 0)  ; "o wo" (substring "hello world" 4 8) = "o wo" - no 'l')

;; 谓词与范围组合测试
(check (string-count "abc123ABC" char-lower-case? 0 6) => 3)   ; "abc123" -> 3 lowercase
(check (string-count "abc123ABC" char-upper-case? 3 9) => 3)   ; "123ABC" -> 3 uppercase
(check (string-count "Programming123" char-numeric? 11) => 3)  ; "123"
(check (string-count "123456789" char-numeric? 3 6) => 3)      ; positions 3,4,5 -> "456"

;; 空范围测试
(check (string-count "hello" #\l 0 0) => 0)
(check (string-count "hello" #\l 3 3) => 0)
(check (string-count "hello" #\l 5 5) => 0)
(check (string-count "hello" char-lower-case? 2 2) => 0)

;; 全范围测试
(check (string-count "hello" #\e 0) => 1)
(check (string-count "hello" #\e 0 5) => 1)

;; 自定义谓词测试
(check (string-count "hello world" (lambda (c) (or (char=? c #\l) (char=? c #\o)))) => 5)
(check (string-count "test123" (lambda (c) (or (char=? c #\t) (char=? c #\s) (char=? c #\e)))) => 4)
(check (string-count "SPECIAL#chars" (lambda (c) (not (char-alphabetic? c)))) => 1)  ; # only one special char in "#"

;; 原有测试案例确保向后兼容
(check (string-count "xyz" #\x) => 1)
(check (string-count "xyz" #\x 0 1) => 1)
(check (string-count "xyz" #\y 0 1) => 0)
(check (string-count "xyz" #\x 0 3) => 1)
(check (string-count "xyz" (lambda (x) (char=? x #\x))) => 1)

;; 错误处理测试
(check-catch 'type-error (string-count 123 #\a))
(check-catch 'wrong-type-arg (string-count "hello" 123))
(check-catch 'wrong-type-arg (string-count "hello" "a"))
(check-catch 'wrong-type-arg (string-count "hello" '(a b c)))

;; 参数数量错误测试
(check-catch 'wrong-number-of-args (string-count))
(check-catch 'wrong-number-of-args (string-count "hello"))
(check-catch 'wrong-type-arg (string-count "hello" #\l "invalid"))

;; 范围越界测试
(check-catch 'out-of-range (string-count "hello" #\l -1))
(check-catch 'out-of-range (string-count "hello" #\l 0 10))
(check-catch 'out-of-range (string-count "hello" #\l 5 1))
(check-catch 'out-of-range (string-count "" #\l 1 2))
(check-catch 'out-of-range (string-count "hello" #\l 3 7))

#|
string-upcase
将字符串中的所有小写字母转化为大写字母。

语法
----
(string-upcase str)
(string-upcase str start)
(string-upcase str start end)

参数
----
str : string?
要转换的字符串

start : integer? 可选
transformation的起始位置(包含)，默认为0

end : integer? 可选
transformation的结束位置(不包含)，默认为字符串长度

返回值
----
string
返回将str中指定范围内的小写字母转化为大写字母后的新字符串。

注意
----
仅在ASCII范围内进行大小写转换，非字母字符保持不变。
当前实现对Unicode字符的支持有限。
空字符串会返回空字符串。


错误处理
----
out-of-range 当start/end超出字符串索引范围时
|#

(check (string-upcase "abc") => "ABC")
(check (string-upcase "ABC") => "ABC")
(check (string-upcase "aBc") => "ABC")
(check (string-upcase "123") => "123")
(check (string-upcase "!@#") => "!@#")
(check (string-upcase "abc123xyz") => "ABC123XYZ")
(check (string-upcase "") => "")
(check (string-upcase "中文english123") => "中文ENGLISH123")
(check (string-upcase "mixedUPPERlower123") => "MIXEDUPPERLOWER123")

; 边界情况测试
(check (string-upcase (make-string 0)) => "")
(check (string-upcase (make-string 10 #\a)) => "AAAAAAAAAA")

; 位置参数测试
(check (string-upcase "abcdef" 0 1) => "Abcdef")
(check (string-upcase "abcdef" 0 3) => "ABCdef")
(check (string-upcase "abcdef" 2 4) => "abCDef")
(check (string-upcase "abcdef" 3 (string-length "abcdef")) => "abcDEF")
(check (string-upcase "abcdef" 0 (string-length "abcdef")) => "ABCDEF")
(check (string-upcase "abc" 0) => "ABC")
(check (string-upcase "abc" 1) => "aBC")

; 特殊字符测试
(check (string-upcase "space char space") => "SPACE CHAR SPACE")
(check (string-upcase "tab	newline\nreturn\r") => "TAB\tNEWLINE\nRETURN\r")

; 错误处理测试
(check-catch 'out-of-range (string-upcase "abc" 0 4))
(check-catch 'out-of-range (string-upcase "abc" -1 2))
(check-catch 'out-of-range (string-upcase "abc" 2 1))

#|
string-downcase
将字符串转换为其小写等价形式。

语法
----
(string-downcase str)
(string-downcase str start)
(string-downcase str start end)

参数
----
str : string?
要转换的源字符串。

start : integer? 可选
转换的起始位置(包含)，默认为0。

end : integer? 可选
转换的结束位置(不包含)，默认为字符串长度。

返回值
----
string
返回一个新的字符串，其中str中从start到end的字符被转换为小写形式。

注意
----
string-downcase对字符串中每个大写字符的指定范围应用字符映射转换，使用ASCII字符大小写映射。
只有ASCII范围内的字符会被转换，非ASCII字符保持不变。

当前实现**仅支持ASCII字符范围**内的转换（A-Z→a-z）。
- 非ASCII字符（如中文、拉丁扩展字符、希腊字母等）保持不变
- Unicode复杂字符（如À, Á, Ω等）**不被支持转换**

对于纯ASCII字符串，转换规则很简单：A-Z被映射到a-z。
对于没有ASCII大写字母的字符串，将返回原字符串内容的完整副本。


对于空字符串输入，始终返回空字符串。
对于没有大写字母的字符串，将返回原字符串内容的完整副本。

错误处理
----
out-of-range 当start/end超出字符串索引范围时
wrong-type-arg 当str不是字符串类型时
|#

;;; Basic functionality tests for string-downcase
(check (string-downcase "ABC") => "abc")
(check (string-downcase "abc") => "abc")
(check (string-downcase "ABC123") => "abc123")
(check (string-downcase "123ABC") => "123abc")
(check (string-downcase "Hello World") => "hello world")
(check (string-downcase "!@#$%") => "!@#$%")
(check (string-downcase "MixedCaseString") => "mixedcasestring")
(check (string-downcase "UPPERCASE") => "uppercase")
(check (string-downcase "lowercase") => "lowercase")
(check (string-downcase "CamelCaseISAGoodName") => "camelcaseisagoodname")
(check (string-downcase "") => "")
(check (string-downcase "A") => "a")
(check (string-downcase "Z") => "z")
(check (string-downcase "a1B2c3D4") => "a1b2c3d4")

;;; 验证非ASCII字符保持不变（当前实现只支持基本ASCII）
(check (string-downcase "中文") => "中文")
(check (string-downcase "中文TEST功能") => "中文test功能")
(check (string-downcase "ÀÁÂ") => "ÀÁÂ")
(check (string-downcase "À") => "À")
(check (string-downcase "Á") => "Á")
(check (string-downcase "ÄÖÜ") => "ÄÖÜ")
(check (string-downcase "ΑΒΓ") => "ΑΒΓ")
(check (string-downcase "café") => "café")

;;; Mixed alphanumeric and special characters
(check (string-downcase "ABC-def-GHI") => "abc-def-ghi")
(check (string-downcase "123-ABC-xyz") => "123-abc-xyz")
(check (string-downcase "___ABCDE___") => "___abcde___")
(check (string-downcase ".COM/.NET/.ORG") => ".com/.net/.org")

;;; Edge cases - single character
(check (string-downcase "X") => "x")
(check (string-downcase "x") => "x")
(check (string-downcase "0") => "0")
(check (string-downcase ".") => ".")
(check (string-downcase " ") => " ")

;;; String case variations
(check (string-downcase "ABCDEFGHIJKLMNOPQRSTUVWXYZ") => "abcdefghijklmnopqrstuvwxyz")
(check (string-downcase "abcdefghijklmnopqrstuvwxyz") => "abcdefghijklmnopqrstuvwxyz")
(check (string-downcase "AbCdEfGhIjKlMnOpQrStUvWxYz") => "abcdefghijklmnopqrstuvwxyz")
(check (string-downcase "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") => "0123456789abcdefghijklmnopqrstuvwxyz0123456789")

;;; Boundary conditions
(check (string-downcase "A" 0) => "a")
(check (string-downcase "ABC" 0) => "abc")
(check (string-downcase "ABC" 0 1) => "aBC")
(check (string-downcase "ABC" 0 2) => "abC")
(check (string-downcase "ABC" 0 3) => "abc")
(check (string-downcase "ABC" 1) => "Abc")
(check (string-downcase "ABC" 1 2) => "AbC")
(check (string-downcase "ABC" 1 3) => "Abc")
(check (string-downcase "ABC" 2) => "ABc")
(check (string-downcase "ABC" 2 3) => "ABc")

;;; ASCII boundary verification
(check (string-downcase "aBc" 0 1) => "aBc")
(check (string-downcase "aBc" 0 0) => "aBc") ; no change when start=end

;; Additional boundary tests
(check (string-downcase "A1B2C3D4E5" 0) => "a1b2c3d4e5")
(check (string-downcase "A1B2C3D4E5" 2) => "A1b2c3d4e5")
(check (string-downcase "A1B2C3D4E5" 2 5) => "A1b2c3D4E5")
(check (string-downcase "A1B2C3D4E5" 0 1) => "a1B2C3D4E5")

;;; Spanning over different character types
(check (string-downcase "TEST123" 0 4) => "test123")
(check (string-downcase "TEST123" 2 5) => "TEst123")
(check (string-downcase "TEST123" 4 7) => "TEST123")
(check (string-downcase "aBc123XyZ" 1 7) => "abc123xyZ")

;;; Error handling tests
(check-catch 'out-of-range (string-downcase "ABC" -1))
(check-catch 'out-of-range (string-downcase "ABC" 4))
(check (string-downcase "ABC" 0 0) => "ABC")
(check-catch 'out-of-range (string-downcase "ABC" 0 4))
(check-catch 'out-of-range (string-downcase "ABC" 2 1))  ; start > end
(check-catch 'out-of-range (string-downcase "" 1))
(check-catch 'out-of-range (string-downcase "A" 2))
(check-catch 'out-of-range (string-downcase "ABC" 3 6))
(check-catch 'out-of-range (string-downcase "ABC" 5 7))

;;; Invalid argument type tests
(check-catch 'wrong-type-arg (string-downcase 123))
(check-catch 'wrong-type-arg (string-downcase "hello" "123"))
(check-catch 'wrong-type-arg (string-downcase "hello" 1.5))
(check-catch 'wrong-type-arg (string-downcase "hello" 1 4.5))
(check-catch 'wrong-type-arg (string-downcase 'a))
(check-catch 'wrong-type-arg (string-downcase "hello" 'a 'b))

;;; Long strings and performance considerations
(check (string-downcase (make-string 100 #\A)) => (make-string 100 #\a))
(check (string-downcase (make-string 1000 #\A)) => (make-string 1000 #\a))

;;; Special case consistency
(check (string-downcase "Test" 0 1) => "test")
(check (string-downcase "Test" 1 2) => "Test")
(check (string-downcase "Test" 2 3) => "Test")
(check (string-downcase "Test" 3 4) => "Test")
(check (string-downcase "Test " 0) => "test ")
(check (string-downcase " Test") => " test")

(check (string-downcase "ABC" 0 1) => "aBC")

#|
string-reverse
反转字符串的字符顺序。

语法
----
(string-reverse str)
(string-reverse str start)
(string-reverse str start end)

参数
----
str : string?
要反转的源字符串。

start : integer? 可选
反转操作的起始位置索引（包含），默认为0。

end : integer? 可选
反转操作的结束位置索引（不包含），默认为字符串长度。

返回值
----
string
返回一个新的字符串，其字符顺序与源字符串相反。

注意
----
string-reverse会将字符串中的字符顺序完全颠倒过来。
当指定start和end参数时，仅反转指定范围内的字符，范围外的字符保持不变。

错误处理
----
out-of-range 当start/end超出字符串索引范围时
wrong-type-arg 当str不是字符串类型时
|#

; Basic functionality tests
(check (string-reverse "01234") => "43210")
(check (string-reverse "hello") => "olleh")
(check (string-reverse "hello world") => "dlrow olleh")
(check (string-reverse "abc123") => "321cba")
(check (string-reverse "") => "")
(check (string-reverse "a") => "a")
(check (string-reverse "ab") => "ba")
(check (string-reverse "abc") => "cba")
(check (string-reverse "A1B2C3") => "3C2B1A")

; Single character tests
(check (string-reverse "x") => "x")
(check (string-reverse "1") => "1")
(check (string-reverse "z") => "z")

; Empty string tests
(check (string-reverse "") => "")
(check (string-reverse "" 0) => "")
(check (string-reverse "" 0 0) => "")

; Palindrome tests
(check (string-reverse "racecar") => "racecar")
(check (string-reverse "A man, a plan, a canal, Panama") => "amanaP ,lanac a ,nalp a ,nam A")
(check (string-reverse "aba") => "aba")
(check (string-reverse "abba") => "abba")

; Numeric string tests
(check (string-reverse "1234567890") => "0987654321")
(check (string-reverse "12345") => "54321")
(check (string-reverse "1001") => "1001")

; With start parameter
(check (string-reverse "01234" 0) => "43210")
(check (string-reverse "01234" 1) => "04321")
(check (string-reverse "01234" 2) => "01432")
(check (string-reverse "01234" 3) => "01243")
(check (string-reverse "01234" 4) => "01234")
(check (string-reverse "01234" 5) => "01234")

; With start and end parameters
(check (string-reverse "01234" 0 2) => "10234")
(check (string-reverse "01234" 0 3) => "21034")
(check (string-reverse "01234" 1 3) => "02134")
(check (string-reverse "01234" 1 4) => "03214")
(check (string-reverse "01234" 2 4) => "01324")  ; Correct for byte-level
(check (string-reverse "01234" 0 5) => "43210")
(check (string-reverse "hello" 1 4) => "hlleo")
(check (string-reverse "abcdef" 1 4) => "adcbef")

; Edge case testing
(check (string-reverse "test string" 0 0) => "test string")
(check (string-reverse "test string" 3 3) => "test string")
(check (string-reverse "test string" 11 11) => "test string")
(check (string-reverse "abcdefghij" 5) => "abcdejihgf")
(check (string-reverse "reverse" 2) => "reesrev")

; Null range edge cases
(check (string-reverse "hello" 0 1) => "hello")
(check (string-reverse "hello" 4 5) => "hello")
(check (string-reverse "hello" 1 2) => "hello")
(check (string-reverse "longertext" 8 9) => "longertext")

; Swap two characters
(check (string-reverse "abcd" 0 2) => "bacd")
(check (string-reverse "abcd" 1 3) => "acbd")
(check (string-reverse "abcd" 2 4) => "abdc")

; Full string reverse with parameters
(check (string-reverse "abcdef" 0 (string-length "abcdef")) => "fedcba")
(check (string-reverse "programming" 0 11) => "gnimmargorp")

; UTF-8 multi-byte character support - byte-level operation demonstration
; Note: Limited support as string-reverse is based on byte operations rather than Unicode code points
; Chinese characters: typically 3 bytes (U+4E00-U+9FFF), 4 bytes for extended range
; Emoji: typically 4 bytes per character in modern Unicode

; ASCII character tests (1 byte each, confirming baseline)
(check (string-reverse "a") => "a")
(check (string-reverse "abc") => "cba")

; Verify the byte-level behavior through length preservation
(check (string? (string-reverse "中")) => #t)         ; Returns valid string
(check (= (string-length (string-reverse "中")) (string-length "中")) => #t) ; Preserves length

(check (string? (string-reverse "中文")) => #t)       ; Multi-character Chinese
(check (= (string-length (string-reverse "中文")) (string-length "中文")) => #t)

(check (string? (string-reverse "国")) => #t)         ; Different Chinese character
(check (= (string-length (string-reverse "国")) (string-length "国")) => #t)

; Unicode currency symbols (3 bytes each)
(check (string? (string-reverse "￥")) => #t)         ; Chinese Yuan symbol
(check (= (string-length (string-reverse "￥")) (string-length "￥")) => #t)

; Emoji byte-level behavior (4 bytes each)
(check (string? (string-reverse "🙂")) => #t)         ; Basic emoji
(check (= (string-length (string-reverse "🙂")) (string-length "🙂")) => #t)

(check (string? (string-reverse "👍")) => #t)         ; Thumbs up emoji
(check (= (string-length (string-reverse "👍")) (string-length "👍")) => #t)

(check (string? (string-reverse "🙂👍")) => #t)       ; Multiple emojis
(check (= (string-length (string-reverse "🙂👍")) (string-length "🙂👍")) => #t)

; Mixed content tests showing byte preservation
(check (string? (string-reverse "Hello世界")) => #t)   ; ASCII + Chinese
(check (= (string-length (string-reverse "Hello世界")) (string-length "Hello世界")) => #t)

(check (string? (string-reverse "测试🎉")) => #t)      ; Chinese + emoji
(check (= (string-length (string-reverse "测试🎉")) (string-length "测试🎉")) => #t)

; Error handling tests
(check-catch 'out-of-range (string-reverse "01234" -1))
(check-catch 'out-of-range (string-reverse "01234" 6))
(check-catch 'out-of-range (string-reverse "01234" 5 4))
(check-catch 'out-of-range (string-reverse "01234" 1 6))
(check-catch 'out-of-range (string-reverse "01234" -1 3))
(check-catch 'out-of-range (string-reverse "01234" 3 1))
(check-catch 'out-of-range (string-reverse "" -1))
(check-catch 'out-of-range (string-reverse "test" 0 5))
(check-catch 'out-of-range (string-reverse "" 1))

; Type error handling
(check-catch 'wrong-type-arg (string-reverse 123))
(check-catch 'wrong-type-arg (string-reverse "hello" "not-number"))
(check-catch 'wrong-type-arg (string-reverse "hello" 1.5))
(check-catch 'wrong-type-arg (string-reverse "hello" 1 2.5))

#|
string-map
将给定过程应用于字符串的每个字符，并返回新字符串，包含将过程应用于每个字符的结果。

语法
----
(string-map proc str)

参数
----
proc : procedure?
一个函数，接收单个字符作为参数，返回转换后的字符。

str : string?
要处理的源字符串。

返回值
----
string
返回一个新的字符串，包含将proc应用于str中每个字符后的结果。

注意
----
string-map会创建一个新的字符串，包含将转换过程应用于每个字符的结果。
空字符串会返回空字符串。
谓词函数必须将每个字符映射到新的字符。

示例
----
(string-map char-upcase "hello") => "HELLO"
(string-map char-downcase "WORLD") => "world"
(string-map (lambda (c) (if (char-alphabetic? c) #\X c)) "abc123") => "XXX123"

错误处理
----
wrong-type-arg 当proc不是过程类型时
type-error 当str不是字符串类型时
|#

; Basic functionality tests for string-map
(check (string-map char-upcase "hello world") => "HELLO WORLD")
(check (string-map char-downcase "HELLO WORLD") => "hello world")
(check (string-map char-upcase "") => "")
(check (string-map char-downcase "") => "")
(check (string-map identity "test") => "test")

; 原始测试验证
(check
  (string-map
    (lambda (ch) (integer->char (+ 1 (char->integer ch))))
    "HAL")
  => "IBM")

; Character transformation tests
(check (string-map (lambda (c) (integer->char (- (char->integer c) 32))) "hello") => "HELLO")
(check (string-map (lambda (c) (integer->char (+ (char->integer c) 32))) "HELLO") => "hello")
(check (string-map (lambda (c) (if (char=? c #\a) #\A c)) "banana") => "bAnAnA")
(check (string-map (lambda (c) (if (char-numeric? c) #\X c)) "a1b2c3") => "aXbXcX")
(check (string-map (lambda (c) (if (char-upper-case? c) #\X #\o)) "HeLLo") => "XoXXo")

; Whitespace and special characters
(check (string-map (lambda (c) #\.) "absolute") => "........")
(check (string-map (lambda (c) (if (char-whitespace? c) #\- c)) "hello world") => "hello-world")
(check (string-map (lambda (c) (if (char-alphabetic? c) #\* c)) "test123") => "****123")

; Unicode characters (verification on byte-level)
(check (string-map char-upcase "中文english") => "中文ENGLISH")
(check (string-map (lambda (c) (if (char-alphabetic? c) #\X c)) "abc中文123") => "XXX中文123")

; Empty string handling
(check (string-map char-upcase "") => "")
(check (string-map char-downcase "") => "")
(check (string-map (lambda (c) #\a) "") => "")

; Single character handling
(check (string-map char-upcase "a") => "A")
(check (string-map char-downcase "Z") => "z")
(check (string-map (lambda (c) (integer->char (+ 1 (char->integer c)))) "a") => "b")

; Numeric handling
(check (string-map (lambda (c) (if (char-numeric? c) #\* c)) "123abc") => "***abc")
(check (string-map (lambda (c) (integer->char (+ (char->integer c) 1))) "123") => "234")
(check (string-map (lambda (c) (integer->char (- (char->integer c) 1))) "234") => "123")

; Complex transformations
(check (string-map
          (lambda (c) 
            (if (even? (char->integer c))
                char-upcase
                char-downcase)
            c)
          "AbCdEf") => "AbCdEf")
(check (string-map
          (lambda (c)
            (let ((val (char->integer c)))
              (if (and (>= val 65) (<= val 90))
                  (integer->char (+ val 32))
                  (if (and (>= val 97) (<= val 122))
                      (integer->char (- val 32))
                      c))))
          "Hello123World") => "hELLO123wORLD")


; Mixed case transformations
(check (string-map (lambda (c) (if (char-lower-case? c) (char-upcase c) (char-downcase c))) "HeLLo") => "hEllO")

; Identity function and no-op transformations
(check (string-map (lambda (c) c) "hello") => "hello")
(check (string-map (lambda (c) (if (char=? c #\space) #\space c)) "hello world") => "hello world")


; Whitespace preservation
(check (string-map char-upcase "  hello  world  ") => "  HELLO  WORLD  ")
(check (string-map (lambda (c) (if (char-whitespace? c) #\_ c)) "  hello  world  ") => "__hello__world__")

; Special escape character handling
(check (string-map (lambda (c) #\newline) "test") => 
"\n\n\n\n")
(check (string-map (lambda (c) (integer->char 10)) "abc") => 
"\n\n\n")

; Error handling tests
(check-catch 'wrong-type-arg (string-map 123 "hello"))
(check-catch 'wrong-type-arg (string-map char-upcase 123))
(check-catch 'wrong-type-arg (string-map "not-function" "hello")) 

; Long string handling
(check (string-map char-upcase (make-string 100 #\a)) => (make-string 100 #\A))
(check (string-map char-downcase (make-string 100 #\Z)) => (make-string 100 #\z))

; Unicode string tests - 适应性测试，考虑字符映射对不同Unicode字符集的支持
(check (string-map char-upcase "cafe latte") => "CAFE LATTE")
(check (string-map char-downcase "CAFE LATTE") => "cafe latte")

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (char->integer x) lst)))
      "12345")
    lst)
  => '(53 52 51 50 49))

#|
string-for-each
将给定过程应用于字符串的每个字符，用于副作用操作，不返回有意义的值。

语法
----
(string-for-each proc str)

参数
----
proc : procedure?
一个函数，接收单个字符作为参数，用于副作用处理。

str : string?
要处理的源字符串。

返回值
----
unspecified
未指定返回值，执行只为了副作用。

注意
----
string-for-each与string-map不同，它不产生新字符串，而是对每个字符执行副作用操作。
常用于遍历字符串并对每个字符执行操作，如统计、打印、修改可变状态等。

string-for-each 不支持Unicode字符，按照字节而非字符级别处理字符串。
遇到中文字符等多字节字符会基于UTF-8编码字节进行处理。

错误处理
----
wrong-type-arg 当proc不是过程类型时
type-error 当str不是字符串类型时
|#

; Basic functionality tests for string-for-each
(check
  (let ((result '()))
    (string-for-each (lambda (c) (set! result (cons c result))) "abc")
    result)
  => '(#\c #\b #\a))

(check
  (let ((count 0))
    (string-for-each (lambda (c) (set! count (+ count 1))) "hello")
    count)
  => 5)

(check
  (let ((sum 0))
    (string-for-each 
      (lambda (c) (set! sum (+ sum (char->integer c))))
      "ABC")
    sum)
  => 198) ; 65+66+67

; Empty string handling
(check
  (let ((result 0))
    (string-for-each (lambda (c) (set! result 999)) "")
    result)
  => 0)

; Single character handling
(check
  (let ((char-list '()))
    (string-for-each (lambda (c) (set! char-list (cons c char-list))) "X")
    char-list)
  => '(#\X))

; Special character handling
(check
  (let ((whitespace-count 0))
    (string-for-each
      (lambda (c) (when (char-whitespace? c) (set! whitespace-count (+ whitespace-count 1))))
      "hello world\n")
    whitespace-count)
  => 2)

; Numeric and alphabetic character handling
(check
  (let ((alphas '())
        (digits '()))
    (string-for-each
      (lambda (c)
        (if (char-alphabetic? c)
            (set! alphas (cons c alphas))
            (set! digits (cons c digits))))
      "a1b2c3")
    (list (reverse alphas) (reverse digits)))
  => '((#\a #\b #\c) (#\1 #\2 #\3)))

; Unicode character handling
(check
  (let ((all-chars '()))
    (string-for-each (lambda (c) (set! all-chars (cons c all-chars))) "中文english")
    (> (length all-chars) 8))
  => #t)

; Multiple side effects
(check
  (let ((chars '()) 
        (count 0))
    (string-for-each
      (lambda (c)
        (set! chars (cons c chars))
        (set! count (+ count 1)))
      "test")
    (list (reverse chars) count))
  => '((#\t #\e #\s #\t) 4))

; String mutation tracking
(check
  (let ((tracker (make-string 3 #\a)))
    (string-for-each
      (lambda (c) (set! tracker (string-append tracker (string c))))
      "xyz")
    (> (string-length tracker) 3))
  => #t)

; Error handling tests
(check-catch 'wrong-type-arg (string-for-each 123 "hello"))
(check-catch 'wrong-type-arg (string-for-each (lambda (x) x) 123))
(check-catch 'wrong-type-arg (string-for-each "not-function" "hello"))
(check-catch 'wrong-type-arg (string-for-each char-upcase 123))

; Complex operations
(check
  (let ((ascii-sum 0))
    (string-for-each
      (lambda (c) (set! ascii-sum (+ ascii-sum (char->integer c))))
      "Hello")
    (>= ascii-sum 500))
  => #t)

; Functional conversion tracking
(check
  (let ((upper-chars '()))
    (string-for-each
      (lambda (c) (set! upper-chars (cons (char-upcase c) upper-chars)))
      "abc")
    (reverse upper-chars))
  => '(#\A #\B #\C))

; Very long string processing
(check
  (let ((char-count 0))
    (string-for-each
      (lambda (c) (set! char-count (+ char-count 1)))
      (make-string 1000 #\x))
    char-count)
  => 1000)

; Mixed content handling
(check
  (let ((vowel-count 0))
    (string-for-each
      (lambda (c)
        (when (member c '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
          (set! vowel-count (+ vowel-count 1))))
      "Hello World")
    vowel-count)
  => 3)

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (- (char->integer x) (char->integer #\0)) lst)))
      "12345")
    lst)
  => '(5 4 3 2 1))

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (- (char->integer x) (char->integer #\0)) lst)))
      "123")
    lst)
  => '(3 2 1))

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (- (char->integer x) (char->integer #\0)) lst)))
      "")
    lst)
  => '())

(check (string-fold (lambda (c acc) (+ acc 1)) 0 "hello") => 5)

(check (string-fold (lambda (c acc) (cons c acc)) '() "hello") => '(#\o #\l #\l #\e #\h))

(check (string-fold (lambda (c acc) (string-append (string c) acc)) "" "hello") => "olleh")

(check (string-fold (lambda (c acc)
                      (if (char=? c #\l)
                          (+ acc 1)
                          acc))
                    0
                    "hello")
       => 2)

(check (string-fold (lambda (c acc) (+ acc 1)) 0 "") => 0)

(check-catch 'type-error (string-fold 1 0 "hello"))  ;; 第一个参数不是过程
(check-catch 'type-error (string-fold (lambda (c acc) (+ acc 1)) 0 123))  ;; 第二个参数不是字符串
(check-catch 'out-of-range (string-fold (lambda (c acc) (+ acc 1)) 0 "hello" -1 5))  ;; start 超出范围
(check-catch 'out-of-range (string-fold (lambda (c acc) (+ acc 1)) 0 "hello" 0 6))  ;; end 超出范围
(check-catch 'out-of-range (string-fold (lambda (c acc) (+ acc 1)) 0 "hello" 3 2))  ;; start > end

(check (string-fold (lambda (c acc) (+ acc 1)) 0 "hello" 1 4) => 3)
(check (string-fold (lambda (c acc) (cons c acc)) '() "hello" 1 4) => '(#\l #\l #\e))
(check (string-fold (lambda (c acc) (string-append (string c) acc)) "" "hello" 1 4) => "lle") 

(check (string-fold-right cons '() "abc") => '(#\a #\b #\c))
(check (string-fold-right (lambda (char result) (cons (char->integer char) result)) '() "abc") => '(97 98 99))
(check (string-fold-right (lambda (char result) (+ result (char->integer char))) 0 "abc") => 294)
(check (string-fold-right (lambda (char result) (string-append result (string char))) "" "abc") => "cba")
(check (string-fold-right (lambda (char result) (cons char result)) '() "") => '())
(check (string-fold-right (lambda (char result) (cons char result)) '() "abc" 1) => '(#\b #\c))
(check (string-fold-right (lambda (char result) (cons char result)) '() "abc" 1 2) => '(#\b))
(check-catch 'type-error (string-fold-right 1 '() "abc"))
(check-catch 'type-error (string-fold-right cons '() 123))
(check-catch 'out-of-range (string-fold-right cons '() "abc" 4))
(check-catch 'out-of-range (string-fold-right cons '() "abc" 1 4))

(check
  (string-for-each-index
    (lambda (i c acc)
      (cons (list i c) acc))
    "hello")
  => '((0 #\h) (1 #\e) (2 #\l) (3 #\l) (4 #\o)))

(check
  (string-for-each-index
    (lambda (i c acc)
      (cons (list i c) acc))
    (substring "hello" 1 4))
  => '((0 #\e) (1 #\l) (2 #\l)))

(check
  (list->string
    (reverse
      (string-for-each-index
        (lambda (i c acc)
          (cons c acc))
        "hello")))
  => "olleh")

(check
  (string-for-each-index
    (lambda (i c acc)
      (cons (list i c) acc))
    "")
  => '())

(check-catch 'out-of-range
  (string-for-each-index
   (lambda (i c) (display c))
   "hello" 6))

(check-catch 'out-of-range
  (string-for-each-index
   (lambda (i c) (display c))
   "hello" 0 6))

(check-catch 'out-of-range
  (string-for-each-index
   (lambda (i c) (display c))
   "hello" 3 2))

(check-catch 'type-error
  (string-for-each-index
   (lambda (i c) (display c))
   123))

#|
string-tokenize
将字符串按指定分隔符分割成多个子字符串（标记化）。

语法
----
(string-tokenize str)
(string-tokenize str char)
(string-tokenize str char start)
(string-tokenize str char start end)

参数
----
str : string?
要标记化的源字符串。

char : char? 可选
用作分隔符的字符。省略时默认为空白字符(#\ )。

start : integer? 可选
搜索的起始位置索引（包含），默认为0。

end : integer? 可选
token_type行为的结束位置索引（不包含），默认为字符串长度。

返回值
----
list
返回一个字符串列表，包含由分隔符分割的所有非空子字符串。
分隔符本身不包含在返回的子字符串中。
如果字符串为空或只包含分隔符，返回空列表'()。

注意
----
string-tokenize会从左到右扫描字符串，遇到分隔符时进行分割。
连续的分隔符会被忽略，不会产生空字符串。
对于空字符串输入，返回空列表'()。

示例
----
函数的用法已在上面的测试示例中充分展示。

错误处理
----
wrong-type-arg 当str不是字符串类型时
wrong-type-arg 当char不是字符类型时
out-of-range 当start/end超出字符串索引范围时
|#

; 基本功能测试
(check (string-tokenize "a b c") => '("a" "b" "c"))
(check (string-tokenize "a b c ") => '("a" "b" "c" ""))
(check (string-tokenize " a b c") => '("a" "b" "c"))
(check (string-tokenize "  a  b c  ") => '("a" "b" "c" ""))
(check (string-tokenize "abc") => '("abc"))
(check (string-tokenize "   ") => '(""))
(check (string-tokenize "") => '(""))

; 自定义分隔符测试
(check (string-tokenize "one,two,three" #\,) => '("one" "two" "three"))
(check (string-tokenize "path/to/file" #\/) => '("path" "to" "file"))
(check (string-tokenize "192.168.1.1" #\.) => '("192" "168" "1" "1"))
(check (string-tokenize "hello:::world" #\:) => '("hello" "world"))
(check (string-tokenize "test---case" #\-) => '("test" "case"))

; 边界情况测试
(check (string-tokenize "x") => '("x"))
(check (string-tokenize "x" #\x) => '(""))
(check (string-tokenize "xx") => '("xx"))
(check (string-tokenize "x x") => '("x" "x"))
(check (string-tokenize "x x" #\x) => '(" " ""))

; 特殊字符测试
(check (string-tokenize "hello\tworld\nscheme" #\tab) => '("hello" "world\nscheme"))
(check (string-tokenize "line1\nline2\nline3" #\newline) => '("line1" "line2" "line3"))
(check (string-tokenize "a|b|c|d" #\|) => '("a" "b" "c" "d"))

; 多字符标记测试
(check (string-tokenize "The quick brown fox") => '("The" "quick" "brown" "fox"))
(check (string-tokenize "multiple   spaces   here") => '("multiple" "spaces" "here"))
(check (string-tokenize "comma,separated,values,test" #\,) => '("comma" "separated" "values" "test"))

; 包含start/end参数的测试
(check (string-tokenize "hello world scheme" #\space 6) => '("world" "scheme"))
(check (string-tokenize "hello world scheme" #\space 0 11) => '("hello" "world"))
(check (string-tokenize "hello world scheme" #\space 6 11) => '("world"))
(check (string-tokenize "a,b,c,d" #\, 2) => '("b" "c" "d"))
(check (string-tokenize "a,b,c,d" #\, 0 3) => '("a" "b"))

; start/end边界测试
(check (string-tokenize "test string" #\space 0 4) => '("test"))
(check (string-tokenize "test string" #\space 5 11) => '("string"))
(check (string-tokenize "test string" #\space 5) => '("string"))

; 数字和特殊字符混合测试
(check (string-tokenize "123 456 789") => '("123" "456" "789"))
(check (string-tokenize "file1.txt:file2.txt:file3.txt" #\:) => '("file1.txt" "file2.txt" "file3.txt"))
(check (string-tokenize "user@domain.com;user2@domain.com" #\;) => '("user@domain.com" "user2@domain.com"))

; 连续分隔符测试
(check (string-tokenize "a,,b,,,c" #\,) => '("a" "b" "c"))
(check (string-tokenize "::::" #\:) => '(""))
(check (string-tokenize "a::b" #\:) => '("a" "b"))
(check (string-tokenize "::a::" #\:) => '("a" ""))

; Unicode和多字节字符测试
(check (string-tokenize "中文 测试 功能") => '("中文" "测试" "功能"))

; 错误处理测试
(check-catch 'wrong-type-arg (string-tokenize 123))
(check-catch 'wrong-type-arg (string-tokenize "hello" "not-a-char"))
(check-catch 'wrong-type-arg (string-tokenize "hello" #\h 1.5))
(check-catch 'out-of-range (string-tokenize "hello" #\space -1))
(check-catch 'out-of-range (string-tokenize "hello" #\space 0 10))
(check-catch 'out-of-range (string-tokenize "" #\space 1))
(check-catch 'out-of-range (string-tokenize "test" #\space 5))

; 函数调用和面向对象风格使用示例
(check (let ((s "lisp scheme clojure"))
         (string-tokenize s)) => '("lisp" "scheme" "clojure"))

(check (let ((data "2024-08-07 10:30:00"))
         (string-tokenize data #\- 0 10)) => '("2024" "08" "07"))

#|
string-starts?
检查字符串是否以指定前缀开始。

语法
----
(string-starts? str prefix)

参数
----
str : string?
要检查的源字符串。

prefix : string?
前缀字符串，用于检查str是否以其开始。

返回值
----
boolean
如果str以prefix开头返回#t，否则返回#f。

注意
----
该函数默认使用标准SRFI-13中的string-prefix?实现。
空字符串作为prefix时总是返回#t，因为任何字符串都以空字符串开始。
当prefix长度大于str长度时，string-starts?返回#f。

错误处理
----
type-error 当参数不是字符串类型时。需要两个参数都是字符串；非字符串参数会抛出type-error。
|#

; Basic functionality tests for string-starts?
(check-true (string-starts? "MathAgape" "Ma"))
(check-true (string-starts? "MathAgape" ""))
(check-true (string-starts? "MathAgape" "MathAgape"))
(check-true (string-starts? "" ""))
(check-true (string-starts? "hello" "h"))
(check-true (string-starts? "hello" "he"))
(check-true (string-starts? "hello" "hello"))
(check-true (string-starts? "test123" "test"))
(check-true (string-starts? "中文测试" "中"))
(check-true (string-starts? "空格 测试" "空格"))

; False case tests for string-starts?
(check-false (string-starts? "MathAgape" "a"))
(check-false (string-starts? "hello" "world"))
(check-false (string-starts? "hello" "hello world"))
(check-false (string-starts? "hello" "ello"))
(check-false (string-starts? "hello" "Hello"))
(check-false (string-starts? "test" "test123"))
(check-false (string-starts? "a" "abc"))
(check-false (string-starts? "" "a"))

; Edge cases for string-starts?
(check-true (string-starts? "a" "a"))
(check-true (string-starts? "a" ""))
(check-false (string-starts? "a" "ab"))
(check-true (string-starts? "abc" ""))
(check-false (string-starts? "abc" "abcd"))
(check-true (string-starts? "中文文字" "中"))
(check-true (string-starts? "Mix3d" "Mix"))

; Error handling for string-starts?
(check-catch 'type-error (string-starts? 123 "hello"))
(check-catch 'type-error (string-starts? "hello" 123))
(check-catch 'type-error (string-starts? 'hello "hello"))
(check-catch 'type-error (string-starts? "hello" 'world))
(check-catch 'type-error (string-starts? '(a b c) "hello"))
(check-catch 'type-error (string-starts? "hello" '\n))

#|
string-ends?
检查字符串是否以指定后缀结束。

语法
----
(string-ends? str suffix)

参数
----
str : string?
要检查的源字符串。

suffix : string?
后缀字符串，用于检查str是否以其结束。

返回值
----
boolean
如果str以suffix结尾返回#t，否则返回#f。

注意
----
该函数默认使用标准SRFI-13中的string-suffix?实现。
空字符串作为suffix时总是返回#t，因为任何字符串都以空字符串结束。
当suffix长度大于str长度时，string-ends?返回#f。

错误处理
----
type-error 当参数不是字符串类型时。需要两个参数都是字符串；非字符串参数会抛出type-error。
|#

; Basic functionality tests for string-ends?
(check-true (string-ends? "MathAgape" "e"))
(check-true (string-ends? "MathAgape" ""))
(check-true (string-ends? "MathAgape" "MathAgape"))

(check (string-remove-prefix "浙江省杭州市西湖区" "浙江省") => "杭州市西湖区")
(check (string-remove-prefix "aaa" "a") => "aa")
(check (string-remove-prefix "abc" "bc") => "abc")
(check (string-remove-prefix "abc" "") => "abc")

(check (string-remove-suffix "aaa" "a") => "aa")
(check (string-remove-suffix "aaa" "") => "aaa")
(check (string-remove-suffix "Goldfish.tmu" ".tmu") => "Goldfish")

#|
string-remove-suffix
如果字符串以指定后缀结束，则移除该后缀；否则返回原字符串。

语法
----
(string-remove-suffix str suffix)

参数
----
str : string?
要处理的源字符串。

suffix : string?
要移除的后缀字符串。

返回值
----
string
- 如果str以suffix结尾，返回移除suffix后的新字符串。
- 如果str不以suffix结尾，返回原字符串的副本。
- 如果suffix为空字符串，返回原字符串的副本。

注意
----
string-remove-suffix使用string-suffix?来判断字符串是否以后缀结束。
移除后缀是指将字符串末尾与后缀匹配的部分删除，返回剩余部分。
该函数会返回新的字符串对象，而不是修改原字符串。

示例
----
(string-remove-suffix "filename.txt" ".txt") => "filename"
(string-remove-suffix "test.js" ".py") => "test.js"
(string-remove-suffix "hello world" "world") => "hello "
(string-remove-suffix "test" "") => "test"
(string-remove-suffix "" "test") => ""

错误处理
----
type-error 当参数不是字符串类型时。需要两个参数都是字符串。
|#

; 基本功能测试 - string-remove-suffix
(check (string-remove-suffix "filename.txt" ".txt") => "filename")
(check (string-remove-suffix "test.js" ".js") => "test")
(check (string-remove-suffix "document.pdf" ".pdf") => "document")
(check (string-remove-suffix "hello world" "world") => "hello ")
(check (string-remove-suffix "scheme.scm" ".scm") => "scheme")

; 后缀不匹配的情况
(check (string-remove-suffix "hello.txt" ".js") => "hello.txt")
(check (string-remove-suffix "abcdef" "xyz") => "abcdef")
(check (string-remove-suffix "test" "longsuffix") => "test")

; 空字符串和边界情况
(check (string-remove-suffix "" "") => "")
(check (string-remove-suffix "test" "") => "test")
(check (string-remove-suffix "" "test") => "")

; 单字符测试
(check (string-remove-suffix "a" "a") => "")
(check (string-remove-suffix "a" "b") => "a")
(check (string-remove-suffix "abc" "c") => "ab")

; 相同字符串情况
(check (string-remove-suffix "hello" "hello") => "")
(check (string-remove-suffix "test" "test") => "")

; 多级后缀测试
(check (string-remove-suffix "file.tar.gz" ".gz") => "file.tar")
(check (string-remove-suffix "file.tar.gz" ".tar.gz") => "file")

; 中文和Unicode支持测试
(check (string-remove-suffix "中文文档.txt" ".txt") => "中文文档")
(check (string-remove-suffix "测试文件.json" ".json") => "测试文件")
(check (string-remove-suffix "金鱼缸.tmu" ".tmu") => "金鱼缸")
(check (string-remove-suffix "文件" "文件") => "")

; 目录路径模拟
(check (string-remove-suffix "/path/to/file.txt" ".txt") => "/path/to/file")
(check (string-remove-suffix "C:\\Windows\\test.exe" ".exe") => "C:\\Windows\\test")

; 重复字符模式测试
(check (string-remove-suffix "aaaa" "aa") => "aa")
(check (string-remove-suffix "aaa" "aa") => "a")
(check (string-remove-suffix "aaaa" "aaa") => "a")

; 复杂后缀测试
(check (string-remove-suffix "application.log.backup" ".backup") => "application.log")
(check (string-remove-suffix "data.2024.01.15.csv" ".csv") => "data.2024.01.15")

; 特殊字符测试
(check (string-remove-suffix "test-file_name.backup.suffix" ".suffix") => "test-file_name.backup")
(check (string-remove-suffix "user@domain.com" "@domain.com") => "user")
(check (string-remove-suffix "http://example.com" ".com") => "http://example")

; 数字和字母组合
(check (string-remove-suffix "temp123.tmp" ".tmp") => "temp123")
(check (string-remove-suffix "file2024.log" ".log") => "file2024")

; 多重扩展名顺序
(check (string-remove-suffix "image.png.backup" ".backup") => "image.png")
(check (string-remove-suffix "document.pdf.encrypted" ".encrypted") => "document.pdf")

; 大小写敏感测试（应该区分大小写）
(check (string-remove-suffix "TEST.TXT" ".txt") => "TEST.TXT")
(check (string-remove-suffix "Test.TXT" ".TXT") => "Test")
(check (string-remove-suffix "hello.TXT" ".txt") => "hello.TXT")

; 文件路径测试
(check (string-remove-suffix "filename.tar.gz" ".gz") => "filename.tar")
(check (string-remove-suffix "/var/log/app.log" ".log") => "/var/log/app")
(check (string-remove-suffix "./config.json" ".json") => "./config")

; 错误处理测试 - 参数类型验证
(check-catch 'type-error (string-remove-suffix 123 "test"))
(check-catch 'type-error (string-remove-suffix "test" 123))
(check-catch 'type-error (string-remove-suffix 'symbol "test"))
(check-catch 'type-error (string-remove-suffix "test" '(not-a-string)))
(check-catch 'type-error (string-remove-suffix 123.5 "suffix"))
(check-catch 'type-error (string-remove-suffix "filename" 123.45))
(check-catch 'type-error (string-remove-suffix '(1 2 3) ".txt"))
(check-catch 'type-error (string-remove-suffix "text" #\c))

; 双字节字符边界测试
(check (string-remove-suffix "中文测试文件.txt" ".txt") => "中文测试文件")
(check (string-remove-suffix "中文.json" ".json") => "中文")
(check (string-remove-suffix "引用的文件.js" ".js") => "引用的文件")

; 函数调用和面向对象风格验证
(check (let ((filename "program.c"))
         (string-remove-suffix filename ".c")) => "program")

(check (let ((path "/usr/local/bin/script.py"))
         (string-remove-suffix path ".py")) => "/usr/local/bin/script")

; 确保返回新字符串对象
(let ((original "application.log")
      (modified (string-remove-suffix "application.log" ".log")))
  (check-true (equal? modified "application"))
  (check-false (eq? original modified)))

(check (format #f "~A" 'hello) => "hello")
(check (format #f "~S" 'hello) => "hello")
(check (format #f "~S" "hello") => "\"hello\"")

(check (format #f "~D" 123) => "123")
(check (format #f "~X" 255) => "ff")
(check (format #f "~B" 13) => "1101")
(check (format #f "~O" 13) => "15")

(check (format #f "~E" 100.1) => "1.001000e+02")
(check (format #f "~F" 100.1) => "100.100000")
(check (format #f "~G" 100.1) => "100.1")

(check (format #f "~%") => "\n")
(check (format #f "~~") => "~")

(check (format #f "~{~C~^ ~}" "hiho") => "h i h o")
(check (format #f "~{~{~C~^ ~}~^...~}" (list "hiho" "test"))
       => "h i h o...t e s t")

#|
string-copy
创建字符串的副本，支持可选的开始和结束位置参数进行子串拷贝。

语法
----
(string-copy str)
(string-copy str start)
(string-copy str start end)

参数
----
str : string?
要复制的源字符串。

start : integer? 可选
复制开始的位置索引（包含），默认为0。

end : integer? 可选
复制结束的位置索引（不包含），默认为字符串长度。

返回值
----
string
返回源字符串的深拷贝，与源字符串内容相同但为不同的对象。

注意
----
string-copy创建的是字符串内容的完整副本，即使内容与源字符串相同，
返回的也是新的字符串对象，这一点可以通过eq?函数验证。

与substring函数不同，string-copy始终返回新的字符串对象，
而substring在某些实现中可能会返回源字符串本身（当子串与源字符串相同时）。

start和end参数遵循substring的索引规则，支持负索引和超出范围的索引处理。

错误处理
----
wrong-type-arg 当str不是字符串类型时
out-of-range 当start或end超出字符串索引范围时
out-of-range 当start > end时
|#

; Basic string-copy functionality tests
(check-true (equal? (string-copy "hello") "hello"))
(check-true (equal? (string-copy "hello" 1) "ello"))
(check-true (equal? (string-copy "hello" 1 4) "ell"))
(check-true (equal? (string-copy "") ""))
(check-true (equal? (string-copy "中文测试") "中文测试"))
(check-true (equal? (string-copy "中文测试" 6) "测试"))
(check-true (equal? (string-copy "中文测试" 0 6) "中文"))

(check-true (equal? (string-copy "hello" 0) "hello"))
(check-true (equal? (string-copy "hello" 5) ""))
(check-true (equal? (string-copy "abc" 0 0) ""))
(check-true (equal? (string-copy "abc" 0 1) "a"))
(check-true (equal? (string-copy "abc" 0 2) "ab"))
(check-true (equal? (string-copy "abc" 0 3) "abc"))

; Deep copy verification
(check-false (eq? (string-copy "hello") "hello"))

(let ((original "hello"))
  (check-true (string=? (string-copy original) original))
  (check-false (eq? (string-copy original) original)))

; Substring copy tests
(check-true (equal? (string-copy "test123" 0 4) "test"))
(check-true (equal? (string-copy "test123" 4 7) "123"))

; Unicode and emoji tests
(check-true (equal? (string-copy "🌟🎉" 0 4) "🌟"))
(check-true (equal? (string-copy "🌟🎉" 4 8) "🎉"))

; Error handling tests
(check-catch 'wrong-type-arg (string-copy 123))
(check-catch 'wrong-type-arg (string-copy 'hello))
(check-catch 'out-of-range (string-copy "hello" -1))
(check-catch 'out-of-range (string-copy "hello" 10))
(check-catch 'out-of-range (string-copy "hello" 0 10))
(check-catch 'out-of-range (string-copy "" 1))
(check-catch 'out-of-range (string-copy "hello" 3 2))
(check-catch 'out-of-range (string-copy "hello" 4 3))

(check-catch 'wrong-type-arg (string-copy "hello" "a"))
(check-catch 'wrong-type-arg (string-copy "hello" 1.5))
(check-catch 'wrong-type-arg (string-copy "hello" 1 4.5))

#|
string-fold
 通过从左到右的顺序遍历字符串字符，将给定过程应用于每个字符和累加器值。

语法
----
(string-fold proc knil s)
(string-fold proc knil s start)
(string-fold proc knil s start end)

参数
----
proc : procedure?
  一个函数，接收两个参数：当前字符和当前累加器值，返回新的累加器值。

knil : any
  初始累加器值。

s : string?
  要遍历的源字符串。

start : integer? 可选
  遍历的起始位置（包含），默认为0。

end : integer? 可选
  遍历的结束位置（不包含），默认为字符串长度。

返回值
----
any
  最后一个累加器值，即将proc应用于所有相关字符后的结果。

注意
----
string-fold是一种累加器函数，用于从左到右处理字符串字符。
常用于字符串统计、转换累加或逐步构建复杂结果。
空字符串直接返回初始累加器值knil。
支持可选的start/end参数限定处理范围。

示例
----
统计字符：
(string-fold (lambda (c count) (+ count 1)) 0 "hello") => 5

构建字符列表：
(string-fold cons '() "abc") => '(#\c #\b #\a)

过滤特定字符：
(string-fold 
  (lambda (c acc) (if (char=? c #\l) (+ acc 1) acc))
  0 "hello") => 2

反向累加：
(string-fold (lambda (c str) (string-append str (string c))) "" "abc") => "abc"

范围限定：
(string-fold (lambda (c acc) (+ acc 1)) 0 "hello" 1 4) => 3

累加求和：
(string-fold (lambda (c total) (+ total (char->integer c))) 0 "AB") => 131

错误处理
----
type-error 当proc不是procedure?类型时
wrong-type-arg 当s不是字符串类型时
out-of-range 当start/end超出字符串索引范围或start > end时
|#

#|
string-fold-right
  通过从右到左的顺序遍历字符串字符，将给定过程应用于每个字符和累加器值。

语法
----
(string-fold-right proc knil s)
(string-fold-right proc knil s start)
(string-fold-right proc knil s start end)

参数
----
proc : procedure?
  一个函数，接收两个参数：当前字符和当前累加器值，返回新的累加器值。

knil : any
  初始累加器值。

s : string?
  要遍历的源字符串。

start : integer? 可选
  遍历的起始位置（包含），默认为0。

end : integer? 可选
  遍历的结束位置（不包含），默认为字符串长度。

返回值
----
any
  最后一个累加器值，即将proc应用于所有相关字符后的结果。

注意
----
string-fold-right与string-fold的主要区别在于遍历顺序：
- string-fold: 从左到右（low indices to high）
- string-fold-right: 从右到左（high indices to low）
与常规fold类似，fold-right有时可以提供更自然的右结合构建方式。
常用于需要逆序处理字符串的场景。

示例
----
正向字符收集：
(string-fold-right cons '() "abc") => '(#\a #\b #\c)

反向字符串：
(string-fold-right (lambda (c acc) (string-append (string c) acc)) "" "abc") => "cba"

数学累加：
(string-fold-right (lambda (c acc) (+ acc 1)) 0 "xyz") => 3

范围处理：
(string-fold-right cons '() "abcdef" 2 5) => '(#\c #\d #\e)

与string-fold对比：
(string-fold (lambda (c acc) (cons c acc)) '() "abc") => '(#\c #\b #\a)
(string-fold-right (lambda (c acc) (cons c acc)) '() "abc") => '(#\a #\b #\c)

错误处理
----
type-error 当proc不是procedure?类型时
wrong-type-arg 当s不是字符串类型时
out-of-range 当start/end超出字符串索引范围或start > end时
|#

; === string-fold comprehensive tests ===

; 基本功能测试 - 空字符串
(check (string-fold (lambda (c acc) (+ acc 1)) 0 "") => 0)
(check (string-fold-right (lambda (c acc) (+ acc 1)) 0 "") => 0)

; 基本功能测试 - 简单累加
(check (string-fold (lambda (c acc) (+ acc 1)) 0 "hello") => 5)
(check (string-fold-right (lambda (c acc) (+ acc 1)) 0 "hello") => 5)

; 字符收集测试
(check (string-fold cons '() "abc") => '(#\c #\b #\a))
(check (string-fold-right cons '() "abc") => '(#\a #\b #\c))

; 内容处理测试 - 字符连接方向验证
(check
  (string-fold 
    (lambda (c acc) (string-append acc (string c))) 
    "" 
    "abc")
  => "abc")

(check
  (string-fold-right 
    (lambda (c acc) (string-append acc (string c))) 
    "" 
    "abc")
  => "cba")

; 统计分析测试
(check
  (string-fold
    (lambda (c acc) (if (char=? c #\a) (+ acc 1) acc))
    0
    "banana")
  => 3)

(check
  (string-fold-right
    (lambda (c acc) (if (char=? c #\l) (+ acc 1) acc))
    0
    "hello world")
  => 3)

; ASCII码累加求和
(check
  (string-fold (lambda (c total) (+ total (char->integer c))) 0 "AB")
  => 131) ; 65 + 66

(check
  (string-fold-right (lambda (c total) (+ total (char->integer c))) 0 "AB")
  => 131) ; 65 + 66

; 字符过滤 - 数字
(check
  (string-fold
    (lambda (c acc) 
      (if (char-numeric? c) 
          (cons c acc) 
          acc))
    '()
    "a1b2c3")
  => '(#\3 #\2 #\1))

; 字符分类统计
(check
  (string-fold
    (lambda (c counts)
      (cond
        ((char-alphabetic? c) 
         (list (+ (car counts) 1) (cadr counts) (caddr counts)))
        ((char-numeric? c) 
         (list (car counts) (+ (cadr counts) 1) (caddr counts)))
        (else 
         (list (car counts) (cadr counts) (+ (caddr counts) 1)))))
    '(0 0 0)  ; letters, digits, others
    "hello123!")
  => '(5 3 1))

; start/end 范围参数测试
(check (string-fold (lambda (c acc) (+ acc 1)) 0 "hello" 1 4) => 3)
(check (string-fold-right (lambda (c acc) (+ acc 1)) 0 "hello" 1 4) => 3)

(check (string-fold cons '() "abcdef" 2 5) => '(#\e #\d #\c))
(check (string-fold-right cons '() "abcdef" 2 5) => '(#\c #\d #\e))

; 边界条件测试 - single character
(check (string-fold (lambda (c acc) (+ acc 1)) 0 "a") => 1)
(check (string-fold-right (lambda (c acc) (+ acc 1)) 0 "a") => 1)

; 边界条件测试 - range equals string length
(check (string-fold (lambda (c acc) (+ acc 1)) 0 "test" 0 4) => 4)
(check (string-fold-right (lambda (c acc) (+ acc 1)) 0 "test" 0 4) => 4)

; 极限空范围测试
(check (string-fold (lambda (c acc) (+ acc 1)) 0 "test" 2 2) => 0)
(check (string-fold-right (lambda (c acc) (+ acc 1)) 0 "test" 2 2) => 0)

; 复杂lambda计算测试
(check
  (string-fold
    (lambda (c acc) 
      (+ acc (* (char->integer c) (char->integer c))))
    0
    "AB")
  => 8581) ; 65² + 66²

(check
  (string-fold
    (lambda (c acc)
      (max acc (char->integer c)))
    0
    "ABC")
  => 67) ; max ASCII of A,B,C

; Unicode字符测试
(check 
  (string-fold (lambda (c acc) (+ acc 1)) 0 "中文") 
  => (string-length "中文"))

(check 
  (string-fold-right (lambda (c acc) (+ acc 1)) 0 "测试") 
  => (string-length "测试"))

; 反向构建测试
(check
  (string-fold
    (lambda (c acc) (string-append acc (string (char-upcase c))))
    ""
    "abc")
  => "ABC")

(check
  (string-fold-right
    (lambda (c acc) (string-append acc (string (char-downcase c))))
    ""
    "XYZ")
  => "zyx")

; 多类型累加器 - hand calculation: 104+101+108+108+111 = 532 for "hello"
(check
  (string-fold (lambda (c acc) (+ acc (char->integer c))) 0 "hello")
  => 532)

; === 错误处理测试 ===

; 参数类型错误测试
(check-catch 'type-error (string-fold 123 0 "hello"))
(check-catch 'type-error (string-fold-right 123 0 "hello"))
(check-catch 'type-error (string-fold (lambda (c acc) (+ acc 1)) 0 123))
(check-catch 'type-error (string-fold-right (lambda (c acc) (+ acc 1)) 0 123))
(check-catch 'type-error (string-fold "not-a-proc" 0 "hello"))

; 范围越界测试
(check-catch 'out-of-range (string-fold (lambda (c acc) (+ acc 1)) 0 "hello" -1))
(check-catch 'out-of-range (string-fold-right (lambda (c acc) (+ acc 1)) 0 "hello" -1))
(check-catch 'out-of-range (string-fold (lambda (c acc) (+ acc 1)) 0 "hello" 0 6))
(check-catch 'out-of-range (string-fold-right (lambda (c acc) (+ acc 1)) 0 "hello" 0 6))
(check-catch 'out-of-range (string-fold (lambda (c acc) (+ acc 1)) 0 "hello" 3 2))
(check-catch 'out-of-range (string-fold-right (lambda (c acc) (+ acc 1)) 0 "hello" 3 2))
(check-catch 'out-of-range (string-fold (lambda (c acc) (+ acc 1)) 0 "" 1 2))

#|
string-prefix?

语法
----
(string-prefix? prefix str)

参数
----
prefix : string?
要检查的前缀字符串。

str : string?
要检查的源字符串。

返回值
----
boolean : 如果str以prefix开头返回#t，否则返回#f。

注意
----
空字符串作为prefix时总是返回#t。
当prefix长度大于str长度时，string-prefix?返回#f。

错误处理
----
type-error 当参数不是字符串类型时。
|#

; string-prefix? 作为前缀检查函数的验证测试
(check-true (string-prefix? "" "hello"))
(check-true (string-prefix? "h" "hello"))
(check-true (string-prefix? "he" "hello"))
(check-true (string-prefix? "hel" "hello"))
(check-true (string-prefix? "hell" "hello"))
(check-true (string-prefix? "hello" "hello"))
(check-true (string-prefix? "test" "test123"))
(check-true (string-prefix? "" ""))
(check-true (string-prefix? "a" "a"))
(check-true (string-prefix? "abc" "abc"))

; string-prefix? 边界测试
(check-true (string-prefix? "hello" "hello world"))
(check-true (string-prefix? "hi" "hi there"))

; string-prefix? 返回false的情况
(check-false (string-prefix? "ello" "hello"))
(check-false (string-prefix? "world" "hello"))
(check-false (string-prefix? "x" "hello"))
(check-false (string-prefix? "hello" "hi"))

(check-report)
