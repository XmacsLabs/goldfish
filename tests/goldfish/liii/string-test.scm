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
        (liii string)
        (srfi srfi-13)
        (liii error))

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

(check (string-count "xyz" #\x) => 1)
(check (string-count "xyz" #\x 0 1) => 1)
(check (string-count "xyz" #\y 0 1) => 0)
(check (string-count "xyz" #\x 0 3) => 1)
(check (string-count "xyz" (lambda (x) (char=? x #\x))) => 1)

(check (string-upcase "abc") => "ABC")
(check (string-upcase "abc" 0 1) => "Abc")

(check-catch 'out-of-range (string-upcase "abc" 0 4))

(check (string-downcase "ABC") => "abc")
(check (string-downcase "ABC" 0 1) => "aBC")

(check-catch 'out-of-range (string-downcase "ABC" 0 4))

#|
string-reverse
反转字符串中的字符顺序，支持可选的开始和结束位置参数。

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
反转开始的起始位置索引(包含)，默认为0。

end : integer? 可选
反转结束的结束位置索引(不包含)，默认为字符串长度。

返回值
----
string
一个新的字符串，其中指定范围内的字符顺序被反转。
- 如果没有提供start和end参数，则整个字符串被反转
- 如果范围被指定，则只有该范围内的字符被反转

注意
----
string-reverse返回新的字符串对象，不会改变原始字符串。
该函数支持SRFI-13标准的语法，提供了完整的边界检查。
对于空字符串始终返回空字符串。
start和end参数必须符合以下要求：
- 0 <= start <= end <= (string-length str)
- 当end < start时应该触发错误处理机制

示例
----
(string-reverse "hello") => "olleh"
(string-reverse "abc123") => "321cba"
(string-reverse "") => ""
(string-reverse "a") => "a"
(string-reverse "hello" 1) => "h" + "olle" 实际效果的"holle"
(string-reverse "hello" 1 4) => "h" + "lle" + "o" 实际效果的"hlleo"
(string-reverse "01234" 1 3) => "0" + "21" + "34" 实际效果的"02134"

错误处理
----
wrong-type-arg 当str不是字符串类型时
out-of-range 当start或end超出字符串索引范围时
out-of-range 当start > end时
wrong-type-arg 当start或end不是整数类型时
|#

; 基本功能测试
(check (string-reverse "01234") => "43210")
(check (string-reverse "hello") => "olleh")
(check (string-reverse "abc123") => "321cba")
(check (string-reverse "a") => "a")
(check (string-reverse "") => "")

; 单字符和多字节字符测试
(check (string-reverse "中文") => "文中")
(check (string-reverse "🌟🎉") => "🎉🌟")
(check (string-reverse "Test123!@#") => "#@!321tseT")

; 边界情况测试
(check (string-reverse "01234" 0) => "43210")
(check (string-reverse "01234" 1) => "04321")
(check (string-reverse "01234" 4) => "4321")
(check (string-reverse "01234" 5) => "01234")
(check (string-reverse "01234" 0 5) => "43210")
(check (string-reverse "01234" 0 1) => "01234")
(check (string-reverse "01234" 4 5) => "01234")
(check (string-reverse "01234" 0 0) => "01234")
(check (string-reverse "01234" 3 3) => "01234")

; 范围内的字符串反转测试
(check (string-reverse "01234" 1 3) => "02134")
(check (string-reverse "01234" 1 4) => "04321")
(check (string-reverse "01234" 2 4) => "01432")
(check (string-reverse "01234" 0 4) => "43210")

; 空字符串和特殊字符测试
(check (string-reverse "" 0) => "")
(check (string-reverse "" 0 0) => "")
(check (string-reverse "!@#$%^&*()" 2 7) => "!@^%$#*()")
(check (string-reverse "测试用例123abc" 6 12) => "测试cba3216例")

; 长字符串测试
(check (string-reverse "The quick brown fox jumps over the lazy dog") => "god yzal eht revo spmuj xof nworb kciuq ehT")

; 错误处理测试
(check-catch 'wrong-type-arg (string-reverse 123))
(check-catch 'wrong-type-arg (string-reverse 'hello))
(check-catch 'wrong-type-arg (string-reverse "hello" #\a))
(check-catch 'wrong-type-arg (string-reverse "hello" "a"))
(check-catch 'wrong-type-arg (string-reverse "hello" 1.5))
(check-catch 'wrong-type-arg (string-reverse "hello" 1 2.5))
(check-catch 'wrong-type-arg (string-reverse "hello" 1 'a))

(check-catch 'out-of-range (string-reverse "hello" -1))
(check-catch 'out-of-range (string-reverse "hello" 6))
(check-catch 'out-of-range (string-reverse "hello" -1 3))
(check-catch 'out-of-range (string-reverse "hello" 2 6))
(check-catch 'out-of-range (string-reverse "hello" 3 2))
(check-catch 'out-of-range (string-reverse "" 1))
(check-catch 'out-of-range (string-reverse "hello" 0 6))

; 重叠和极端范围测试
(check-catch 'out-of-range (string-reverse "12345" -1 3))
(check-catch 'out-of-range (string-reverse "12345" 5 6))
(check-catch 'out-of-range (string-reverse "12345" 3 1))
(check-catch 'out-of-range (string-reverse "12345" 6 6))

(check
  (string-map
    (lambda (ch) (integer->char (+ 1 (char->integer ch))))
    "HAL")
  => "IBM")

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (char->integer x) lst)))
      "12345")
    lst)
  => '(53 52 51 50 49))

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

(check (string-tokenize "1 22 333") => '("1" "22" "333"))
(check (string-tokenize "1 22 333" #\2) => '("1 " " 333"))
(check (string-tokenize "1 22 333" #\  2) => `("22" "333"))

(check-true (string-starts? "MathAgape" "Ma"))
(check-true (string-starts? "MathAgape" ""))
(check-true (string-starts? "MathAgape" "MathAgape"))

(check-false (string-starts? "MathAgape" "a"))

(check-true (string-ends? "MathAgape" "e"))
(check-true (string-ends? "MathAgape" ""))
(check-true (string-ends? "MathAgape" "MathAgape"))

(check-false (string-ends? "MathAgape" "p"))

(check (string-remove-prefix "浙江省杭州市西湖区" "浙江省") => "杭州市西湖区")
(check (string-remove-prefix "aaa" "a") => "aa")
(check (string-remove-prefix "abc" "bc") => "abc")
(check (string-remove-prefix "abc" "") => "abc")

(check (string-remove-suffix "aaa" "a") => "aa")
(check (string-remove-suffix "aaa" "") => "aaa")
(check (string-remove-suffix "Goldfish.tmu" ".tmu") => "Goldfish")

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

(check-report)

