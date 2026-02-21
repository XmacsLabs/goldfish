(import (liii check)
        (srfi srfi-175))

(check-set-mode! 'report-failed)

#|
ascii-codepoint? / ascii-char? / ascii-bytevector? / ascii-string?

语法
----
(ascii-codepoint? x)
(ascii-char? x)
(ascii-bytevector? x)
(ascii-string? x)

参数
----
x : any

返回值
----
boolean?

说明
----
这组函数用于判断字符/整数是否属于 ASCII 码点范围，
以及字节向量、字符串是否完全由 ASCII 内容构成。
测试覆盖边界值与明显的非 ASCII 输入。
|#

(check-true (ascii-codepoint? 0))
(check-true (ascii-codepoint? #x7f))
(check-false (ascii-codepoint? -1))
(check-false (ascii-codepoint? #x80))
(check-false (ascii-codepoint? #\A))

(check-true (ascii-char? #\A))
(check-true (ascii-char? #\newline))
(check-false (ascii-char? #\x80))
(check-false (ascii-char? 65))

(check-true (ascii-bytevector? #u8()))
(check-true (ascii-bytevector? #u8(0 65 127)))
(check-false (ascii-bytevector? #u8(0 128)))
(check-false (ascii-bytevector? '(65 66)))

(check-true (ascii-string? "Goldfish"))
(check-true (ascii-string? "A\tB\nC"))
(check-false (ascii-string? "G中"))
(check-false (ascii-string? #\A))

#|
ascii-control? / ascii-non-control? / ascii-whitespace? / ascii-space-or-tab?
ascii-other-graphic? / ascii-upper-case? / ascii-lower-case?
ascii-alphabetic? / ascii-alphanumeric? / ascii-numeric?

语法
----
(ascii-control? x)
(ascii-non-control? x)
(ascii-whitespace? x)
(ascii-space-or-tab? x)
(ascii-other-graphic? x)
(ascii-upper-case? x)
(ascii-lower-case? x)
(ascii-alphabetic? x)
(ascii-alphanumeric? x)
(ascii-numeric? x)

参数
----
x : char | integer

返回值
----
boolean?

说明
----
这组函数按 ASCII 子类对输入做布尔判定，
包括控制字符、空白、字母、数字与其他可见符号。
|#

(check-true (ascii-control? 0))
(check-true (ascii-control? #x1f))
(check-true (ascii-control? #x7f))
(check-false (ascii-control? #x20))

(check-true (ascii-non-control? #x20))
(check-true (ascii-non-control? #x7e))
(check-false (ascii-non-control? #x1f))
(check-false (ascii-non-control? #x7f))

(check-true (ascii-whitespace? #\tab))
(check-true (ascii-whitespace? #\newline))
(check-true (ascii-whitespace? #\space))
(check-false (ascii-whitespace? #\A))

(check-true (ascii-space-or-tab? #\space))
(check-true (ascii-space-or-tab? #\tab))
(check-false (ascii-space-or-tab? #\newline))

(check-true (ascii-other-graphic? #\!))
(check-true (ascii-other-graphic? #\{))
(check-false (ascii-other-graphic? #\A))
(check-false (ascii-other-graphic? #\0))

(check-true (ascii-upper-case? #\A))
(check-false (ascii-upper-case? #\a))
(check-true (ascii-lower-case? #\z))
(check-false (ascii-lower-case? #\Z))

(check-true (ascii-alphabetic? #\A))
(check-true (ascii-alphabetic? #\z))
(check-false (ascii-alphabetic? #\0))

(check-true (ascii-alphanumeric? #\0))
(check-true (ascii-alphanumeric? #\G))
(check-false (ascii-alphanumeric? #\-))

(check-true (ascii-numeric? #\0))
(check-true (ascii-numeric? #\9))
(check-false (ascii-numeric? #\a))

#|
ascii-digit-value / ascii-upper-case-value / ascii-lower-case-value
ascii-nth-digit / ascii-nth-upper-case / ascii-nth-lower-case

语法
----
(ascii-digit-value x limit)
(ascii-upper-case-value x offset limit)
(ascii-lower-case-value x offset limit)
(ascii-nth-digit n)
(ascii-nth-upper-case n)
(ascii-nth-lower-case n)

参数
----
x : char | integer
limit : integer
offset : integer
n : integer

返回值
----
ascii-digit-value / ascii-upper-case-value / ascii-lower-case-value:
    integer? | #f
ascii-nth-digit / ascii-nth-upper-case / ascii-nth-lower-case:
    char? | #f

说明
----
这组函数用于在字符与数值语义之间转换：
前半部分把字符映射为数值，后半部分按序号反向生成字符。
|#

(check (ascii-digit-value #\0 10) => 0)
(check (ascii-digit-value #\9 10) => 9)
(check (ascii-digit-value #\9 9) => #f)
(check (ascii-digit-value #\A 10) => #f)

(check (ascii-upper-case-value #\A 10 26) => 10)
(check (ascii-upper-case-value #\F 10 16) => 15)
(check (ascii-upper-case-value #\Q 10 16) => #f)

(check (ascii-lower-case-value #\a 10 26) => 10)
(check (ascii-lower-case-value #\f 10 16) => 15)
(check (ascii-lower-case-value #\q 10 16) => #f)

(check (ascii-nth-digit 0) => #\0)
(check (ascii-nth-digit 9) => #\9)
(check (ascii-nth-digit -1) => #f)
(check (ascii-nth-digit 10) => #f)

(check (ascii-nth-upper-case 0) => #\A)
(check (ascii-nth-upper-case 25) => #\Z)
(check (ascii-nth-upper-case 26) => #\A)

(check (ascii-nth-lower-case 0) => #\a)
(check (ascii-nth-lower-case 25) => #\z)
(check (ascii-nth-lower-case 26) => #\a)

#|
ascii-upcase / ascii-downcase / ascii-control->graphic
ascii-graphic->control / ascii-mirror-bracket

语法
----
(ascii-upcase x)
(ascii-downcase x)
(ascii-control->graphic x)
(ascii-graphic->control x)
(ascii-mirror-bracket x)

参数
----
x : char | integer

返回值
----
ascii-upcase / ascii-downcase:
    与输入同类型（char 或 integer）
ascii-control->graphic / ascii-graphic->control / ascii-mirror-bracket:
    与输入同类型或 #f

说明
----
这组函数处理 ASCII 规则下的字符变换，
包括大小写折叠、控制字符与图形字符互转、括号镜像。
|#

(check (ascii-upcase #\a) => #\A)
(check (ascii-upcase #\A) => #\A)
(check (ascii-upcase #\?) => #\?)
(check (ascii-upcase 97) => 65)

(check (ascii-downcase #\A) => #\a)
(check (ascii-downcase #\a) => #\a)
(check (ascii-downcase #\?) => #\?)
(check (ascii-downcase 65) => 97)

(check (ascii-control->graphic #x00) => #x40)
(check (ascii-control->graphic #x1f) => #x5f)
(check (ascii-control->graphic #x7f) => #x3f)
(check (ascii-control->graphic #\x7f) => #\?)
(check (ascii-control->graphic #x20) => #f)

(check (ascii-graphic->control #x40) => #x00)
(check (ascii-graphic->control #x5f) => #x1f)
(check (ascii-graphic->control #x3f) => #x7f)
(check (ascii-graphic->control #\@) => #\nul)
(check (ascii-graphic->control #\A) => #\x01)
(check (ascii-graphic->control #x20) => #f)

(check (ascii-mirror-bracket #\() => #\))
(check (ascii-mirror-bracket #\]) => #\[)
(check (ascii-mirror-bracket #\>) => #\<)
(check (ascii-mirror-bracket #\A) => #f)
(check (ascii-mirror-bracket 40) => 41)

#|
ascii-ci=? / ascii-ci<? / ascii-ci>? / ascii-ci<=? / ascii-ci>=?
ascii-string-ci=? / ascii-string-ci<? / ascii-string-ci>?
ascii-string-ci<=? / ascii-string-ci>=?

语法
----
(ascii-ci=? char1 char2)
(ascii-ci<? char1 char2)
(ascii-ci>? char1 char2)
(ascii-ci<=? char1 char2)
(ascii-ci>=? char1 char2)
(ascii-string-ci=? string1 string2)
(ascii-string-ci<? string1 string2)
(ascii-string-ci>? string1 string2)
(ascii-string-ci<=? string1 string2)
(ascii-string-ci>=? string1 string2)

参数
----
char1, char2 : char | integer
string1, string2 : string

返回值
----
boolean?

说明
----
这组函数提供大小写无关的比较语义，
分别覆盖单字符与字符串的相等性和顺序关系。
|#

(check-true (ascii-ci=? #\a #\A))
(check-false (ascii-ci=? #\a #\b))
(check-true (ascii-ci<? #\a #\B))
(check-true (ascii-ci>? #\Z #\y))
(check-true (ascii-ci<=? #\A #\a))
(check-true (ascii-ci>=? #\z #\Y))

(check-true (ascii-string-ci=? "GoldFish" "goldfish"))
(check-false (ascii-string-ci=? "goldfish" "gold-fish"))
(check-true (ascii-string-ci<? "abc" "ABD"))
(check-true (ascii-string-ci>? "ABD" "abc"))
(check-true (ascii-string-ci<=? "abc" "ABC"))
(check-true (ascii-string-ci<=? "abc" "abd"))
(check-true (ascii-string-ci>=? "ABD" "abc"))
(check-true (ascii-string-ci>=? "abc" "ABC"))

(check-report)
