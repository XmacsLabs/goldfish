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

(check-true (string-null? ""))

(check-false (string-null? "MathAgape"))

(check-false (string-null? 'not-a-string))

(check-true (string-every #\x "xxxxxx"))
(check-false (string-every #\x "xxx0xx"))

(check-true (string-every char-numeric? "012345"))
(check-false (string-every char-numeric? "012d45"))

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

(check-catch 'out-of-range (string-every char-numeric? "ab234f" 2 7))
(check-catch 'out-of-range (string-every char-numeric? "ab234f" 2 1))

(check-true (string-any #\0 "xxx0xx"))
(check-false (string-any #\0 "xxxxxx"))
(check-true (string-any char-numeric? "xxx0xx"))
(check-false (string-any char-numeric? "xxxxxx"))

(check-catch 'wrong-type-arg (string-every 0 "xxx0xx"))
(check-catch 'wrong-type-arg (string-any (lambda (n) (= n 0)) "xxx0xx"))
(check-catch 'wrong-type-arg (string-every "0" "xxx0xx"))

(check-true (string-any char-alphabetic? "01c345" 2))
(check-false (string-any char-alphabetic? "01c345" 3))
(check-true (string-any char-alphabetic? "01c345" 2 4))
(check-false (string-any char-alphabetic? "01c345" 2 2))
(check-false (string-any char-alphabetic? "01c345" 3 4))
(check-true (string-any char-alphabetic? "01c345" 2 6))

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

(check (string-take "MathAgape" 4) => "Math")

(check-catch 'out-of-range (string-take "MathAgape" 20))

(check (string-take-right "MathAgape" 0) => "")
(check (string-take-right "MathAgape" 1) => "e")
(check (string-take-right "MathAgape" 9) => "MathAgape")

(check-catch 'out-of-range (string-take-right "MathAgape" 20))

(check (string-drop "MathAgape" 8) => "e")
(check (string-drop "MathAgape" 9) => "")
(check (string-drop "MathAgape" 0) => "MathAgape")

(check-catch 'out-of-range (string-drop "MahtAgape" -1))
(check-catch 'out-of-range (string-drop "MathAgape" 20))

(check (string-drop-right "MathAgape" 5) => "Math")
(check (string-drop-right "MathAgape" 9) => "")
(check (string-drop-right "MathAgape" 0) => "MathAgape")

(check-catch 'out-of-range (string-drop-right "MathAgape" -1))
(check-catch 'out-of-range (string-drop-right "MathAgape" 20))

(check (string-pad "MathAgape" 15) => "      MathAgape")
(check (string-pad "MathAgape" 12 #\1) => "111MathAgape")
(check (string-pad "MathAgape" 6 #\1 0 4) => "11Math")
(check (string-pad "MathAgape" 9) => "MathAgape")
(check (string-pad "MathAgape" 5) => "Agape")
(check (string-pad "MathAgape" 2 #\1 0 4) => "th")

(check-catch 'out-of-range (string-pad "MathAgape" -1))

(check (string-pad-right "MathAgape" 15) => "MathAgape      ")
(check (string-pad-right "MathAgape" 12 #\1) => "MathAgape111")
(check (string-pad-right "MathAgape" 6 #\1 0 4) => "Math11")
(check (string-pad-right "MathAgape" 9) => "MathAgape")
(check (string-pad-right "MathAgape" 9 #\1) => "MathAgape")
(check (string-pad-right "MathAgape" 4) => "Math")
(check (string-pad "MathAgape" 2 #\1 0 4) => "th")

(check-catch 'out-of-range (string-pad-right "MathAgape" -1))

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

(check (string-index "0123456789" #\2) => 2)
(check (string-index "0123456789" #\2 2) => 2)
(check (string-index "0123456789" #\2 3) => #f)
(check (string-index "01x3456789" char-alphabetic?) => 2)

(check (string-index-right "0123456789" #\8) => 8)
(check (string-index-right "0123456789" #\8 2) => 8)
(check (string-index-right "0123456789" #\8 9) => #f)
(check (string-index-right "01234567x9" char-alphabetic?) => 8)

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

(check (string-reverse "01234") => "43210")

(check-catch 'out-of-range (string-reverse "01234" -1))

(check (string-reverse "01234" 0) => "43210")
(check (string-reverse "01234" 1) => "04321")
(check (string-reverse "01234" 5) => "01234")

(check-catch 'out-of-range (string-reverse "01234" 6))

(check (string-reverse "01234" 0 2) => "10234")
(check (string-reverse "01234" 1 3) => "02134")
(check (string-reverse "01234" 1 5) => "04321")
(check (string-reverse "01234" 0 5) => "43210")

(check-catch 'out-of-range (string-reverse "01234" 1 6))

(check-catch 'out-of-range (string-reverse "01234" -1 3))

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

(check-report)

