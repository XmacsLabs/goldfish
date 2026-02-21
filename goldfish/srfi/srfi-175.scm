;
; Copyright (C) 2026 The Goldfish Scheme Authors
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

(define-library (srfi srfi-175)
  (export ascii-codepoint?
          ascii-bytevector?

          ascii-char?
          ascii-string?

          ascii-control?
          ascii-non-control?
          ascii-whitespace?
          ascii-space-or-tab?
          ascii-other-graphic?
          ascii-upper-case?
          ascii-lower-case?
          ascii-alphabetic?
          ascii-alphanumeric?
          ascii-numeric?

          ascii-digit-value
          ascii-upper-case-value
          ascii-lower-case-value
          ascii-nth-digit
          ascii-nth-upper-case
          ascii-nth-lower-case
          ascii-upcase
          ascii-downcase
          ascii-control->graphic
          ascii-graphic->control
          ascii-mirror-bracket

          ascii-ci=?
          ascii-ci<?
          ascii-ci>?
          ascii-ci<=?
          ascii-ci>=?

          ascii-string-ci=?
          ascii-string-ci<?
          ascii-string-ci>?
          ascii-string-ci<=?
          ascii-string-ci>=?)
  (begin
    #|
    ensure-int
    统一将字符输入转换为码点整数。

    语法
    ----
    (ensure-int x)

    参数
    ----
    x : char | integer

    返回值
    ----
    integer?

    说明
    ----
    若 x 为字符，则返回 (char->integer x)；否则原样返回，
    供后续以整数码点逻辑统一处理。
    |#
    (define (ensure-int x)
      (if (char? x) (char->integer x) x))

    #|
    base-offset-limit
    按 base/offset/limit 规则计算字符值映射。

    语法
    ----
    (base-offset-limit x base offset limit)

    参数
    ----
    x : char | integer
    base : integer
    offset : integer
    limit : integer

    返回值
    ----
    integer? | #f

    说明
    ----
    当 x 落在 [base, base+limit) 时，返回 offset + (x-base)；
    否则返回 #f。
    |#
    (define (base-offset-limit x base offset limit)
      (let ((cc (ensure-int x)))
        (and (>= cc base) (< cc (+ base limit))
             (+ offset (- cc base)))))

    #|
    char->int->char
    将整数映射函数提升为字符映射函数。

    语法
    ----
    (char->int->char map-int char)

    参数
    ----
    map-int : procedure
    char : char

    返回值
    ----
    char? | #f

    说明
    ----
    先将 char 转为整数调用 map-int，再将结果整数转回字符；
    若 map-int 返回 #f，则整体返回 #f。
    |#
    (define (char->int->char map-int char)
      (let ((int (map-int (char->integer char))))
        (and int (integer->char int))))

    ;;

    #|
    ascii-codepoint?
    判断是否为 ASCII 码点整数。

    语法
    ----
    (ascii-codepoint? x)

    参数
    ----
    x : any

    返回值
    ----
    boolean?

    说明
    ----
    仅当 x 为精确整数且位于 [0, #x7f] 时返回 #t。
    |#
    (define (ascii-codepoint? x)
      (and (exact-integer? x) (<= 0 x #x7f)))

    #|
    ascii-char?
    判断是否为 ASCII 字符。

    语法
    ----
    (ascii-char? x)

    参数
    ----
    x : any

    返回值
    ----
    boolean?

    说明
    ----
    仅当 x 为字符且其码点小于 #x80 时返回 #t。
    |#
    (define (ascii-char? x)
      (and (char? x) (< (char->integer x) #x80)))

    #|
    ascii-bytevector?
    判断字节向量是否全部由 ASCII 字节构成。

    语法
    ----
    (ascii-bytevector? x)

    参数
    ----
    x : any

    返回值
    ----
    boolean?

    说明
    ----
    仅当 x 为 bytevector，且每个元素都小于 #x80 时返回 #t。
    |#
    (define (ascii-bytevector? x)
      (and (bytevector? x)
           (let check ((i (- (bytevector-length x) 1)))
             (or (< i 0) (and (< (bytevector-u8-ref x i) #x80)
                              (check (- i 1)))))))

    #|
    ascii-string?
    判断字符串是否全部由 ASCII 字符构成。

    语法
    ----
    (ascii-string? x)

    参数
    ----
    x : any

    返回值
    ----
    boolean?

    说明
    ----
    逐字符读取字符串并检查码点是否小于 #x80；若存在非 ASCII
    字符则返回 #f。
    |#
    (define (ascii-string? x)
      (and (string? x)
           (call-with-port
            (open-input-string x)
            (lambda (in)
              (let check ()
                (let ((char (read-char in)))
                  (or (eof-object? char)
                      (and (< (char->integer char) #x80) (check)))))))))

    #|
    ascii-control?
    判断是否为 ASCII 控制字符。

    语法
    ----
    (ascii-control? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    当码点在 [0, #x1f] 或等于 #x7f 时返回 #t。
    |#
    (define (ascii-control? x)
      (let ((cc (ensure-int x)))
        (or (<= 0 cc #x1f) (= cc #x7f))))

    #|
    ascii-non-control?
    判断是否为 ASCII 非控制可见字符。

    语法
    ----
    (ascii-non-control? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    当码点位于 [#x20, #x7e] 时返回 #t。
    |#
    (define (ascii-non-control? x)
      (let ((cc (ensure-int x)))
        (<= #x20 cc #x7e)))

    #|
    ascii-whitespace?
    判断是否为 ASCII 空白字符。

    语法
    ----
    (ascii-whitespace? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    将 TAB/LF/VT/FF/CR/SPACE 视为空白字符。
    |#
    (define (ascii-whitespace? x)
      (let ((cc (ensure-int x)))
        (cond ((< cc #x09) #f)
              ((< cc #x0e) #t)
              (else (= cc #x20)))))

    #|
    ascii-space-or-tab?
    判断是否为空格或制表符。

    语法
    ----
    (ascii-space-or-tab? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    仅当码点为 #x20 或 #x09 时返回 #t。
    |#
    (define (ascii-space-or-tab? x)
      (let ((cc (ensure-int x)))
        (case cc ((#x09 #x20) #t) (else #f))))

    #|
    ascii-other-graphic?
    判断是否属于“其他图形字符”区间。

    语法
    ----
    (ascii-other-graphic? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    匹配 ASCII 标点符号区间，不包含字母和数字。
    |#
    (define (ascii-other-graphic? x)
      (let ((cc (ensure-int x)))
        (or (<= #x21 cc #x2f)
            (<= #x3a cc #x40)
            (<= #x5b cc #x60)
            (<= #x7b cc #x7e))))

    #|
    ascii-upper-case?
    判断是否为 ASCII 大写字母。

    语法
    ----
    (ascii-upper-case? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    当码点位于 [#x41, #x5a]（A-Z）时返回 #t。
    |#
    (define (ascii-upper-case? x)
      (let ((cc (ensure-int x)))
        (<= #x41 cc #x5a)))

    #|
    ascii-lower-case?
    判断是否为 ASCII 小写字母。

    语法
    ----
    (ascii-lower-case? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    当码点位于 [#x61, #x7a]（a-z）时返回 #t。
    |#
    (define (ascii-lower-case? x)
      (let ((cc (ensure-int x)))
        (<= #x61 cc #x7a)))

    #|
    ascii-alphabetic?
    判断是否为 ASCII 字母。

    语法
    ----
    (ascii-alphabetic? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    同时接受 A-Z 与 a-z 两段字母区间。
    |#
    (define (ascii-alphabetic? x)
      (let ((cc (ensure-int x)))
        (or (<= #x41 cc #x5a)
            (<= #x61 cc #x7a))))

    #|
    ascii-alphanumeric?
    判断是否为 ASCII 字母或数字。

    语法
    ----
    (ascii-alphanumeric? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    匹配 0-9、A-Z、a-z 三段字符区间。
    |#
    (define (ascii-alphanumeric? x)
      (let ((cc (ensure-int x)))
        (or (<= #x30 cc #x39)
            (<= #x41 cc #x5a)
            (<= #x61 cc #x7a))))

    #|
    ascii-numeric?
    判断是否为 ASCII 数字字符。

    语法
    ----
    (ascii-numeric? x)

    参数
    ----
    x : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    当码点位于 [#x30, #x39]（0-9）时返回 #t。
    |#
    (define (ascii-numeric? x)
      (let ((cc (ensure-int x)))
        (<= #x30 cc #x39)))

    ;;

    #|
    ascii-digit-value
    获取数字字符的值。

    语法
    ----
    (ascii-digit-value x limit)

    参数
    ----
    x : char | integer
    limit : integer

    返回值
    ----
    integer? | #f

    说明
    ----
    在给定基数上限内，将字符 '0'..'9' 映射到数值；超出范围返回 #f。
    |#
    (define (ascii-digit-value x limit)
      (base-offset-limit x #x30 0 (min limit 10)))

    #|
    ascii-upper-case-value
    获取大写字母字符的值。

    语法
    ----
    (ascii-upper-case-value x offset limit)

    参数
    ----
    x : char | integer
    offset : integer
    limit : integer

    返回值
    ----
    integer? | #f

    说明
    ----
    将大写字母按 A 起点映射到 offset 开始的连续整数。
    |#
    (define (ascii-upper-case-value x offset limit)
      (base-offset-limit x #x41 offset (min limit 26)))

    #|
    ascii-lower-case-value
    获取小写字母字符的值。

    语法
    ----
    (ascii-lower-case-value x offset limit)

    参数
    ----
    x : char | integer
    offset : integer
    limit : integer

    返回值
    ----
    integer? | #f

    说明
    ----
    将小写字母按 a 起点映射到 offset 开始的连续整数。
    |#
    (define (ascii-lower-case-value x offset limit)
      (base-offset-limit x #x61 offset (min limit 26)))

    #|
    ascii-nth-digit
    将 0-9 的整数转换为对应数字字符。

    语法
    ----
    (ascii-nth-digit n)

    参数
    ----
    n : integer

    返回值
    ----
    char? | #f

    说明
    ----
    仅接受 0..9，超出范围返回 #f。
    |#
    (define (ascii-nth-digit n)
      (and (<= 0 n 9) (integer->char (+ #x30 n))))

    #|
    ascii-nth-upper-case
    将整数映射为大写字母字符。

    语法
    ----
    (ascii-nth-upper-case n)

    参数
    ----
    n : integer

    返回值
    ----
    char?

    说明
    ----
    按 26 取模循环映射到 A-Z。
    |#
    (define (ascii-nth-upper-case n)
      (integer->char (+ #x41 (modulo n 26))))

    #|
    ascii-nth-lower-case
    将整数映射为小写字母字符。

    语法
    ----
    (ascii-nth-lower-case n)

    参数
    ----
    n : integer

    返回值
    ----
    char?

    说明
    ----
    按 26 取模循环映射到 a-z。
    |#
    (define (ascii-nth-lower-case n)
      (integer->char (+ #x61 (modulo n 26))))

    #|
    ascii-upcase
    将 ASCII 输入转换为大写形式。

    语法
    ----
    (ascii-upcase x)

    参数
    ----
    x : char | integer

    返回值
    ----
    char? | integer

    说明
    ----
    若 x 为小写字母则转为对应大写；其他字符原样返回，并保持输入类型。
    |#
    (define (ascii-upcase x)
      (if (char? x)
          (integer->char (ascii-upcase (char->integer x)))
          (or (ascii-lower-case-value x #x41 26) x)))

    #|
    ascii-downcase
    将 ASCII 输入转换为小写形式。

    语法
    ----
    (ascii-downcase x)

    参数
    ----
    x : char | integer

    返回值
    ----
    char? | integer

    说明
    ----
    若 x 为大写字母则转为对应小写；其他字符原样返回，并保持输入类型。
    |#
    (define (ascii-downcase x)
      (if (char? x)
          (integer->char (ascii-downcase (char->integer x)))
          (or (ascii-upper-case-value x #x61 26) x)))

    #|
    ascii-control->graphic
    将控制字符码点映射到可见字符区。

    语法
    ----
    (ascii-control->graphic x)

    参数
    ----
    x : char | integer

    返回值
    ----
    char? | integer | #f

    说明
    ----
    将 [0,#x1f] 映射到 [#x40,#x5f]，将 #x7f 映射为 #x3f，
    非控制字符返回 #f。
    |#
    (define (ascii-control->graphic x)
      (if (char? x)
          (char->int->char ascii-control->graphic x)
          (or (and (<= 0 x #x1f) (+ x #x40))
              (and (= x #x7f) #x3f))))

    #|
    ascii-graphic->control
    将特定可见字符码点映射回控制字符区。

    语法
    ----
    (ascii-graphic->control x)

    参数
    ----
    x : char | integer

    返回值
    ----
    char? | integer | #f

    说明
    ----
    将 [#x40,#x5f] 映射回 [0,#x1f]，将 #x3f 映射为 #x7f，
    其他字符返回 #f。
    |#
    (define (ascii-graphic->control x)
      (if (char? x)
          (char->int->char ascii-graphic->control x)
          (or (and (<= #x40 x #x5f) (- x #x40))
              (and (= x #x3f) #x7f))))

    #|
    ascii-mirror-bracket
    返回成对括号的镜像字符。

    语法
    ----
    (ascii-mirror-bracket x)

    参数
    ----
    x : char | integer

    返回值
    ----
    char? | integer | #f

    说明
    ----
    支持 () [] {} <> 四组括号；非括号字符返回 #f，
    并保持输入类型。
    |#
    (define (ascii-mirror-bracket x)
      (if (char? x)
          (case x
            ((#\() #\))
            ((#\)) #\()
            ((#\[) #\])
            ((#\]) #\[)
            ((#\{) #\})
            ((#\}) #\{)
            ((#\<) #\>)
            ((#\>) #\<)
            (else #f))
          (let ((x (ascii-mirror-bracket (integer->char x))))
            (and x (char->integer x)))))

    #|
    ascii-ci-cmp
    大小写无关的字符比较器。

    语法
    ----
    (ascii-ci-cmp char1 char2)

    参数
    ----
    char1, char2 : char | integer

    返回值
    ----
    -1 | 0 | 1

    说明
    ----
    先将 A-Z 归一化为 a-z 后比较，返回三路比较结果。
    |#
    (define (ascii-ci-cmp char1 char2)
      (let ((cc1 (ensure-int char1))
            (cc2 (ensure-int char2)))
        (when (<= #x41 cc1 #x5a) (set! cc1 (+ cc1 #x20)))
        (when (<= #x41 cc2 #x5a) (set! cc2 (+ cc2 #x20)))
        (cond ((< cc1 cc2) -1)
              ((> cc1 cc2) 1)
              (else 0))))

    #|
    ascii-ci=?
    大小写无关字符相等比较。

    语法
    ----
    (ascii-ci=? char1 char2)

    参数
    ----
    char1, char2 : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-ci-cmp 判断是否相等。
    |#
    (define (ascii-ci=? char1 char2)
      (= (ascii-ci-cmp char1 char2) 0))

    #|
    ascii-ci<?
    大小写无关字符“小于”比较。

    语法
    ----
    (ascii-ci<? char1 char2)

    参数
    ----
    char1, char2 : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-ci-cmp 判断是否严格小于。
    |#
    (define (ascii-ci<? char1 char2)
      (< (ascii-ci-cmp char1 char2) 0))

    #|
    ascii-ci>?
    大小写无关字符“大于”比较。

    语法
    ----
    (ascii-ci>? char1 char2)

    参数
    ----
    char1, char2 : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-ci-cmp 判断是否严格大于。
    |#
    (define (ascii-ci>? char1 char2)
      (> (ascii-ci-cmp char1 char2) 0))

    #|
    ascii-ci<=?
    大小写无关字符“小于等于”比较。

    语法
    ----
    (ascii-ci<=? char1 char2)

    参数
    ----
    char1, char2 : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-ci-cmp 判断是否小于或等于。
    |#
    (define (ascii-ci<=? char1 char2)
      (<= (ascii-ci-cmp char1 char2) 0))

    #|
    ascii-ci>=?
    大小写无关字符“大于等于”比较。

    语法
    ----
    (ascii-ci>=? char1 char2)

    参数
    ----
    char1, char2 : char | integer

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-ci-cmp 判断是否大于或等于。
    |#
    (define (ascii-ci>=? char1 char2)
      (>= (ascii-ci-cmp char1 char2) 0))

    #|
    ascii-string-ci-cmp
    大小写无关的字符串三路比较器。

    语法
    ----
    (ascii-string-ci-cmp string1 string2)

    参数
    ----
    string1, string2 : string

    返回值
    ----
    -1 | 0 | 1

    说明
    ----
    按字典序逐字符比较，比较时对 A-Z 做小写归一化，
    并正确处理长度差异。
    |#
    (define (ascii-string-ci-cmp string1 string2)
      (call-with-port
       (open-input-string string1)
       (lambda (in1)
         (call-with-port
          (open-input-string string2)
          (lambda (in2)
            (let loop ()
              (let ((char1 (read-char in1))
                    (char2 (read-char in2)))
                (cond ((eof-object? char1) (if (eof-object? char2) 0 -1))
                      ((eof-object? char2) 1)
                      (else
                       (let ((cc1 (char->integer char1))
                             (cc2 (char->integer char2)))
                         (when (<= #x41 cc1 #x5a) (set! cc1 (+ cc1 #x20)))
                         (when (<= #x41 cc2 #x5a) (set! cc2 (+ cc2 #x20)))
                         (cond ((< cc1 cc2) -1)
                               ((> cc1 cc2) 1)
                               (else (loop)))))))))))))

    #|
    ascii-string-ci=?
    大小写无关字符串相等比较。

    语法
    ----
    (ascii-string-ci=? string1 string2)

    参数
    ----
    string1, string2 : string

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-string-ci-cmp 判断两个字符串是否相等。
    |#
    (define (ascii-string-ci=? string1 string2)
      (= (ascii-string-ci-cmp string1 string2) 0))

    #|
    ascii-string-ci<?
    大小写无关字符串“小于”比较。

    语法
    ----
    (ascii-string-ci<? string1 string2)

    参数
    ----
    string1, string2 : string

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-string-ci-cmp 判断是否严格小于。
    |#
    (define (ascii-string-ci<? string1 string2)
      (< (ascii-string-ci-cmp string1 string2) 0))

    #|
    ascii-string-ci>?
    大小写无关字符串“大于”比较。

    语法
    ----
    (ascii-string-ci>? string1 string2)

    参数
    ----
    string1, string2 : string

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-string-ci-cmp 判断是否严格大于。
    |#
    (define (ascii-string-ci>? string1 string2)
      (> (ascii-string-ci-cmp string1 string2) 0))

    #|
    ascii-string-ci<=?
    大小写无关字符串“小于等于”比较。

    语法
    ----
    (ascii-string-ci<=? string1 string2)

    参数
    ----
    string1, string2 : string

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-string-ci-cmp 判断是否小于或等于。
    |#
    (define (ascii-string-ci<=? string1 string2)
      (<= (ascii-string-ci-cmp string1 string2) 0))

    #|
    ascii-string-ci>=?
    大小写无关字符串“大于等于”比较。

    语法
    ----
    (ascii-string-ci>=? string1 string2)

    参数
    ----
    string1, string2 : string

    返回值
    ----
    boolean?

    说明
    ----
    基于 ascii-string-ci-cmp 判断是否大于或等于。
    |#
    (define (ascii-string-ci>=? string1 string2)
      (>= (ascii-string-ci-cmp string1 string2) 0))
    ))
