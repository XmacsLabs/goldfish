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

(import (liii pretty-print)
        (liii check)
        (liii string)
        (liii pp))

(check-set-mode! 'report-failed)

; 使用 pp 测试基本数据类型
(check (pp 42) => "42")
(check (pp "hello") => "\"hello\"")
(check (pp 'symbol) => "symbol")

; 使用 pp 测试布尔类型
(check (pp #t) => "#t")
(check (pp #f) => "#f")

; 使用 pp 测试数字类型
(check (pp 0) => "0")
(check (pp 42) => "42")
(check (pp -123) => "-123")
(check (pp 3.1416) => "3.1416")

; 使用 pp 测试二进制数字
(check (pp #b1010) => "10")
(check (pp #b0) => "0")
(check (pp #b1111) => "15")

; 使用 pp 测试十六进制数字
(check (pp #xFF) => "255")
(check (pp #x0) => "0")
(check (pp #xABC) => "2748")

; 使用 pp 测试八进制数字
(check (pp #o755) => "493")
(check (pp #o0) => "0")
(check (pp #o644) => "420")

; 使用 pp 测试空列表
(check (pp '()) => "()")

; 使用 pp 测试字符串类型
(check (pp "") => "\"\"")
(check (pp "hello world") => "\"hello world\"")
(check (pp "test string") => "\"test string\"")

; 使用 pp 测试符号类型
(check (pp 'foo) => "foo")
(check (pp 'bar123) => "bar123")

; 使用 pp 测试列表类型
(check (pp '(1 2 3)) => "(1 2 3)")
(check (pp '(a b c)) => "(a b c)")
(check (pp '(1 (2 3) 4)) => "(1 (2 3) 4)")

; 使用 pp 测试向量类型
(check (pp #(1 2 3)) => "#(1 2 3)")
(check (pp #(a b c)) => "#(a b c)")
(check (pp #()) => "#()")

; 使用 pp 测试复合表达式
(check (pp '(+ 1 2 3)) => "(+ 1 2 3)")
(check (pp '(* (+ 1 2) 3)) => "(* (+ 1 2) 3)")
(check (pp '(list 'a 'b 'c)) => "(list 'a 'b 'c)")

; 使用 pp 测试简单的 define 表达式
(check (pp '(define x 10)) => "(define x 10)")
(check (pp '(define x (display 1) 2)) => "(define x (display 1) 2)")

; 使用 pp 测试函数定义（注意：pp 会格式化并添加换行）
(check (pp '(define (f x) (+ x 1))) => "(define (f x)
  (+ x 1))")

; 使用 pp 测试 define-constant
(check (pp '(define-constant PI 3.14159)) => "(define-constant PI 3.14159)")

; 测试 PP_NEWLINE 功能
; PP_NEWLINE(n) 生成 n-2 个换行符
(let ((result (pp '(*PP_NEWLINE* 3))))
  (check (string-length result) => 1)  ; 3-2=1 个换行符
  (check (string-ref result 0) => #\newline))

(let ((result (pp '(*PP_NEWLINE* 4))))
  (check (string-length result) => 2)  ; 4-2=2 个换行符
  (check (string-ref result 0) => #\newline)
  (check (string-ref result 1) => #\newline))

(check (pp '(*PP_NEWLINE* 2)) => "")  ; 2-2=0 个换行符，输出空字符串
(check (pp '(*PP_NEWLINE* 1)) => "")  ; 1-2=-1，但我们的实现中n<2时输出空字符串
(check (pp '(*PP_NEWLINE* 0)) => "")  ; 0-2=-2，输出空字符串

; 测试 PP_MULTI_COMMENT 功能
(let ((result (pp '(*PP_MULTI_COMMENT* "line1" "line2"))))
  (check (string-contains result "#|") => #t)
  (check (string-contains result "|#") => #t)
  (check (string-contains result "line1") => #t)
  (check (string-contains result "line2") => #t))

; 测试 PP_MULTI_COMMENT 空内容
(check (pp '(*PP_MULTI_COMMENT*)) => "#||#")

; 测试 PP_MULTI_COMMENT 单行内容
(check (pp '(*PP_MULTI_COMMENT* "single line")) => "#|single line|#")

; 测试 PP_MULTI_COMMENT 多行内容（内容之间需要换行符）
(check (pp '(*PP_MULTI_COMMENT* "line1" "line2" "line3")) => "#|line1\nline2\nline3|#")

; 测试 PP_MULTI_COMMENT 包含空行
(let ((result (pp '(*PP_MULTI_COMMENT* "line1" "" "line3"))))
  (check (string-contains result "line1") => #t)
  (check (string-contains result "line3") => #t))

; 测试 PP_MULTI_COMMENT 在复杂表达式中的使用
(let ((result (pp '(begin 
                     (display 1) 
                     (*PP_MULTI_COMMENT* "comment1" "comment2") 
                     (display 2)))))
  (check (string-contains result "#|") => #t)
  (check (string-contains result "comment1") => #t)
  (check (string-contains result "comment2") => #t))

; 测试 PP_MULTI_COMMENT 第一个参数是空字符串
(check (pp '(*PP_MULTI_COMMENT* "" "line2")) => "#|\nline2|#")

; 测试 PP_MULTI_COMMENT 最后一个参数是空字符串
(check (pp '(*PP_MULTI_COMMENT* "line1" "")) => "#|line1\n|#")

; 测试 PP_MULTI_COMMENT 两个参数都是空字符串
(check (pp '(*PP_MULTI_COMMENT* "" "")) => "#|\n\n|#")

; 测试 PP_MULTI_COMMENT 第一个参数是空字符串且参数数量>2
(check (pp '(*PP_MULTI_COMMENT* "" "line2" "line3")) => "#|\nline2\nline3|#")

; 测试 PP_MULTI_COMMENT 最后一个参数是空字符串且参数数量>2  
(check (pp '(*PP_MULTI_COMMENT* "line1" "line2" "")) => "#|line1\nline2\n|#")

; 测试 PP_MULTI_COMMENT 中间参数是空字符串且参数数量>2
(check (pp '(*PP_MULTI_COMMENT* "line1" "" "line3")) => "#|line1\n\nline3|#")

; 测试 PP_MULTI_COMMENT 多个空字符串参数
(check (pp '(*PP_MULTI_COMMENT* "" "" "")) => "#|\n\n\n|#")

; 测试 PP_MULTI_COMMENT 参数数量是3，前后都是空字符串，中间有内容
(check (pp '(*PP_MULTI_COMMENT* "" "middle content" "")) => "#|\nmiddle content\n|#")

(check (pp '(*PP_NEWLINE* 0)) => "")  ; 0-2=-2，输出空字符串

; 测试 PP_NEWLINE 在复杂表达式中的使用
(let ((result (pp '(begin 
                     (display 1) 
                     (*PP_NEWLINE* 2) 
                     (display 2)))))
  (check (string-contains result "\n") => #t))  ; 确保结果中包含换行符

; 测试 PP_SINGLE_COMMENT 功能
(check (pp '(*PP_SINGLE_COMMENT* "test comment")) => "; test comment")

; 测试 PP_SINGLE_COMMENT 空内容
(check (pp '(*PP_SINGLE_COMMENT* "")) => ";")

; 测试 PP_SINGLE_COMMENT 有内容
(check (pp '(*PP_SINGLE_COMMENT* "hello world")) => "; hello world")

; 测试 PP_SINGLE_COMMENT 在复杂表达式中的使用
(let ((result (pp '(begin 
                    (display 1) 
                    (*PP_SINGLE_COMMENT* "中间注释") 
                    (display 2)))))
  (display "Complex expression result:\n")
  (display result)
  (newline))

; 测试 PP_SINGLE_COMMENT 长内容（超过 pretty-print-length）
(check (pp '(*PP_SINGLE_COMMENT* "This is a very long comment that should be handled properly by the pretty printer without any issues related to length constraints")) => "; This is a very long comment that should be handled properly by the pretty printer without any issues related to length constraints")

; 测试多个 PP_SINGLE_COMMENT 连续出现（模拟多行注释）
(check (pp '(begin 
              (*PP_SINGLE_COMMENT* "First comment line")
              (*PP_SINGLE_COMMENT* "Second comment line")
              (*PP_SINGLE_COMMENT* "Third comment line")
              (define test 123))) => "(begin\n  ; First comment line\n  ; Second comment line\n  ; Third comment line\n  (define test 123))")

; 测试 PP_SINGLE_COMMENT 包含特殊字符
(check (pp '(*PP_SINGLE_COMMENT* "Comment with special chars: ()[]{}!@#$%^&*")) => "; Comment with special chars: ()[]{}!@#$%^&*")

; 测试具体的长内容注释：guenchi (c) 2018 - 2019 ddd ddd
(check (pp '(*PP_SINGLE_COMMENT* "guenchi (c) 2018 - 2019 ddd ddd")) => "; guenchi (c) 2018 - 2019 ddd ddd")

; 测试包含括号和年份的长注释内容
(check (pp '(*PP_SINGLE_COMMENT* "Copyright (c) 2018 - 2019 guenchi project, all rights reserved")) => "; Copyright (c) 2018 - 2019 guenchi project, all rights reserved")

; 测试混合内容的长注释
(check (pp '(*PP_SINGLE_COMMENT* "guenchi (c) 2018 - 2019 ddd ddd (with additional text) and more content")) => "; guenchi (c) 2018 - 2019 ddd ddd (with additional text) and more content")

; 测试在复杂表达式中的长注释
(check (pp '(begin
              (*PP_SINGLE_COMMENT* "guenchi (c) 2018 - 2019 ddd ddd")
              (define copyright-holder "guenchi")
              (*PP_SINGLE_COMMENT* "Additional copyright notice for the project"))) => "(begin\n  ; guenchi (c) 2018 - 2019 ddd ddd\n  (define copyright-holder \"guenchi\")\n  ; Additional copyright notice for the project)")

(check-report)
