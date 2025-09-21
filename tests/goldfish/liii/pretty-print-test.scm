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

; 使用 pp 测试函数定义（注意：pp 会格式化并添加换行）
(check (pp '(define (f x) (+ x 1))) => "(define (f x)
  (+ x 1))")

; 使用 pp 测试 define-constant
(check (pp '(define-constant PI 3.14159)) => "(define-constant PI 3.14159)")

(check-report)
