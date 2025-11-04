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

(import (liii timeit)
        (liii rich-list)
        (liii lang))

(define (run-rich-list-benchmarks)
  (display "=== Rich List 模块性能基准测试 ===\n\n")

  ; 创建测试数据
  (define test-list (rich-list :range 1 100))
  (define small-list (rich-list :range 1 10))

  ; 测试 rich-list%empty%length 性能
  (let ((time (timeit (lambda () (rich-list :empty :length)) :number 10000)))
    (display* "rich-list%empty%length: \t\t" (number->string time) " 秒\n"))

  ; 测试 rich-list%map 性能
  (let ((time (timeit (lambda () (test-list :map (lambda (x) (* x 2)))) :number 10000)))
    (display* "rich-list%map: \t\t\t" (number->string time) " 秒\n"))

  ; 测试 rich-list%take 性能
  (let ((time (timeit (lambda () (test-list :take 50)) :number 10000)))
    (display* "rich-list%take: \t\t\t" (number->string time) " 秒\n"))

  ; 测试 rich-list%slice 性能
  (let ((time (timeit (lambda () (test-list :slice 20 80)) :number 10000)))
    (display* "rich-list%slice: \t\t\t" (number->string time) " 秒\n"))

  ; 测试 rich-list%exists 性能
  (let ((time (timeit (lambda () (test-list :exists (lambda (x) (> x 50)))) :number 10000)))
    (display* "rich-list%exists: \t\t" (number->string time) " 秒\n"))

  ; 测试 rich-list%reverse 性能
  (let ((time (timeit (lambda () (test-list :reverse)) :number 10000)))
    (display* "rich-list%reverse: \t\t" (number->string time) " 秒\n"))

  ; 测试 rich-list%filter 性能
  (let ((time (timeit (lambda () (test-list :filter even?)) :number 10000)))
    (display* "rich-list%filter: \t\t" (number->string time) " 秒\n"))

  ; 测试 rich-list%fold 性能
  (let ((time (timeit (lambda () (test-list :fold 0 +)) :number 10000)))
    (display* "rich-list%fold: \t\t\t" (number->string time) " 秒\n"))

  ; 测试 rich-list%length 性能
  (let ((time (timeit (lambda () (test-list :length)) :number 10000)))
    (display* "rich-list%length: \t\t" (number->string time) " 秒\n"))

  (display "\n=== 测试完成 ===\n"))

; 运行基准测试
(run-rich-list-benchmarks)
