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
        (liii either))

(check-set-mode! 'report-failed)

;; 测试 from-left 和 to-left
(check (to-left (from-left "error message")) => "error message")
(check (to-left (from-left 42)) => 42)
(check (to-left (from-left '())) => '())

;; 测试 from-right 和 to-right
(check (to-right (from-right "success data")) => "success data")
(check (to-right (from-right 100)) => 100)
(check (to-right (from-right '(1 2 3))) => '(1 2 3))

;; 测试 either-left? 和 either-right?
(let ((left-val (from-left "error"))
      (right-val (from-right "success")))
  (check-true (either-left? left-val))
  (check-false (either-right? left-val))
  (check-true (either-right? right-val))
  (check-false (either-left? right-val)))

;; 测试 either-map
(let ((left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 map 应该返回原值
  (check (either-map (lambda (x) (* x 2)) left-val) => left-val)
  ;; 对右值应用 map 应该应用函数
  (let ((result (either-map (lambda (x) (* x 2)) right-val)))
    (check-true (either-right? result))
    (check (to-right result) => 10)))

;; 测试 either-flat-map
(let ((left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 flat-map 应该返回原值
  (check (either-flat-map (lambda (x) (from-right (* x 2))) left-val) => left-val)
  ;; 对右值应用 flat-map 应该应用函数
  (let ((result (either-flat-map (lambda (x) (from-right (* x 2))) right-val)))
    (check-true (either-right? result))
    (check (to-right result) => 10)))

;; 测试 either-for-each
(let ((counter 0)
      (left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 for-each 不应该执行
  (either-for-each (lambda (x) (set! counter (+ counter x))) left-val)
  (check counter => 0)
  ;; 对右值应用 for-each 应该执行
  (either-for-each (lambda (x) (set! counter (+ counter x))) right-val)
  (check counter => 5))

;; 测试边界情况
(check (to-left '()) => '())
(check (to-right '()) => '())
(check-false (either-left? '()))
(check-false (either-right? '()))

;; 测试嵌套使用
(let* ((val1 (from-right 10))
       (val2 (either-map (lambda (x) (+ x 5)) val1))
       (val3 (either-flat-map (lambda (x) (from-right (* x 2))) val2)))
  (check-true (either-right? val3))
  (check (to-right val3) => 30))

;; 测试错误处理流程
(let* ((error-val (from-left "network error"))
       (mapped-error (either-map (lambda (x) (string-append "Error: " x)) error-val)))
  (check-true (either-left? mapped-error))
  (check (to-left mapped-error) => "network error"))

(display "=== Either module tests completed ===\n")
(display "All tests passed!\n")