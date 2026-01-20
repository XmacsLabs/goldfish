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

(import (liii check)
        (liii either))

(check-set-mode! 'report-failed)

;; ==========================================
;; 基础构造与提取测试
;; ==========================================

;; 测试 from-left 和 to-left
(check (to-left (from-left "error message")) => "error message")
(check (to-left (from-left 42)) => 42)
(check (to-left (from-left '())) => '()) 

;; 测试 from-right 和 to-right
(check (to-right (from-right "success data")) => "success data")
(check (to-right (from-right 100)) => 100)
(check (to-right (from-right '(1 2 3))) => '(1 2 3))

;; ==========================================
;; 谓词测试
;; ==========================================

;; 测试 either-left? 和 either-right?
(let ((left-val (from-left "error"))
      (right-val (from-right "success")))
  (check-true (either-left? left-val))
  (check-false (either-right? left-val))
  (check-true (either-right? right-val))
  (check-false (either-left? right-val)))

;; 注意：新版实现不再将 '() 视为有效的 Either 类型
;; 以下测试用于确认非 Either 类型不会被误判
(check-false (either-left? '()))
(check-false (either-right? '()))
(check-false (either-left? "string"))

;; ==========================================
;; Monad 操作测试 (Map / FlatMap / For-Each)
;; ==========================================

;; 测试 either-map
(let ((left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 map 应该返回原值 (且仍然是左值)
  (check (to-left (either-map (lambda (x) (* x 2)) left-val)) => "error")
  ;; 对右值应用 map 应该应用函数
  (let ((result (either-map (lambda (x) (* x 2)) right-val)))
    (check-true (either-right? result))
    (check (to-right result) => 10)))

;; 测试 either-flat-map
(let ((left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 flat-map 应该返回原值
  (check (to-left (either-flat-map (lambda (x) (from-right (* x 2))) left-val)) => "error")
  ;; 对右值应用 flat-map 应该应用函数
  (let ((result (either-flat-map (lambda (x) (from-right (* x 2))) right-val)))
    (check-true (either-right? result))
    (check (to-right result) => 10)))

;; 测试 either-for-each
(let ((counter 0)
      (left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 for-each 不应该执行副作用
  (either-for-each (lambda (x) (set! counter (+ counter x))) left-val)
  (check counter => 0)
  ;; 对右值应用 for-each 应该执行副作用
  (either-for-each (lambda (x) (set! counter (+ counter x))) right-val)
  (check counter => 5))

;; ==========================================
;; 新增功能测试 (Utility Functions)
;; ==========================================

;; 测试 either-get-or-else
(check (either-get-or-else 0 (from-right 42)) => 42)
(check (either-get-or-else 0 (from-left "error")) => 0)

;; 测试 either-swap
(let ((r (from-right 1))
      (l (from-left 2)))
  ;; Right 变 Left
  (check-true (either-left? (either-swap r)))
  (check (to-left (either-swap r)) => 1)
  ;; Left 变 Right
  (check-true (either-right? (either-swap l)))
  (check (to-right (either-swap l)) => 2))

;; 测试 either-fold
;; 右值应走第二个函数 (* x 2)，左值应走第一个函数 string-length
(check (either-fold string-length (lambda (x) (* x 2)) (from-right 10)) => 20)
(check (either-fold string-length (lambda (x) (* x 2)) (from-left "err")) => 3)

;; 测试 either-or-else
;; 如果是 Right，返回自身；如果是 Left，返回备选方案
(let ((main (from-right 1))
      (backup (from-right 2))
      (fail (from-left 0)))
  (check (to-right (either-or-else backup main)) => 1)
  (check (to-right (either-or-else backup fail)) => 2))

;; ==========================================
;; 模式匹配宏测试
;; ==========================================

;; 测试 either-match 处理 Right
(check (either-match (from-right 100)
         (err 'error-branch)
         (val (+ val 1))) 
       => 101)

;; 测试 either-match 处理 Left
(check (either-match (from-left "fail")
         (err (string-append "Capture: " err))
         (val 'success-branch)) 
       => "Capture: fail")

;; ==========================================
;; 综合流程测试
;; ==========================================

;; 测试嵌套使用
(let* ((val1 (from-right 10))
       (val2 (either-map (lambda (x) (+ x 5)) val1)) ;; Right 15
       (val3 (either-flat-map (lambda (x) (from-right (* x 2))) val2))) ;; Right 30
  (check-true (either-right? val3))
  (check (to-right val3) => 30))

;; 测试错误处理流程 (Short-circuiting)
(let* ((error-val (from-left "network error"))
       ;; 下面的 map 不应执行，因为输入已经是 Left
       (mapped-error (either-map (lambda (x) (string-append "Error: " x)) error-val)))
  (check-true (either-left? mapped-error))
  (check (to-left mapped-error) => "network error"))

(check-report)