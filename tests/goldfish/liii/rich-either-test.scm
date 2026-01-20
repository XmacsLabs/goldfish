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
        (liii rich-either)
        (liii option)
        (liii lang)
        (liii error))

(check-set-mode! 'report-failed)

;; ==========================================
;; 基础构造与类型判断测试
;; ==========================================

;; 测试 left 和 right 构造函数
(check-true ((left "error") :left?))
(check-false ((left "error") :right?))

(check-true ((right "success") :right?))
(check-false ((right "success") :left?))

;; 测试 get 方法
(check ((left "error") :get) => "error")
(check ((right 42) :get) => 42)

;; ==========================================
;; or-else 和 get-or-else 测试
;; ==========================================

;; 测试 %or-else
(let ((right-val (right 1))
      (left-val (left 0))
      (backup (right 2)))
  ;; Right 返回自身
  (check ((right-val :or-else backup) :get) => 1)
  ;; Left 返回备选方案
  (check ((left-val :or-else backup) :get) => 2))

;; 测试 %get-or-else
(check ((right 42) :get-or-else 0) => 42)
(check ((left "error") :get-or-else 0) => 0)
;; 测试函数作为默认值
(check ((left "error") :get-or-else (lambda () 99)) => 99)

;; ==========================================
;; filter-or-else 测试
;; ==========================================

;; Right 且满足条件时返回自身
(let ((r (right 10)))
  (check ((r :filter-or-else (lambda (x) (> x 5)) 0) :get) => 10))

;; Right 但不满足条件时返回 left
(let ((r (right 3)))
  (check-true ((r :filter-or-else (lambda (x) (> x 5)) 0) :left?))
  (check ((r :filter-or-else (lambda (x) (> x 5)) 0) :get) => 0))

;; Left 时返回自身
(let ((l (left "error")))
  (check ((l :filter-or-else (lambda (x) #t) 0) :get) => "error"))

;; ==========================================
;; contains 测试
;; ==========================================

(check-true ((right 42) :contains 42))
(check-false ((right 42) :contains 43))
(check-false ((left "error") :contains "error"))


;; ==========================================
;; for-each 测试
;; ==========================================

(let ((counter 0)
      (right-val (right 5))
      (left-val (left "error")))
  ;; Right 执行副作用
  (begin
    (right-val :for-each (lambda (x) (set! counter (+ counter x))))
    (check counter => 5))
  ;; Left 不执行副作用
  (begin
    (left-val :for-each (lambda (x) (set! counter (+ counter 10))))
    (check counter => 5)))

;; ==========================================
;; to-option 测试
;; ==========================================

;; Right 转换为 defined option
(let ((opt ((right 42) :to-option)))
  (check-true (opt :defined?))
  (check (opt :get) => 42))

;; Left 转换为 empty option
(let ((opt ((left "error") :to-option)))
  (check-true (opt :empty?)))

;; ==========================================
;; map 测试
;; ==========================================

;; 对 Right 应用 map
(let ((result ((right 5) :map (lambda (x) (* x 2)))))
  (check-true (result :right?))
  (check (result :get) => 10))

;; 对 Left 应用 map 返回自身
(let ((l (left "error")))
  (check ((l :map (lambda (x) (string-append "Mapped: " x))) :get) => "error"))

;; ==========================================
;; flat-map 测试
;; ==========================================

;; 对 Right 应用 flat-map
(let ((result ((right 5) :flat-map (lambda (x) (right (* x 2))))))
  (check-true (result :right?))
  (check (result :get) => 10))

;; 对 Left 应用 flat-map 返回自身
(let ((l (left "error")))
  (check ((l :flat-map (lambda (x) (right (string-length x)))) :get) => "error"))

;; ==========================================
;; forall 和 exists 测试
;; ==========================================

;; forall: Right 且满足条件时为真
(check-true ((right 10) :forall (lambda (x) (> x 5))))
(check-false ((right 3) :forall (lambda (x) (> x 5))))
;; forall: Left 总是为真
(check-true ((left "error") :forall (lambda (x) #f)))

;; exists: Right 且满足条件时为真
(check-true ((right 10) :exists (lambda (x) (> x 5))))
(check-false ((right 3) :exists (lambda (x) (> x 5))))
;; exists: Left 总是为假
(check-false ((left "error") :exists (lambda (x) #t)))

;; ==========================================
;; 类型兼容性测试
;; ==========================================

;; 测试 either 是 rich-either 的别名
(check-true (either :is-type-of (left "test")))
(check-true (either :is-type-of (right "test")))

;; ==========================================
;; 错误处理测试
;; ==========================================

;; 测试 %or-else 参数类型检查
(check-catch 'type-error ((right 1) :or-else "not-an-either"))

;; 测试 %filter-or-else 参数类型检查
(check-catch 'type-error ((right 1) :filter-or-else "not-a-procedure" 0))

;; 测试 %forall 参数类型检查
(check-catch 'type-error ((right 1) :forall "not-a-procedure"))

;; 测试 %exists 参数类型检查
(check-catch 'type-error ((right 1) :exists "not-a-procedure"))

;; ==========================================
;; 综合流程测试
;; ==========================================

;; 测试链式操作
(let* ((val1 (right 10))
       (val2 (val1 :map (lambda (x) (+ x 5)))) ;; Right 15
       (val3 (val2 :flat-map (lambda (x) (right (* x 2)))))) ;; Right 30
  (check-true (val3 :right?))
  (check (val3 :get) => 30))

;; 测试错误处理流程
(let* ((error-val (left "network error"))
       ;; 下面的 map 不应执行，因为输入已经是 Left
       (mapped-error (error-val :map (lambda (x) (string-append "Error: " x)))))
  (check-true (mapped-error :left?))
  (check (mapped-error :get) => "network error"))

(check-report)