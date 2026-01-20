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

(define-library (liii either)
  (import (liii base))
  (export from-left to-left
          from-right to-right
          either-left? either-right?
          either-map either-flat-map either-for-each)
  (begin

    ;; 创建左值（错误情况）
    (define (from-left message)
      (cons message 'empty-right))

    ;; 从 either 中提取左值
    (define (to-left either)
      (if (pair? either)
          (car either)
          '()))

    ;; 创建右值（成功情况）
    (define (from-right x)
      (let ((ret (cons 'empty-left 'empty-right)))
        (set-cdr! ret x)
        ret))

    ;; 从 either 中提取右值
    (define (to-right either)
      (if (pair? either)
          (cdr either)
          '()))

    ;; 检查是否是右值
    (define (either-right? either)
      (and (pair? either) (eq? (car either) 'empty-left)))

    ;; 检查是否是左值
    (define (either-left? either)
      (and (pair? either) (eq? (cdr either) 'empty-right)))

    ;; 映射函数：如果 either 是右值，则应用函数 f
    (define (either-map f either)
      (if (either-right? either)
          (from-right (f (to-right either)))
          either))

    ;; 扁平映射函数：如果 either 是右值，则应用函数 f
    (define (either-flat-map f either)
      (if (either-right? either)
          (f (to-right either))
          either))

    ;; 遍历函数：如果 either 是右值，则应用函数 f
    (define (either-for-each f either)
      (when (either-right? either)
            (f (to-right either))))

    ) ; end of begin
  ) ; end of define-library