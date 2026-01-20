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
          either-map either-flat-map either-for-each
          either-get-or-else
          either-fold
          either-or-else
          either-match)
  (begin

    ;; ======================
    ;; 构造函数
    ;; ======================
    
    ;; 创建左值（错误情况）
    (define (from-left value)
      (cons value 'left))

    ;; 创建右值（成功情况）
    (define (from-right value)
      (cons value 'right))

    ;; ======================
    ;; 类型判断函数
    ;; ======================

    ;; 检查是否是左值
    (define (either-left? either)
      (and (pair? either) (eq? (cdr either) 'left)))

    ;; 检查是否是右值
    (define (either-right? either)
      (and (pair? either) (eq? (cdr either) 'right)))

    ;; ======================
    ;; 提取函数
    ;; ======================

    ;; 从 either 中提取左值
    (define (to-left either)
      (cond
        ((not (pair? either))
         (error "Invalid Either value"))
        ((eq? (cdr either) 'left)
         (car either))
        (else
         (error "Cannot extract left from Right" either))))

    ;; 从 either 中提取右值
    (define (to-right either)
      (cond
        ((not (pair? either))
         (error "Invalid Either value"))
        ((eq? (cdr either) 'right)
         (car either))
        (else
         (error "Cannot extract right from Left" either))))

    ;; ======================
    ;; 高阶函数操作
    ;; ======================

    ;; 映射函数：如果 either 是右值，则应用函数 f
    (define (either-map f either)
      (if (either-right? either)
          (from-right (f (car either)))
          either))

    ;; 扁平映射函数：如果 either 是右值，则应用函数 f (f 必须返回 Either)
    (define (either-flat-map f either)
      (if (either-right? either)
          (f (car either))
          either))

    ;; 遍历函数：如果 either 是右值，则应用函数 f (执行副作用)
    (define (either-for-each f either)
      (when (either-right? either)
        (f (car either))))

    ;; ======================
    ;; 附加实用函数
    ;; ======================

    ;; 获取值或默认值
    (define (either-get-or-else default either)
      (if (either-right? either)
          (car either)
          default))


    ;; fold操作：根据类型分别调用 left-func 或 right-func
    (define (either-fold left-func right-func either)
      (if (either-right? either)
          (right-func (car either))
          (left-func (car either))))

    ;; 组合器：如果是 Left 则返回 alternative，否则返回自身
    (define (either-or-else alternative either)
      (if (either-right? either)
          either
          alternative))


  ) ; end of begin
) ; end of define-library