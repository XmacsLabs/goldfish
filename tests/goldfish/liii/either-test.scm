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
;; 1. 基础构造与提取测试
;; ==========================================

#|
from-left
创建 Left 值（通常代表错误或异常情况）。

语法
----
(from-left value)

参数
----
value : any
    要存储在 Left 中的值，可以是任意类型。

返回值
------
either
    返回一个 Left 类型的 Either 值。
|#

#|
to-left
从 Left 类型的 Either 中提取值。

语法
----
(to-left either)

参数
----
either : either
    一个 Left 类型的 Either 值。

返回值
------
any
    存储在 Left 内部的值。
|#
(check (to-left (from-left "error message")) => "error message")
(check (to-left (from-left 42)) => 42)
(check (to-left (from-left '())) => '())

#|
from-right
创建 Right 值（通常代表成功或有效数据）。

语法
----
(from-right value)

参数
----
value : any
    要存储在 Right 中的值，可以是任意类型。

返回值
------
either
    返回一个 Right 类型的 Either 值。
|#

#|
to-right
从 Right 类型的 Either 中提取值。

语法
----
(to-right either)

参数
----
either : either
    一个 Right 类型的 Either 值。

返回值
------
any
    存储在 Right 内部的值。
|#
(check (to-right (from-right "success data")) => "success data")
(check (to-right (from-right 100)) => 100)
(check (to-right (from-right '(1 2 3))) => '(1 2 3))


;; ==========================================
;; 2. 谓词测试
;; ==========================================

#|
either-left? / either-right?
类型判断函数。

语法
----
(either-left? either)
(either-right? either)

参数
----
either : either
    要检查的 Either 对象。

返回值
------
boolean
    如果符合对应类型返回 #t，否则返回 #f。
|#
(let ((left-val (from-left "error"))
      (right-val (from-right "success")))
  (check-true (either-left? left-val))
  (check-false (either-right? left-val))
  (check-true (either-right? right-val))
  (check-false (either-left? right-val)))

;; 边界情况测试：非 Pair 不是 Either
(check-false (either-left? '()))
(check-false (either-right? '()))
(check-false (either-left? "string"))


;; ==========================================
;; 3. 高阶函数操作测试 (Map / For-Each)
;; ==========================================

#|
either-map
Functor 映射操作。

语法
----
(either-map func either)

参数
----
func : procedure (any -> any)
    应用于 Right 值的函数。
either : either
    输入的 Either 值。

返回值
------
either
    - Right: 返回包含 (func value) 的新 Right。
    - Left: 原样返回 Left。
|#
(let ((left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 map 应该返回原值
  (check (to-left (either-map (lambda (x) (* x 2)) left-val)) => "error")
  ;; 对右值应用 map 应该应用函数
  (let ((result (either-map (lambda (x) (* x 2)) right-val)))
    (check-true (either-right? result))
    (check (to-right result) => 10)))

#|
either-for-each
副作用遍历操作。

语法
----
(either-for-each proc either)

参数
----
proc : procedure (any -> void)
    需要执行副作用的函数。
either : either
    输入的 Either 值。

描述
----
如果 either 是 Right，则对内部值执行 proc。
如果 either 是 Left，则什么也不做。
|#
(let ((counter 0)
      (left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 for-each 不执行
  (either-for-each (lambda (x) (set! counter (+ counter x))) left-val)
  (check counter => 0)
  ;; 对右值应用 for-each 执行
  (either-for-each (lambda (x) (set! counter (+ counter x))) right-val)
  (check counter => 5))


;; ==========================================
;; 4. 实用函数测试 (Utility Functions)
;; ==========================================

#|
either-get-or-else
简单获取值或默认值。

语法
----
(either-get-or-else either default)

参数
----
either : either
    目标 Either 对象。
default : any
    备用值。

返回值
------
any
    Right 的值或 default。
|#
(check (either-get-or-else (from-right 42) 0) => 42)
(check (either-get-or-else (from-left "error") 0) => 0)

#|
either-fold
提取值或计算默认值（支持惰性求值）。

语法
----
(either-fold default either)

参数
----
default : any | procedure
    当 either 为 Left 时返回的值。
    如果是无参过程（thunk），则调用该过程并返回结果（惰性求值）。
either : either
    要提取值的 Either 对象。

返回值
------
any
    Right 的内部值，或 default 处理后的结果。

描述
----
在当前实现中，此函数行为类似于支持惰性求值的 get-or-else。
如果 default 是一个过程（且不是 case-class），它将被调用以生成返回值。
|#
;; 1. 测试基础值返回
(check (either-fold 0 (from-right 42)) => 42)
(check (either-fold 0 (from-left "err")) => 0)

;; 2. 测试惰性求值 (Lazy Evaluation)
;; 当 default 是一个 lambda 时，应该调用它
(check (either-fold (lambda () 99) (from-left "err")) => 99)

;; 3. 验证 Right 情况下不会调用 default 函数 (短路)
(let ((called #f))
  (either-fold (lambda () (set! called #t)) (from-right 10))
  (check-false called))

#|
either-or-else
Either 级别的备选方案。

语法
----
(either-or-else alternative either)

参数
----
alternative : either
    备用的 Either 值。
either : either
    主 Either 值。

返回值
------
either
    如果主 either 是 Right，返回主 either。
    如果主 either 是 Left，返回 alternative。
|#
(let ((main (from-right 1))
      (backup (from-right 2))
      (fail (from-left 0)))
  (check (to-right (either-or-else backup main)) => 1)
  (check (to-right (either-or-else backup fail)) => 2))


;; ==========================================
;; 5. 综合流程测试
;; ==========================================

;; 测试 Map 的连续使用
(let* ((val1 (from-right 10))
       (val2 (either-map (lambda (x) (+ x 5)) val1))     ;; Right 15
       (val3 (either-map (lambda (x) (* x 2)) val2)))    ;; Right 30
  (check-true (either-right? val3))
  (check (to-right val3) => 30))

;; 测试错误处理流程 (验证短路特性)
(let* ((error-val (from-left "network error"))
       ;; 下面的 map 不应执行，因为输入已经是 Left
       (mapped-error (either-map (lambda (x) (string-append "Error: " x)) error-val)))
  (check-true (either-left? mapped-error))
  (check (to-left mapped-error) => "network error"))

(check-report)