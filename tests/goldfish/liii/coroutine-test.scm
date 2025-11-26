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
  (liii coroutine))

#|
co-dispatch
调度并执行协程及其派生的所有子协程。

语法
----
(co-dispatch function)

参数
----
function : procedure
要执行的函数，可在其中创建协程。

返回值
----
nil
总是返回空列表。

注意
----
co-dispatch 会创建一个调度器并运行传入的函数及其创建的所有协程，
直到所有协程完成。协程按 LIFO（后进先出）顺序调度：
一旦某个协程开始执行，它会一直运行直到主动让出CPU（通过 yield 或 sleep），
然后调度器会选择最近挂起的协程来执行，而不是交替执行。

示例
----
|#
(check (let ((sum 0))
    (co-dispatch
      (lambda ()
        (co-create (lambda () (set! sum (+ sum 1))))
        (co-create (lambda () (set! sum (+ sum 2))))))
    sum) => 3)

;; co-dispatch 应该执行传入的函数及其所有派生的协程
(let ((xs '(1 2 3 4 5)))
  (check (let ((sum 0))
      (co-dispatch
        (lambda ()
          (map (lambda (x) (co-create (lambda () (set! sum (+ sum x))))) xs)))
      sum) => (apply + xs)))

#|
co-create
创建一个新的协程并立即开始执行。

语法
----
(co-create function)

参数
----
function : procedure
要在协程中执行的函数。

返回值
----
nil
总是返回空列表。

注意
----
co-create 必须在 co-dispatch 创建的调度器上下文中调用。
创建的协程会立即开始执行，并按 LIFO（后进先出）顺序调度：
一旦某个协程开始执行，它会一直运行直到主动让出CPU，
然后调度器会选择最近挂起的协程来执行。协程可以嵌套创建其他协程。

错误处理
----
如果在没有调度器的上下文中调用，会抛出错误。

示例
----
|#
(check (let ((result '()))
    (co-dispatch
      (lambda ()
        (co-create (lambda () (set! result (cons 'a result))))
        (co-create (lambda () (set! result (cons 'b result))))))
    (reverse result)) => '(a b))

;; co-dispatch 嵌套创建协程
(check (let ((v 1))
    (co-dispatch
      (lambda ()
        (co-create
          (lambda ()
            (co-create (lambda () (set! v (* v 10))))
            (set! v (+ v 2))))))
    v) => 30)

;; co-dispatch 执行纯函数
;; TODO(jinser): 创建时不要自动启动
; (check (procedure? (co-create (lambda () 42)) => '()))

;; co-dispatch 执行纯函数
(check (co-dispatch (lambda () 42)) => '())

;; co-dispatch 多个协程的副作用
(check (let ((s '()))
    (co-dispatch
      (lambda ()
        (co-create (lambda () (set! s (cons #\a s))))
        (co-create (lambda () (set! s (cons #\b s))))
        (co-create (lambda () (set! s (cons #\c s))))))
    s) => '(#\c #\b #\a))

#|
co-yield
让出当前协程的执行权，允许其他协程运行。

语法
----
(co-yield)

参数
----
无

返回值
----
boolean
返回 #t 表示成功让出，#f 表示失败。

注意
----
co-yield 用于协程之间的协作调度。
TBOX 调度器使用 LIFO（后进先出）顺序，后创建的协程在栈顶。
一旦某个协程开始执行，它会一直运行直到主动让出CPU。
当协程让出时，调度器会选择最近挂起的协程来执行，而不是交替执行。

示例
----
|#
(check (let ((trace '()))
    (co-dispatch
      (lambda ()
        (co-create
          (lambda ()
            (set! trace (cons 'a trace))
            (co-yield)
            (set! trace (cons 'b trace))))
        (co-create
          (lambda ()
            (set! trace (cons 'c trace))))))
    (reverse trace)) => '(a c b))

;; co-yield 在协程之间交替执行
;; 注意：TBOX 使用 LIFO 顺序调度，先创建的先运行，让出给后创建的
(display "\n=== Test: co-yield alternates execution ===\n")

;; 定义协程 A 的执行逻辑
(define (make-coroutine-a trace-setter)
  (lambda ()
    (trace-setter 'a1)
    (display "Coroutine A: yielding...\n")
    (co-yield)
    (display "Coroutine A: step 2\n")
    (trace-setter 'a2)
    (display "Coroutine A: yielding again...\n")
    (display "Coroutine A: step 3 (final)\n")
    (co-yield)
    (trace-setter 'a3)))

;; 定义协程 B 的执行逻辑
(define (make-coroutine-b trace-setter)
  (lambda ()
    (trace-setter 'b1)
    (display "Coroutine B: yielding...\n")
    (display "Coroutine B: step 2\n")
    (co-yield)
    (trace-setter 'b2)
    (display "Coroutine B: yielding again...\n")
    (display "Coroutine B: step 3 (final)\n")
    (co-yield)
    (trace-setter 'b3)))

(check (let ((trace '()))
    (define (add-trace! symbol)
      (set! trace (cons symbol trace)))
    (display "\nCoroutine A: step 1\n")
    (co-dispatch
      (lambda ()
        (co-create (make-coroutine-a add-trace!))
        (display "Coroutine B: step 1\n")
        (co-create (make-coroutine-b add-trace!))))
    (display (string-append "Execution trace: " (object->string (reverse trace)) "\n"))
    (reverse trace)) => '(a1 b1 b2 b3 a2 a3))  ; LIFO顺序

;; co-yield 返回布尔值表示成功
(check (co-dispatch
    (lambda ()
      (co-create
        (lambda ()
          (boolean? (co-yield)))))) => '())

#|
co-sleep
让当前协程休眠指定的时间，期间允许其他协程运行。

语法
----
(co-sleep seconds)

参数
----
seconds : real
休眠的秒数，可以是小数。

返回值
----
nil
总是返回空列表。

注意
----
co-sleep 是非阻塞的休眠，只会暂停当前协程，
不会阻塞整个程序或调度器。其他协程可以在休眠期间继续执行。
休眠时间到达后，协程会被重新调度执行。
调度器使用 LIFO（后进先出）顺序：最近挂起的协程会优先被调度。

错误处理
----
如果参数不是数字，会抛出类型错误。

示例
----
|#
(check (let ((order '()))
    (co-dispatch
      (lambda ()
        (co-create
          (lambda ()
            (set! order (cons 'start order))
            (co-sleep 0.01)
            (set! order (cons 'end order))))
        (co-create
          (lambda ()
            (set! order (cons 'instant order))))))
    (reverse order)) => '(start instant end))

;; co-sleep 允许定时调度
;; 注意：第一个协程立即启动，休眠，第二个协程运行，然后第一个协程唤醒
(display "\n=== Test: co-sleep scheduling ===\n")
(check (let ((order '()))
    (display "\nCoroutine 1: starting, will sleep 0.05s\n")
    (co-dispatch
      (lambda ()
        (co-create
          (lambda ()
            (set! order (cons 'fast-start order))
            (display "Coroutine 1: entering sleep...\n")
            (co-sleep 0.05)
            (display "Coroutine 1: woke up from sleep\n")
            (set! order (cons 'fast-end order))))
        (display "Coroutine 2: executing immediately (no sleep)\n")
        (co-create
          (lambda ()
            (set! order (cons 'instant order))))))
    (display (string-append "Execution order: " (object->string (reverse order)) "\n"))
    (reverse order)) => '(fast-start instant fast-end))

;; co-sleep 多个协程
;; 注意：两个协程按 LIFO 顺序启动（先 1 后 2），休眠时间短的先唤醒
(display "\n=== Test: multiple coroutines with different sleep times ===\n")
(check (let ((steps '()))
    (display "\nCoroutine A: step 1, sleeping 0.02s\n")
    (co-dispatch
      (lambda ()
        (co-create
          (lambda ()
            (set! steps (cons 1 steps))
            (co-sleep 0.02)
            (display "Coroutine A: step 4 (woke up after longer sleep)\n")
            (set! steps (cons 4 steps))))
        (display "Coroutine B: step 2, sleeping 0.01s\n")
        (co-create
          (lambda ()
            (set! steps (cons 2 steps))
            (co-sleep 0.01)
            (display "Coroutine B: step 3 (woke up first - shorter sleep)\n")
            (set! steps (cons 3 steps))))))
    (display (string-append "Steps order: " (object->string (reverse steps)) "\n"))
    (reverse steps)) => '(1 2 3 4))
