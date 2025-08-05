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
        (liii sort)
        (liii coroutine))

;; coroutine-dispatch should execute the passed-in function and all its derived coroutines
(let ((xs '(1 2 3 4 5)))
  (check (let ((sum 0))
           (coroutine-dispatch
             (lambda ()
               (map (lambda (x) (coroutine-create (lambda () (set! sum (+ sum x))))) xs)))
           sum) => (apply + xs)))

;; coroutine-dispatch with nested coroutine-create
(check (let ((v 1))
         (coroutine-dispatch
           (lambda ()
             (coroutine-create
               (lambda ()
                 (coroutine-create (lambda () (set! v (* v 10))))
                 (set! v (+ v 2))))))
         v) => 30)

;; coroutine-dispatch with pure function
(check (coroutine-dispatch (lambda () 42)) => '())

;; coroutine-dispatch with side effects from multiple coroutines
(check (let ((s '()))
         (coroutine-dispatch
           (lambda ()
             (coroutine-create (lambda () (set! s (cons #\a s))))
             (coroutine-create (lambda () (set! s (cons #\b s))))
             (coroutine-create (lambda () (set! s (cons #\c s))))))
        s) => '(#\c #\b #\a))

