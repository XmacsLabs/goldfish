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

(import (liii check)
        (scheme base)
        (liii rich-list)
        (liii lang)
        (liii error))

(check-set-mode! 'report-failed)

;; 基本测试
(check ((rich-list :range 1 5) :collect) => (list 1 2 3 4))
(check ((rich-list :range 1 5 2) :collect) => (list 1 3))
(check ((rich-list :range 1 6 2) :collect) => (list 1 3 5))
(check ((rich-list :range 5 1 -1) :collect) => (list 5 4 3 2))
(check ((rich-list :range 1 5 :map (lambda (x) (* x 2))) :collect) => (list 2 4 6 8))
(check ((rich-list :range 1 10 1 :map (lambda (x) (+ x 1))) :collect) => (list 2 3 4 5 6 7 8 9 10))
(check ((rich-list :range 5 1 1) :collect) => (list))

;; 边界测试
(check ((rich-list :range 10 1 1) :collect) => (list))
(check ((rich-list :range -5 -1 1) :collect) => (list -5 -4 -3 -2))
(check ((rich-list :range -1 -5 -1) :collect) => (list -1 -2 -3 -4))
(check ((rich-list :range 5 6) :collect) => (list 5))
(check ((rich-list :range 5 6 -1) :collect) => (list))
(check ((rich-list :range 1 5) :length) => 4)
(check ((rich-list :range 1 1) :length) => 0)


;; 基本测试
(check ((rich-list :empty) :collect) => ())
(check ((rich-list :empty) :length) => 0)
(check ((rich-list :empty) :empty?) => #t)

;; 边界测试
(check ((rich-list :empty :map (lambda (x) (* x 2))) :collect) => ())
(check ((rich-list :empty :filter (lambda (x) #t)) :collect) => ())
(check ((rich-list :empty :take 0) :collect) => ())
(check ((rich-list :empty :drop 0) :collect) => ())
(check ((rich-list :empty :reverse) :collect) => ())
(check ((rich-list :fill 0 'x) :collect) => ())


;; 基本测试 - 两个非空列表连接
(check ((rich-list :concat (rich-list '(1 2 3)) (rich-list '(4 5 6))) :collect) => '(1 2 3 4 5 6))
(check ((rich-list :concat (rich-list '(a b)) (rich-list '(c d))) :collect) => '(a b c d))

;; 边界测试 - 第一个列表为空
(check ((rich-list :concat (rich-list :empty) (rich-list '(1 2 3))) :collect) => '(1 2 3))

;; 边界测试 - 第二个列表为空
(check ((rich-list :concat (rich-list '(1 2 3)) (rich-list :empty)) :collect) => '(1 2 3))

;; 边界测试 - 两个列表都为空
(check ((rich-list :concat (rich-list :empty) (rich-list :empty)) :collect) => '())

;; 边界测试 - 嵌套rich-list连接
(check ((rich-list :concat (rich-list :range 1 4) (rich-list :range 4 7)) :collect) => '(1 2 3 4 5 6))

;; 链式调用测试
(check ((rich-list :concat (rich-list '(1 2)) (rich-list '(3 4))) :map (lambda (x) (* x 2)) :collect) => '(2 4 6 8))
(check ((rich-list :concat (rich-list '(a b)) (rich-list '(c d))) :length) => 4)

;; 验证不改变原列表
(let ((lst1 (rich-list '(1 2 3)))
      (lst2 (rich-list '(4 5 6))))
  (rich-list :concat lst1 lst2)
  (check (lst1 :collect) => '(1 2 3))
  (check (lst2 :collect) => '(4 5 6)))

;; 基本测试
(check ((rich-list :fill 5 'x) :collect) => '(x x x x x))
(check ((rich-list :fill 3 10) :collect) => '(10 10 10))
(check ((rich-list :fill 1 "hello") :collect) => '("hello"))
(check ((rich-list :fill 0 'a) :collect) => ())

;; 边界测试 - n为0
(check ((rich-list :fill 0 1) :collect) => ())
(check ((rich-list :fill 0 "test") :collect) => ())

;; 边界测试 - 不同类型的elem
(check ((rich-list :fill 3 #t) :collect) => '(#t #t #t))
(check ((rich-list :fill 2 '(1 2 3)) :collect) => '((1 2 3) (1 2 3)))
(check ((rich-list :fill 4 #f) :collect) => '(#f #f #f #f))

;; 边界测试 - 基础边界
(check ((rich-list :fill 0 'test) :collect) => ())
(check ((rich-list :fill 1 42) :collect) => '(42))
(check ((rich-list :fill 100 0) :length) => 100)
(check ((rich-list :fill 2 '(nested list)) :collect) => '((nested list) (nested list)))

;; 边界测试 - 异常处理
(check-catch 'value-error (rich-list :fill -1 'x))
(check-catch 'value-error (rich-list :fill -3 42))

;; 链式调用测试
(check ((rich-list :fill 4 2) :map (lambda (x) (* x 3)) :collect) => '(6 6 6 6))
(check ((rich-list :fill 3 'a) :length) => 3)
(check ((rich-list :fill 5 1) :filter (lambda (x) (= x 1)) :collect) => '(1 1 1 1 1))
(check ((rich-list :fill 3 100) :take 2 :collect) => '(100 100))

(check-report)
