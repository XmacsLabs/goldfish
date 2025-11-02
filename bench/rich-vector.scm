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
        (liii lang))

(define (timing msg stmt number)
  (display* msg (number->string (timeit stmt (lambda () #t) number)) "\n"))

(timing "rich-vector%empty%length:\t"
  (lambda () (rich-vector :empty :length))
  10000)

(timing "vector-length (vector):\t"
  (lambda () (vector-length (vector)))
  10000)

;; 测试长度为100的随机向量求和操作
(define (create-random-vector n)
  (let ((vec (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((>= i n) vec)
      (vector-set! vec i (random 1000)))))

(define (create-random-rich-vector n)
  (rich-vector (create-random-vector n)))

(define (vector-sum vec)
  (apply + (vector->list vec)))

(define (rich-vector-sum rvec)
  (rvec :reduce +))

;; 测试 rich-vector 求和（只计算求和操作）
(let ((rvec (create-random-rich-vector 100)))
  (timing "rich-vector-sum-only (length=100):\t"
    (lambda () (rich-vector-sum rvec))
    50000))

;; 测试 vector 求和（只计算求和操作）
(let ((vec (create-random-vector 100)))
  (timing "vector-sum-only (length=100):\t"
    (lambda () (vector-sum vec))
    50000))
