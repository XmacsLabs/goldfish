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

(define-library (liii json)
  (import (liii base) 
          (guenchi json))
  (export
    json-string-escape 
    string->json 
    json->string
    
    json-ref  json-ref* json-set  json-set* json-push json-push* json-drop json-drop* json-reduce json-reduce* 

    json-null? json-object? json-array? json-string? json-float? json-number? json-integer? json-boolean?
    
    json-contains-key? 
    
    json-ref-string json-ref-number json-ref-integer json-ref-boolean json-get-or-else   
    
    json-keys)
  
  (begin

    ;;; ---------------------------------------------------------
    ;;; 1. 类型谓词 
    ;;; ---------------------------------------------------------

    (define (json-null? x)
      (eq? x 'null))

    (define (json-object? x)
      (and (list? x) (not (null? x))))

    (define (json-array? x)
      (vector? x))

    (define (json-string? x)
      (string? x))

    (define (json-number? x)
      (number? x))
      
    (define (json-integer? x)
      (integer? x))

    (define (json-float? x)
      (float? x))

    (define (json-boolean? x)
      (boolean? x))

    ;;; ---------------------------------------------------------
    ;;; 2. 状态检查
    ;;; ---------------------------------------------------------

    (define (json-contains-key? json key)
      (if (not (json-object? json))
          #f
          (if (equal? json '(()))
              #f
              (if (assoc key json) #t #f))))

    ;;; ---------------------------------------------------------
    ;;; 3. 安全获取器
    ;;; ---------------------------------------------------------

    (define (json-get-or-else json default)
      (if (json-null? json)
          default
          json))

    (define (json-ref-string json key default)
      (let ((val (json-ref json key)))
        (if (string? val) val default)))

    (define (json-ref-number json key default)
      (let ((val (json-ref json key)))
        (if (number? val) val default)))
    
    (define (json-ref-integer json key default)
      (let ((val (json-ref json key)))
        (if (integer? val) val default)))

    (define (json-ref-boolean json key default)
      (let ((val (json-ref json key)))
        (if (boolean? val) val default)))

    ;;; ---------------------------------------------------------
    ;;; 4. 辅助工具
    ;;; ---------------------------------------------------------

    (define (json-keys json)
      (if (json-object? json)
          (if (equal? json '(()))
              '()
              (map car json))
          '()))

  ) ; end of begin
) ; end of define-library