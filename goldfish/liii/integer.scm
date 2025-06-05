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

(define-library (liii integer)
(import (liii oop)
        (liii error)
        (liii list)
        (liii lang))
(export rich-integer rich-integers)
(begin

;;; ========================================
;;; rich-integer - Instance methods
;;; ========================================

(define (rich-integer data)

(define %this (inlet 'data data))

(define (%equals that)
  (and (let? that) ; TODO: must be a rich-integer
       (equal? (%this 'data) (that 'data))))

(define (%get) data)

(define (%to n) 
  (unless (integer? n) 
    (type-error 
      (format #f "In function #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
              %to '(n) 'n "integer" (object->string n))))
  (if (< n data) 
    (rich-list (list)) 
    (rich-list (iota (+ (- n data) 1) data))))

(define (%until n) 
  (unless (integer? n) 
    (type-error 
      (format #f "In function #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
              %until '(n) 'n "integer" (object->string n))))
  (if (<= n data) 
    (rich-list (list)) 
    (rich-list (iota (+ (- n data)) data))))

(define (%to-rich-char)
  (rich-char data))

(define (%to-string)
  (number->string data))

;;return exact integer
(define (%sqrt)
  (if (< data 0)
      (value-error
        (format #f "rich-integer%sqrt of negative integer is undefined!         ** Got ~a **" data))
      (inexact->exact (floor (sqrt data)))))

(define (%apply msg . args)
  (if (defined? msg %this #t)
      (apply (%this msg) args)))

(varlet %this :equals %equals)
(varlet %this :get %get)
(varlet %this :to %to)
(varlet %this :until %until)
(varlet %this :to-rich-char %to-rich-char)
(varlet %this :to-string %to-string)
(varlet %this :sqrt %sqrt)
(varlet %this :apply %apply)

) ; end of rich-integer

;;; ========================================
;;; rich-integers - Static methods
;;; ========================================

(define-object rich-integers

(define (@max-value) 9223372036854775807)

(define (@min-value) -9223372036854775808)

) ; end of rich-integers

) ; end of begin
) ; end of define-library 