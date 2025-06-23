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

(define-library (liii lang)
                
(import (only (liii base)
              u8-string-length any? receive u8-substring)
        (only (liii oop)
              define-case-class display* @ typed-define case-class? chained-define
              define-object define-class chain-apply object->string)
        (only (liii sort) list-stable-sort vector-stable-sort)
        (only (liii hash-table)
              hash-table-update!/default hash-table-for-each hash-table-ref/default hash-table-contains? hash-table-delete!
              hash-table-count)
        (only (liii bitwise) bitwise-and bitwise-ior arithmetic-shift)
        (liii error)
        (liii list)
        (liii option)
        (liii rich-list)
        (liii rich-char)
        (liii rich-vector)
        (liii rich-string))

(export
  @ typed-define define-case-class define-object define-class
  case-class? class=? chained-define display* object->string
  option none either left right
  rich-integer rich-float rich-char rich-string
  rich-vector rich-list array rich-hash-table
  box $
)
(begin

(define (class=? left right)
  (cond
    ((and (case-class? left) (case-class? right))
     (left :equals right))
    ((case-class? left)
     (left :equals ($ right)))
    ((case-class? right)
     ($ left :equals right))
    (else
     (equal? left right))))

(define (box x)
  (cond ((integer? x) (rich-integer x))
        ((rational? x) (rich-rational x))
        ((float? x) (rich-float x))
        ((char? x) (rich-char x))
        ((string? x) (rich-string x))
        ((list? x) (rich-list x))
        ((vector? x) (rich-vector x))
        ((hash-table? x) (rich-hash-table x))
        (else (type-error "box: x must be integer?, rational?, float?, char?, string?, list?, vector?, hash-table?"))))

(define ($ x . xs)
  (if (null? xs) (box x) (apply (box x) xs)))

(define-case-class rich-integer ((data integer?))

(define (%get) data)

(define (%to n) 
  (unless (integer? n) 
    (type-error 
      (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
              %to '(n) 'n "integer" (object->string n))))
  (if (< n data) 
    (rich-list (list)) 
    (rich-list (iota (+ (- n data) 1) data))))

(define (%until n) 
  (unless (integer? n) 
    (type-error 
      (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
              %until '(n) 'n "integer" (object->string n))))
  (if (<= n data) 
    (rich-list (list)) 
    (rich-list (iota (+ (- n data)) data))))

(define (%to-rich-char)
  (rich-char data))

(define (%to-string)
  (number->string data))

(define (@max-value) 9223372036854775807)

(define (@min-value) -9223372036854775808)

;;return exact integer
(define (%sqrt)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
      (inexact->exact (floor (sqrt data)))))

)

(define-case-class rich-rational ((data rational?))

(define (%get) data)

(define (%abs) 
  (if (< data 0)
      (- 0 data)
      data))
  
)

(define-case-class rich-float ((data float?))
                   
(define (%get) data)

(define (%abs) 
  (if (< data 0)
      (- 0 data)
      data))
  
(define (%to-string)
  (number->string data))

(define (%sqrt)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative float is undefined!         ** Got ~a **" data))
      (sqrt data)))

)


(define-case-class either
  ((type symbol?)
   (value any?))

(define (%left?)
  (eq? type 'left))

(define (%right?)
  (eq? type 'right))

(define (%get)
  value)

(define (%or-else default)
  (unless (case-class? default) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %or-else '(default) 'default "case-class" (object->string default))))  
  
  (when (not (default :is-instance-of 'either))
    (type-error "The first parameter of either%or-else must be a either case class"))

  (if (%right?)
      (%this)
      default))

(define (%get-or-else default)
  (cond ((%right?) value)
        ((and (procedure? default) (not (case-class? default)))
         (default))
        (else default)))

(define (%filter-or-else pred zero)
  (unless (procedure? pred) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %filter-or-else '(pred zero) 'pred "procedure" (object->string pred))))
  
  (unless (any? zero) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %filter-or-else '(pred zero) 'zero "any" (object->string zero))))  
  (if (%right?)
      (if (pred value)
          (%this)
          (left zero))
      (%this)))

(define (%contains x)
  (and (%right?)
       (class=? x value)))

(define (%for-each f)
  (when (%right?)
    (f value)))

(define (%to-option)
  (if (%right?)
      (option value)
      (none)))

(define (%map f . args)
  (chain-apply args
    (if (%right?)
      (right (f value))
      (%this))))

(define (%flat-map f . args)
  (chain-apply args
    (if (%right?)
      (f value)
      (%this))))

(define (%forall pred)
  (unless (procedure? pred) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %forall '(pred) 'pred "procedure" (object->string pred))))
  (if (%right?)
      (pred value)
      #t))

(define (%exists pred)
  (unless (procedure? pred) 
      (type-error 
        (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                    %exists '(pred) 'pred "procedure" (object->string pred))))
  (if (%right?)
      (pred value)
      #f))

)

(define (left v)
  (either 'left v))

(define (right v)
  (either 'right v))

(define array rich-vector)

(define-case-class rich-hash-table ((data hash-table?))
  (define (%collect) data)

(chained-define (@empty)
  (rich-hash-table (make-hash-table)))

(define (%find pred?)
  (define iter (make-iterator data))
  (let loop ((kv (iter)))
    (cond 
        ((eof-object? kv) (none))
        ((and (pair? kv) (pred? (car kv) (cdr kv))) (option kv))
        (else (loop (iter))))))

(define (%get k)
  (option (hash-table-ref/default data k '())))

(define (%remove k)
  (rich-hash-table
   (let ((new (make-hash-table)))
     (hash-table-for-each
      (lambda (key val)
       (unless (equal? key k)
        (hash-table-set! new key val)))
       data)
      new)))

(chained-define (%remove! k)
  (hash-table-delete! data k)
  %this)

(define (%contains k)
  (hash-table-contains? data k))

(define (%forall pred?)
  (let ((all-kv (map identity data)))
    (let loop ((kvs all-kv))  
      (if (null? kvs)
          #t  
          (let ((kv (car kvs)))
            (if (pred? (car kv) (cdr kv))
                (loop (cdr kvs))  
                #f))))))  

(define (%exists pred?)
  (define iter (make-iterator data))
  (let loop ((kv (iter)))
    (cond 
        ((eof-object? kv) #f)
        ((and (pair? kv) (pred? (car kv) (cdr kv))) #t)
        (else (loop (iter))))))

(define (%map f . args)
  (chain-apply args
    (let ((r (make-hash-table)))
      (hash-table-for-each
         (lambda (k v)
           (receive (k1 v1) (f k v)
             (hash-table-set! r k1 v1)))
         data)
      (rich-hash-table r))))

(define (%count pred)
  (hash-table-count pred data))

(define (%for-each proc)
  (hash-table-for-each proc data))

(define (%filter f . args)
  (chain-apply args
    (let ((r (make-hash-table)))
      (hash-table-for-each
         (lambda (k v)
           (when (f k v) (hash-table-set! r k v)))
         data)
      (rich-hash-table r))))

)

) ; end of begin
) ; end of library

