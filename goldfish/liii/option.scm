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

(define-library (liii option)
(import (liii error)
        (liii oop))
(export option none)
(begin

;;; ========================================
;;; option - Instance methods
;;; ========================================

(define (option value)

(define %this (inlet 'value value))

(define (%get)
  (if (null? value)
      (value-error "option is empty, cannot get value")
      value))

(define (%get-or-else default)
  (cond ((not (null? value)) value)
        ((and (procedure? default) (not (let? default)))
         (default))
        (else default)))

(define (%or-else default . args)
  (when (not (and (let? default) (default 'value)))
    (type-error "The first parameter of option%or-else must be an option"))
  
  ; Apply chain operations if provided
  (let ((result (if (null? value)
                    default
                    (option value))))
    (if (null? args)
        result
        (apply (result :apply) args))))

(define (%equals that)
  (and (let? that)
       (defined? 'value that)
       (equal? value (that 'value))))

(define (%defined?) 
  (not (null? value)))
  
(define (%empty?)
  (null? value))

(define (%forall f)
  (if (null? value)
      #f
      (f value)))

(define (%exists f)
  (if (null? value)
      #f
      (f value)))

(define (%contains elem)
  (if (null? value)
      #f
      (equal? value elem)))

(define (%for-each f)
  (when (not (null? value))
        (f value)))

(define (%map f . args)
  (let ((result (if (null? value)
                    (option '())
                    (option (f value)))))
    (if (null? args)
        result
        (apply (result :apply) args))))

(define (%flat-map f . args)
  (let ((result (if (null? value)
                    (option '())
                    (f value))))
    (if (null? args)
        result
        (apply (result :apply) args))))

(define (%filter pred . args)
  (let ((result (if (or (null? value) (not (pred value)))
                    (option '())
                    (option value))))
    (if (null? args)
        result
        (apply (result :apply) args))))

(define (%apply msg . args)
  (if (defined? msg %this #t)
      (apply (%this msg) args)
      (error 'undefined-method (format #f "Method ~a not found" msg))))

;; Register all methods
(varlet %this :get %get)
(varlet %this :get-or-else %get-or-else)
(varlet %this :or-else %or-else)
(varlet %this :equals %equals)
(varlet %this :defined? %defined?)
(varlet %this :empty? %empty?)
(varlet %this :forall %forall)
(varlet %this :exists %exists)
(varlet %this :contains %contains)
(varlet %this :for-each %for-each)
(varlet %this :map %map)
(varlet %this :flat-map %flat-map)
(varlet %this :filter %filter)
(varlet %this :apply %apply)

%this

) ; end of option

;;; ========================================
;;; Static methods and constructors
;;; ========================================

(define (none) 
  (option '()))

) ; end of begin
) ; end of define-library

