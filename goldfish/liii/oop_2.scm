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

(define-library (liii oop_2)
(import (liii error)
        (liii string))
(export define-case-class-2)
(begin

(define-macro (define-case-class-2 class-name fields . instance-methods)
  (let* ((field-names (map car fields))
         (field-types (map cadr fields))
         (field-defaults (map (lambda (field)
                               (if (>= (length field) 3)
                                   (caddr field)
                                   #f))
                             fields))
         
         ;; Extract instance method names for registration
         (instance-method-names (map (lambda (method)
                                     (let ((method-name (symbol->string (caadr method))))
                                       (string->symbol (string-append ":" (substring method-name 1)))))
                                   instance-methods))
         
         (type-checks (map (lambda (field-name field-type)
                            `(unless (,field-type ,field-name)
                               (type-error
                                 (format #f "In constructor #<~a>: argument *~a* must be *~a*! **Got ~a**"
                                         ',class-name
                                         ',field-name
                                         ,(let ((type-str (symbol->string field-type)))
                                            (if (string-ends? type-str "?")
                                                (substring type-str 0 (- (string-length type-str) 1))
                                                type-str))
                                         (object->string ,field-name)))))
                          field-names field-types)))

    `(define* (,class-name 
               ,@(map (lambda (field-name field-default)
                       (if field-default
                           `(,field-name ,field-default)
                           field-name))
                     field-names field-defaults))
       
       ,@type-checks
       
       (define %this (inlet ,@(apply append (map (lambda (name) `(',name ,name)) field-names))))
       
       ,@instance-methods
       
       (define (%apply msg . args)
         (if (defined? msg %this #t)
             (apply (%this msg) args)
             (error 'undefined-method (format #f "Method ~a not found" msg))))
       
       ,@(map (lambda (method-sym method-name)
                `(varlet %this ,method-name ,method-sym))
             (map caadr instance-methods) instance-method-names)
       
       (varlet %this :apply %apply)
       
       %this)))

) ; end of begin
) ; end of define-library
