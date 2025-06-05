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

(import (srfi srfi-2) 
        (liii list)
        (liii error)
        (liii string))

(export 
  @ typed-define
  define-case-class-2 chain-apply case-class? display* object->string)

(begin

(define-macro (@ . paras)
  (letrec* ((slot? (lambda (x) (equal? '_ x)))
            (exprs (filter (lambda (x) (not (slot? x))) paras))
            (slots (filter slot? paras))

            (exprs-sym-list (map (lambda (x) (gensym)) exprs))  
            (slots-sym-list (map (lambda (x) (gensym)) slots))

            (lets (map list exprs-sym-list exprs))

            (parse
              (lambda (exprs-sym-list slots-sym-list paras)
                (cond
                  ((null? paras) paras)
                  ((not (list? paras)) paras)
                  ((slot? (car paras)) 
                    `(,(car slots-sym-list) 
                      ,@(parse exprs-sym-list (cdr slots-sym-list) (cdr paras))))
                  (else 
                    `(,(car exprs-sym-list) 
                      ,@(parse (cdr exprs-sym-list) slots-sym-list (cdr paras))))))))
                
  `(let ,lets 
        (lambda ,slots-sym-list 
                ,(parse exprs-sym-list slots-sym-list paras)))))

(define-macro (typed-define name-and-params body . rest)
  (let* ((name (car name-and-params))
          (params (cdr name-and-params))
          (param-names (map car params)))

        `(define* 
            (,name 
            ,@(map  
              (lambda (param)
                (let  ((param-name (car param))
                      (type-pred (cadr param))
                      (default-value (cddr param)))
                      (if (null? default-value)
                          param-name
                          `(,param-name ,(car default-value)))))
              params))

        ;; Runtime type check                    
        ,@(map (lambda (param)
                (let* ((param-name (car param))
                      (type-pred (cadr param))
                      ;;remove the '?' in 'type?'
                      (type-name-str 
                         (let ((s (symbol->string type-pred)))
                           (if (and (positive? (string-length s))
                                    (char=? (string-ref s (- (string-length s) 1)) #\?))
                               (substring s 0 (- (string-length s) 1))
                               s))))

                  `(unless 
                      (,type-pred ,param-name)
                      (type-error 
                          (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**"
                                ,name
                                ',param-names
                                ',param-name
                                ,type-name-str
                                (object->string ,param-name))))))
              params)
       ,body
       ,@rest)))

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
                          field-names field-types))
         
         ;; Generate class-name?
         (class-name? (string->symbol (string-append (symbol->string class-name) "?"))))

    `(begin
       (define (,class-name? obj)
         (and (let? obj)
              (defined? '*class-name* obj #t)
              (eq? (obj '*class-name*) ',class-name)))
       
       (define* (,class-name 
                 ,@(map (lambda (field-name field-default)
                         (if field-default
                             `(,field-name ,field-default)
                             field-name))
                       field-names field-defaults))
         
         ,@type-checks
         
         (define %this (inlet ,@(apply append (map (lambda (name) `(',name ,name)) field-names))
                               '*type* 'case-class
                               '*class-name* ',class-name))
         
         ,@instance-methods
         
         ;; Standard methods
         (define (%to-string)
           (let ((field-strings
                  (list ,@(map (lambda (field-name)
                                `(string-append
                                  ,(string-append ":" (symbol->string field-name)) " "
                                  (object->string ,field-name)))
                              field-names))))
             (let loop ((strings field-strings)
                        (acc ""))
               (if (null? strings)
                   (string-append "(" ,(symbol->string class-name) " " acc ")")
                   (loop (cdr strings)
                         (if (zero? (string-length acc))
                             (car strings)
                             (string-append acc " " (car strings))))))))
         
         (define (%equals that)
           (and (,class-name? that)
                ,@(map (lambda (field-name)
                        `(equal? ,field-name (that ',field-name)))
                      field-names)))
         
         (define (%apply msg . args)
           (if (defined? msg %this #t)
               (apply (%this msg) args)
               (error 'undefined-method (format #f "Method ~a not found" msg))))
         
         ;; Register all methods
         ;; call zero-parameter methods directly
         ,@(map (lambda (method-sym method-name method-def)
                  (let* ((method-params (if (>= (length method-def) 2)
                                           (cdadr method-def)
                                           '()))
                         (is-zero-param? (null? method-params)))
                    (if is-zero-param?
                        `(varlet %this ,method-name (,method-sym))
                        `(varlet %this ,method-name ,method-sym))))
               (map caadr instance-methods) 
               instance-method-names
               instance-methods)
         
         (varlet %this :to-string (%to-string))
         (varlet %this :equals %equals)
         (varlet %this :apply %apply)
         
         %this))))

(define (chain-apply args proc)
  (if (null? args)
      proc
      (apply proc args)))

(define (case-class? obj)
  (and  (let? obj) 
        (defined? '*type* obj #t)
        (eq? (obj '*type*) 'case-class)))

(define (display* . params)
  (define (%display x)
    (if (case-class? x)
        (display (x :to-string))
        (display x)))
  (for-each %display params))

(define s7-object->string object->string)

(define (object->string x)
  (if (case-class? x)
      (x :to-string)
      (s7-object->string x)))

) ; end of begin
) ; end of define-library
