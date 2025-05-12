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

(import (scheme time))

(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display* msg (number->string (- end start)) "\n")))

(define (repeat n proc)
  (when (>= n 0)
        (proc)
        (repeat (- n 1) proc)))

(define (prim-sqrt data)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
      (inexact->exact (floor (sqrt data)))))

(define-case-class rich-integer2 ((data integer?))
  (define (%to-string)
    (number->string data)))

(define (rint x)
  (lambda (msg . args)
    (let ((env (funclet rint)))
      (varlet env 'data x)
      (let1 r (case msg
                ((:to-string)
                 ((env '%to-string)))
                ((:to-rich-string)
                 ((env '%to-rich-string)))
                ((:sqrt)
                 ((env '%sqrt))))
        (cutlet env 'data)
        r))))

(define rint-lambda
(lambda args (define (@is-type-of obj) (and (case-class? obj) (obj :is-instance-of 'rich-integer))) (define (@max-value) 9223372036854775807) (define (@min-value) -9223372036854775808) (define (is-normal-function? msg) (and (symbol? msg) (char=? (string-ref (symbol->string msg) 0) #\:))) (define (static-dispatcher msg . args) (cond ((eq? msg :is-type-of) (apply @is-type-of args)) ((eq? msg :max-value) (apply @max-value args)) ((eq? msg :min-value) (apply @min-value args)) (else (value-error "No such static method " msg)))) (define* (make-case-class-rich-integer data) (unless (integer? data) (type-error (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" make-case-class-rich-integer '(data) 'data "integer" (object->string data)))) (define {gensym}-171 #f) (define (%this . xs) (if (null? xs) {gensym}-171 (apply {gensym}-171 xs))) (define (%is-instance-of x) (eq? x 'rich-integer)) (define (%equals that) (unless (case-class? that) (type-error (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" %equals '(that) 'that "case-class" (object->string that)))) (and (that :is-instance-of 'rich-integer) (equal? data (that 'data)))) (define (%apply . args) (cond ((null? args) (value-error rich-integer "Apply on zero args is not implemented")) ((equal? ((symbol->string (car args)) 0) #\:) (value-error rich-integer "No such method: " (car args))) (else (value-error rich-integer "No such field: " (car args))))) (define (%to-string) (let ((field-strings (list (string-append ":data" " " (object->string data))))) (let loop ((strings field-strings) (acc "")) (if (null? strings) (string-append "(" "rich-integer" " " acc ")") (loop (cdr strings) (if (zero? (string-length acc)) (car strings) (string-append acc " " (car strings)))))))) (define (%get) data) (define (%to n) (unless (integer? n) (type-error (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" %to '(n) 'n "integer" (object->string n)))) (if (< n data) (rich-list (list)) (rich-list (iota (+ (- n data) 1) data)))) (define (%until n) (unless (integer? n) (type-error (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" %until '(n) 'n "integer" (object->string n)))) (if (<= n data) (rich-list (list)) (rich-list (iota (+ (- n data)) data)))) (define (%to-rich-char) (rich-char data)) (define (%to-string) (number->string data)) (define (%sqrt) (if (< data 0) (value-error (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data)) (inexact->exact (floor (sqrt data))))) (define (instance-dispatcher) (lambda (msg . args) (cond ((eq? msg :sqrt) (apply %sqrt args)) ((eq? msg :is-instance-of) (apply %is-instance-of args)) ((eq? msg :equals) (apply %equals args)) ((eq? msg :to-string) (%to-string)) ((eq? msg :this) (apply %this args)) ((eq? msg :data) (rich-integer (car args))) ((is-normal-function? msg) (case msg ((:get) (apply %get args)) ((:to) (apply %to args)) ((:until) (apply %until args)) ((:to-rich-char) (apply %to-rich-char args)) ((:to-string) (apply %to-string args)) ((:sqrt) (apply %sqrt args)) (else (value-error rich-integer "No such method: " msg)))) ((eq? msg 'data) data) (else (apply %apply (cons msg args)))))) (set! {gensym}-171 (instance-dispatcher)) {gensym}-171) (if (null? args) (make-case-class-rich-integer) (let ((msg (car args))) (cond ((in? msg (list :max-value :min-value :is-type-of)) (apply static-dispatcher args)) ((and (zero? 1) (in? :apply (list :max-value :min-value))) (apply static-dispatcher (cons :apply args))) (else (apply make-case-class-rich-integer args))))))
)

(with-let (funclet rint)
  (define (%to-string)
    (number->string data))
  (varlet (funclet rint) '%to-string %to-string)
  
  (define (%to-rich-string)
    (rich-string (%to-string)))
  (varlet (funclet rint) '%to-rich-string %to-rich-string)
  
  (define (%sqrt)
        (if (< data 0)
            (value-error
              (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
            (inexact->exact (floor (sqrt data)))))
  (varlet (funclet rint) '%sqrt %sqrt))

(display* "Bench of number->string:\n")
(timing "prim%to-string:\t\t\t" (lambda () (repeat 10000 (lambda () (number->string 65536)))))
(timing "rich-integer%to-string:\t\t" (lambda () (repeat 10000 (lambda () ((rich-integer 65536) :to-string)))))
(timing "rich-integer2%to-string:\t" (lambda () (repeat 10000 (lambda () ((rich-integer2 65536) :to-string)))))
(timing "rint%to-string:\t\t\t" (lambda () (repeat 10000 (lambda () ((rint 65536) :to-string)))))

(display* ((rint 65535) :to-string))
(newline)

(display* "\n\nBench of SQRT:\n")
(timing "prim%sqrt:\t\t\t" (lambda () (repeat 10000 (lambda () (prim-sqrt 65536)))))
(timing "rint%sqrt:\t\t\t" (lambda () (repeat 10000 (lambda () ((rint 65536) :sqrt)))))
(timing "rint-lambda%sqrt:\t\t" (lambda () (repeat 10000 (lambda () ((rint-lambda 65536) :sqrt)))))

(display "\nBench of integer\n")
(timing "rich-integer%sqrt:\t\t" (lambda () (repeat 10000 (lambda () ((rich-integer 65536) :sqrt)))))

(display* ((rint 65535) :sqrt))

; slow because of rich-string
; (timing "rint%to-rich-string " (lambda () (repeat 1000 (lambda () (((rint 65536) :to-rich-string) :length)))))
