(import (liii list))

(define (rich-string str)
  (define %this (inlet 'str str))
  (define (%equals that)
    (string=? (%this 'str) (that 'str)))
  (define (%is-instance-of x) (eq? x 'rich-string))
  (define (%get-str) (%this 'str))
  (varlet %this 'equals %equals 'get-str %get-str 'is-instance-of %is-instance-of)
  %this)

(define (rich-integer data)
  (define %this (inlet 'data data))
  (define (%is-instance-of x) (eq? x 'rich-integer))
  (define (%equals that)
    (= (%this 'data) (that 'data)))
  (define (%set-number! new-num) (begin (let-set! %this 'data new-num) %this))
  (define (%get-number) (%this 'data))
  (define (%to-string) (number->string (%this 'data)))
  (define (%to-rich-string) (rich-string (number->string (%this 'data))))
  (define (%to n) 
    (if (< n (%this 'data)) 
      (list) 
      (iota (+ (- n (%this 'data)) 1) (%this 'data))))
  (varlet %this 
    'equals %equals 
    'set-number! %set-number! 
    'get-number %get-number 
    'to-string %to-string 
    'to-rich-string %to-rich-string
    'is-instance-of %is-instance-of
    'to %to)
  %this)

(define num1 (rich-integer 1))
(define num2 (rich-integer 2))
(define str1 (rich-string "test"))

((num1 :equals) num2)
(num1 :set-number! 2)
((num1 :equals) num2)
((num1 :get-number))
(num1 :set-number! 7)
((num1 :to-string))
(num1 :is-instance-of 'rich-integer)

(define str2 ((num1 :to-rich-string)))

((str1 :equals) str2)

(str2 :is-instance-of 'rich-integer)

((((num2 :set-number! 10) :set-number! 11) :to-string))


((num2 :to) 50)