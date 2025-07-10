(import (liii range)
        (scheme time))

(((range 0 1000)
  :map
  (lambda (y)
    (((range 0 10000) :map (lambda (x) (* x x)))
     :filter (lambda (z) (zero? (modulo z 3)))
     :collect)))
 :collect)

(display (rich-list :range 1 10 1 :collect))
(newline)

(define start-time (current-jiffy))
(define result
  ((rich-vector :range 0 1000 1)
   :map
   (lambda (y)
      ((rich-vector :range 0 10000 1)
       :map (lambda (x) (* x x))
       :filter (lambda (z) (zero? (modulo z 3)))
       :collect))
   :collect))
(define end-time (current-jiffy))

(display "Length of each result: ")
(display (length (result 0)))
(newline)

(display* "Elapsed time: " (/ (- end-time start-time) 1000.0) " ms\n")
