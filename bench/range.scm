(import (scheme time)
        (liii range)
        (liii list))

(define start-time (current-jiffy))

(define result
  (((range 1 1000) :map
    (lambda (y)
      (((range 1 10000)
        :map (lambda (x) (* x x)))
       :count odd?)))
   :collect))

(display (car result))
(newline)

(define end-time (current-jiffy))
(display (- end-time start-time))
