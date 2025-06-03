(define-syntax chained-lambda
  (syntax-rules ()
    ((_ (args ...) body ...)
     (lambda (args ... . rest)
       (let ((result (begin body ...)))
         (if (null? rest)
             result
             (apply result rest)))))))

(define (gen-dispatcher table)
  (lambda args
    (cond
      ((null? args) (error 'me "unexpected"))
      (else
        (apply
          (symbol-hashtable-ref table (car args) (lambda r (error 'me "unexpected")))
          (cdr args))))))

(define (rint x)
  (let ((self '*))
    (define table (make-hashtable symbol-hash eq?))
    (set! self (gen-dispatcher table))

    (symbol-hashtable-set! table 'set! (chained-lambda (y) (set! x y) self))
    (symbol-hashtable-set! table 'sqrt 
      (lambda ()
        (if (< x 0) 
            (error)
            (inexact->exact (floor (sqrt x))))))
    (symbol-hashtable-set! table 'to-string (lambda () (number->string x)))
    self))

(define (repeat n proc)
  (when (>= n 0)
        (proc)
        (repeat (- n 1) proc)))

(define (timing msg thunk)
  (let* ((start-time (current-time))
         (result (thunk))
         (end-time (current-time)))
    (format #t "~a elapsed ~a ms\n" msg (/ (time-nanosecond (time-difference end-time start-time)) 1000000.0))))

(timing "rint sqrt" (lambda () (repeat 100000 (lambda () ((rint 65536) 'sqrt)))))
(timing "rint to-string" (lambda () (repeat 100000 (lambda () ((rint 65536) 'to-string)))))
(timing "rint set!+sqrt"
        (lambda ()
          (let loop ((n 100000))
            (when (> n 0)
              ((rint 65536) 'set! n 'sqrt)
              (loop (- n 1))))))
