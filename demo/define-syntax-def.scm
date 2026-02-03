 (define-syntax def
   (syntax-rules ()
     ((def f (p ...) body)
      (define (f p ...) body))))

 (def f (x)
   (+ x 42))

 (display (f 0))
 (newline)
