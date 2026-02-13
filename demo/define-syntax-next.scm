(define-library (demo define-syntax-next)
  (import (demo define-syntax-def))
  (export ans)
  (begin
    (define (ans) (+ 0 answer))
    (newline)))
