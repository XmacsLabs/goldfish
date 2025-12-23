
(define-library (liii hashlib)
  (export md5)
  (begin

    (define (md5 str) (g_md5 str))

    ) ; end of begin
  ) ; end of define-library
