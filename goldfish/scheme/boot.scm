(define (void) (if #f #f))

(define (andmap f first . rest)
  (if (null? rest)
      (let andmap ((first first))
        (if (null? first)
            #t
            (let ((x (car first))
                  (rest (cdr first)))
              (if (null? rest)
                  (f x)
                  (and (f x) (andmap rest))))))
      (let andmap ((first first) (rest rest))
        (if (null? first)
            #t
            (let ((x (car first))
                  (xr (map car rest))
                  (next-first (cdr first))
                  (next-rest (map cdr rest)))
              (if (null? next-first)
                  (apply f (cons x xr))
                  (and (apply f (cons x xr))
                       (andmap next-first next-rest))))))))

(define (ormap f list1)
  (and (not (null? list1))
       (or (f (car list1))
           (ormap f (cdr list1)))))

(define *symbol-properties* (make-hash-table))

(define (putprop symbol key value)
  (let ((props (hash-table-ref *symbol-properties* symbol)))
    (if props
        (hash-table-set! props key value)
        (let ((new-props (make-hash-table)))
          (hash-table-set! new-props key value)
          (hash-table-set! *symbol-properties* symbol new-props)))
    value))

(define (getprop symbol key)
  (let ((props (hash-table-ref *symbol-properties* symbol)))
    (if props
        (hash-table-ref props key)
        #f)))

(define (remprop symbol key)
  (let ((props (hash-table-ref *symbol-properties* symbol)))
    (if props
        (hash-table-set! props key #f))
    #f))


;; API provided by psyntax
(define $sc-put-cte             #f)
(define sc-expand               #f)
(define $make-environment       #f)
(define environment?            #f)
(define interaction-environment #f)
(define identifier?             #f)
(define syntax->list            #f)
(define syntax-object->datum    #f)
(define datum->syntax-object    #f)
(define generate-temporaries    #f)
(define free-identifier=?       #f)
(define bound-identifier=?      #f)
(define literal-identifier=?    #f)
(define syntax-error            #f)
(define $syntax-dispatch        #f)

(define syntax->vector          #f)

(set! (*s7* 'symbol-quote?) #t)

(define %primitive-eval eval)
(define %primitive-load load)

(define (eval expr . env)
  ; (display "evaling ") (display expr) (newline)
  (if (and (list? expr)
           (string? (car expr))
           (string=? (car expr) "noexpand"))
    (%primitive-eval (cadr expr))
    (%primitive-eval (sc-expand expr))))

(load "scheme/psyntax.pp")


(define (file-exists? path)
  (if (string? path)
    (if (not (g_access path 0)) ; F_OK
      #f
      (if (g_access path 1) ; R_OK
          #t
          (error 'permission-error (string-append "No permission: " path))))
    (error 'type-error "(file-exists? path): path should be string")))


; (define (find-file-in-paths filename)
;   (if (file-exists? filename)
;       filename
;       (let loop ((paths *load-path*))
;         (if (null? paths)
;             #f
;             (let ((full-path (string-append (car paths) "/" filename)))
;               (if (file-exists? full-path)
;                   full-path
;                   (loop (cdr paths))))))))

(define (psyntax-load filename)
  (if (file-exists? filename)
      (let ((forms '()))
        (with-input-from-file filename
          (lambda ()
            (let loop ((expr (read)))
              (if (eof-object? expr)
                  (begin
                    ; (display `(begin ,@forms)) (newline)
                    ;; 核心：包装成一个巨大的 begin，一次性交给 psyntax
                    (let ((expanded (sc-expand `(begin ,@(reverse forms)))))
                      ; (display expanded) (newline) ; 调试用：查看最终生成的 s7 代码
                      (eval expanded)))
                  (begin
                    (set! forms (cons expr forms))
                    (loop (read))))))))
      (error 'load (string-append "file not found: " filename))))

(define load psyntax-load)

; (define (load filename)
;   ; (display "loading ") (display filename) (newline)
;   (let ((abs-path (find-file-in-paths filename)))
;     ; (display "loadinG ") (display abs-path) (newline)
;     (if abs-path
;         (with-input-from-file abs-path
;           (lambda ()
;             (let loop ((expr (read))
;                        (forms '()))
;               (if (eof-object? expr)
;                   (eval (reverse forms))
;                   (loop (read) (cons expr forms))))))
;         (error 'load (string-append "file not found: " abs-path)))))

(load "goldfish/scheme/r7rs.scm")
