(define (read . port)
  (if (null? port)
    (g_goldfish-read (current-input-port))
    (g_goldfish-read port)))

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
  (display "[put]symbol.key.value: ") (display symbol)
  (display " . ") (display key)
  (display " . ") (display value) (newline)
  (let ((props (hash-table-ref *symbol-properties* symbol)))
    (if props
        (hash-table-set! props key value)
        (let ((new-props (make-hash-table)))
          (hash-table-set! new-props key value)
          (hash-table-set! *symbol-properties* symbol new-props)))
    value))

(define (getprop symbol key)
  (display "[get]symbol.key: ") (display symbol)
  (display " . ") (display key) (newline)
  (let ((props (hash-table-ref *symbol-properties* symbol)))
    (if props
        (hash-table-ref props key)
        #f)))

(define (remprop symbol key)
  (let ((props (hash-table-ref *symbol-properties* symbol)))
    (if props
        (hash-table-set! props key #f))
    #f))

(define s7-gensym gensym)
(define (gensym x)
  (cond
    ((symbol? x) (s7-gensym (symbol->string x)))
    (else        (s7-gensym x))))

;; API provided by psyntax
(define $sc-put-cte             #f)
(define sc-expand               #f)
(define $make-environment       #f)
(define environment?            #f)
(define interaction-environment #f)
(define identifier?             #f)
(define unwrap-syntax           #f)
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
  (let ((target-env (if (pair? env) (car env) (rootlet))))
    (if (and (pair? expr)
             (equal? (car expr) "noexpand"))
        (%primitive-eval (cadr expr) target-env)
        (%primitive-eval expr target-env))))

(load "scheme/psyntax.pp")

(define syntax->datum syntax-object->datum)
(define datum->syntax datum->syntax-object)

(define (file-exists? path)
  (if (string? path)
    (if (not (g_access path 0)) ; F_OK
      #f
      (if (g_access path 1) ; R_OK
          #t
          (error 'permission-error (string-append "No permission: " path))))
    (error 'type-error "(file-exists? path): path should be string")))


(define (find-file-in-paths filename)
  (if (file-exists? filename)
      filename
      (let loop ((paths *load-path*))
        (if (null? paths)
            #f
            (let ((full-path (string-append (car paths) "/" filename)))
              (if (file-exists? full-path)
                  full-path
                  (loop (cdr paths))))))))

(define (psyntax-load filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((expr (read)))
        (unless (eof-object? expr)
          (begin
            ; (display "eval: ") (write expr) (newline)
            (let ((expanded (sc-expand expr #f #f #f)))
              ; (display "nxpa: ") (write expanded) (newline)
              (eval expanded)))
          (loop (read)))))))

(define (load fn)
  (display "loading: ") (display fn) (newline)
  (let ((fn* (find-file-in-paths fn)))
    (if fn*
      (psyntax-load fn*)
      (error "not found" fn*))))

(set! *#readers*
  (append
    (list
      ;; #` (quasisyntax)
      (cons #\` (lambda (str)
                  (list 'quasisyntax (read))))
      ;; #, (unsyntax)
      (cons #\, (lambda (str)
                  ;; 检查后面是否跟着 @，即 #,@ (unsyntax-splicing)
                  (let ((next-char (peek-char)))
                    (if (eq? next-char #\@)
                        (begin
                          (read-char) ;; 消耗掉 @
                          (list 'unsyntax-splicing (read)))
                        (list 'unsyntax (read))))))
      ;; #' (syntax) - 保持之前的修正
      (cons #\' (lambda (str)
                  (list 'syntax (read)))))
    *#readers*))

; (load "demo/x.scm")

(display "~~~~~~~~~~") (newline)
(define (register-primitive sym value)
  ($sc-put-cte sym value                '*top*))
  ; ($sc-put-cte sym (cons 'global value) '*sc-expander*))

(register-primitive 'gcd   gcd)
; (register-primitive 'pair? pair?)
(display "~~~~~~~~~~") (newline)

(load "scheme/s7-shim.scm")
(load "scheme/r7rs-small.scm")

(define load psyntax-load-r7rs)
(load "demo/define-syntax-last.scm")

(display *symbol-properties*) (newline)

(newline)
