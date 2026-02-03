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

(define s7-eval eval)
(define (eval x) (s7-eval (cadr x)))

(set! (*s7* 'symbol-quote?) #t)

(load "scheme/psyntax.pp")

(define (file-exists? path)
  (if (string? path)
    (if (not (g_access path 0)) ; F_OK
      #f
      (if (g_access path 1) ; R_OK
          #t
          (error 'permission-error (string-append "No permission: " path))))
    (error 'type-error "(file-exists? path): path should be string")))


(define %primitive-eval s7-eval)
(define %primitive-load load)


(define (filter pred l)
  (let recur ((l l))
    (if (null? l)
        l
        (let ((head (car l))
              (tail (cdr l)))
          (if (pred head)
              (let ((new-tail (recur tail)))
                (if (eq? tail new-tail)
                    l
                    (cons head new-tail)))
              (recur tail))))))

(define (psyntax-expand expr)
  (display "evaluating ") (display expr) (newline)
  (let ((expr (if (and (list? expr) (= (length expr) 1)) (car expr) expr)))
    (cond
      ;; 情况 A：处理 define-library
      ((and (pair? expr) (eq? (car expr) 'define-library))
       (let* ((body (cddr expr))
              ;; 1. 提取所有的 (import ...) 语句
              (imports (filter (lambda (x) (and (pair? x) (eq? (car x) 'import))) body))
              (exports (filter (lambda (x) (and (pair? x) (eq? (car x) 'export))) body))
              ;; 2. 提取所有的 (begin ...) 或 顶层定义
              (real-body (filter (lambda (x) (not (member (car x) '(import export)))) body)))

         (display real-body) (newline)

         (for-each (lambda (imp)
                     (r7rs-import-library-filename (cdr imp)))
                   imports)

         (sc-expand real-body)))

      ;; 情况 B：单独的 import 语句
      ((and (pair? expr) (eq? (car expr) 'import))
       (r7rs-import-library-filename (cdr expr))
       '(begin)) ; 返回一个空表达式，因为 import 已经处理完了

      ;; 情况 C：普通表达式
      (else (sc-expand expr)))))

(define (eval expr . env)
  ; (display "evaling ") (display expr) (newline)
  (if (and (list? expr)
           (string? (car expr))
           (string=? (car expr) "noexpand"))
    (%primitive-eval (cadr expr))
    (%primitive-eval (sc-expand expr))))

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

(define (load filename)
  ; (display "loading ") (display filename) (newline)
  (let ((abs-path (find-file-in-paths filename)))
    (display "loadinG ") (display abs-path) (newline)
    (if abs-path
        (with-input-from-file abs-path
          (lambda ()
            (let loop ((expr (read))
                       (forms '()))
              (if (eof-object? expr)
                  (eval (reverse forms))
                  (loop (read) (cons expr forms))))))
        (error 'load (string-append "file not found: " abs-path)))))

;; ---

(define (delete-file path)
  (if (not (string? path))
    (error 'type-error "(delete-file path): path should be string")
    (if (not (file-exists? path))
      (error 'read-error (string-append path " does not exist"))
      (g_delete-file path))))

(define define-library module)

; ; 0-clause BSD
; ; Adapted from S7 Scheme's r7rs.scm
; (define-macro (define-library libname . body) ; |(lib name)| -> environment
;   `(define ,(symbol (object->string libname))
;      (with-let (sublet (unlet)
;                  (cons 'import import)
;                  (cons '*export* ())
;                  (cons 'export (define-macro (,(gensym) . names)
;                                  `(set! *export* (append ',names *export*)))))
;        ,@body
;        (apply inlet
;               (map (lambda (entry)
;                      (if (or (member (car entry) '(*export* export import))
;                              (and (pair? *export*)
;                                   (not (member (car entry) *export*))))
;                          (values)
;                          entry))
;                    (curlet))))))
;
; (unless (defined? 'r7rs-import-library-filename)
;   (define (r7rs-import-library-filename libs)
;     (when (pair? libs)
;       (let ((lib-filename (let loop ((lib (if (memq (caar libs) '(only except prefix rename))
;                                               (cadar libs)
;                                               (car libs)))
;                                      (name ""))
;                             (set! name (string-append name (symbol->string (car lib))))
;                             (if (null? (cdr lib))
;                                 (string-append name ".scm")
;                                 (begin
;                                   (set! name (string-append name "/"))
;                                   (loop (cdr lib) name))))))
;         (when (not (defined? (symbol (object->string (car libs)))))
;           ;(display "Loading ") (display lib-filename) (newline)
;           (load lib-filename))
;         (r7rs-import-library-filename (cdr libs)))))
;   )
;
; (define-macro (import . libs)
;   `(begin
;      (r7rs-import-library-filename ',libs)
;      (varlet (curlet)
;        ,@(map (lambda (lib)
;                 (case (car lib)
;                   ((only)
;                    `((lambda (e names)
;                        (apply inlet
;                               (map (lambda (name)
;                                      (cons name (e name)))
;                                    names)))
;                      (symbol->value (symbol (object->string (cadr ',lib))))
;                      (cddr ',lib)))
;                   ((except)
;                    `((lambda (e names)
;                        (apply inlet
;                               (map (lambda (entry)
;                                      (if (member (car entry) names)
;                                          (values)
;                                          entry))
;                                    e)))
;                      (symbol->value (symbol (object->string (cadr ',lib))))
;                      (cddr ',lib)))
;                   ((prefix)
;                    `((lambda (e prefx)
;                        (apply inlet
;                               (map (lambda (entry)
;                                      (cons (string->symbol 
;                                             (string-append (symbol->string prefx) 
;                                                            (symbol->string (car entry)))) 
;                                            (cdr entry)))
;                                    e)))
;                      (symbol->value (symbol (object->string (cadr ',lib))))
;                      (caddr ',lib)))
;                   ((rename)
;                    `((lambda (e names)
;                        (apply inlet
;                               (map (lambda (entry)
;                                      (let ((info (assoc (car entry) names)))
;                                        (if info
;                                            (cons (cadr info) (cdr entry))
;                                            entry))) 
;                                    e)))
;                      (symbol->value (symbol (object->string (cadr ',lib))))
;                      (cddr ',lib)))
;                   (else
;                    `(let ((sym (symbol (object->string ',lib))))
;                       (if (not (defined? sym))
;                           (format () "~A not loaded~%" sym)
;                           (symbol->value sym))))))
;               libs))))
