(define-syntax define-macro
  (lambda (x)
    (syntax-case x ()
      ((_ (macro . args) body1 body ...)
       #'(define-macro macro (lambda args body1 body ...)))
      ((_ macro transformer)
       #'(define-syntax macro
            (lambda (y)
              (syntax-case y ()
                ((_ . args)
                 (let ((v (syntax->datum (syntax args))))
                   (datum->syntax y (apply transformer v)))))))))))

(define-syntax syntax-rules
  (lambda (stx)
    (syntax-case stx ()
      ((_ (lit ...) (pat tmpl) ...)
       #'(lambda (stx*)
           (syntax-case stx* (lit ...)
             (pat (syntax tmpl)) ...)))
      ((_ (lit ...) (pat tmpl) ... . __)
       (syntax-violation 'syntax-rules "invalid syntax" stx)))))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
         ((_ . pattern) template))))))

(define (list-head l k)
  (let loop ((l l)
             (k k))
    (if (zero? k)
        '()
        (cons (car l) (loop (cdr l) (- k 1))))))

(define-syntax-rule (lambda* (spec ...) body ...)
  (lambda*-specs (spec ...) () () body ...))

;; 归一化
(define-syntax lambda*-specs
  (syntax-rules ()
    ((_ () ((id d) ...) body ...)
     (lambda _args
       (lambda*-bind ((id d) ...) _args body ...)))
    ;; 带显式默认值
    ((_ ((x d) . rest) (acc ...) body ...)
     (lambda*-specs rest (acc ... (x d)) body ...))
    ;; 无默认值 → #f
    ((_ (x . rest) (acc ...) body ...)
     (lambda*-specs rest (acc ... (x #f)) body ...))))

;; 顺序绑定
(define-syntax lambda*-bind
  (syntax-rules ()
    ((_ () _args body ...)
     (let () body ...))
    ((_ ((x d) . more) _args body ...)
     (let ((x (if (null? _args) d (car _args)))
           (_rest (if (null? _args) '() (cdr _args))))
       (lambda*-bind more _rest body ...)))))

(define-syntax define*
  (lambda (x)
    (syntax-case x ()
      ((_ (id . args) b0 b1 ...)
       #'(define id (lambda* args (let () b0 b1 ...))))
      ((_ id val) (identifier? (syntax id))
       #'(define id val)))))




; (define (and-map f lst)
;   (let loop ((result #t)
;              (l lst))
;     (and result
;          (or (and (null? l)
;                   result)
;              (loop (f (car l)) (cdr l))))))
;
; (define-syntax define-values
;   (lambda (orig-form)
;     (syntax-case orig-form ()
;       ((_ () expr)
;        ;; XXX Work around the lack of hygienic top-level identifiers
;        (with-syntax (((dummy) (generate-temporaries '(dummy))))
;          #`(define dummy
;              (call-with-values (lambda () expr)
;                (lambda () #f)))))
;       ((_ (var) expr)
;        (identifier? (syntax var))
;        #`(define var
;            (call-with-values (lambda () expr)
;              (lambda (v) v))))
;       ((_ (var0 ... varn) expr)
;        (and-map identifier? #'(var0 ... varn))
;        ;; XXX Work around the lack of hygienic toplevel identifiers
;        (with-syntax (((dummy) (generate-temporaries '(dummy))))
;          #`(begin
;              ;; Avoid mutating the user-visible variables
;              (define dummy
;                (call-with-values (lambda () expr)
;                  (lambda (var0 ... varn)
;                    (list var0 ... varn))))
;              (define var0
;                (let ((v (car dummy)))
;                  (set! dummy (cdr dummy))
;                  v))
;              ...
;              (define varn
;                (let ((v (car dummy)))
;                  (set! dummy #f)  ; blackhole dummy
;                  v)))))
;       ((_ var expr)
;        (identifier? (syntax var))
;        #'(define var
;            (call-with-values (lambda () expr)
;              list)))
;       ((_ (var0 ... . varn) expr)
;        (and-map identifier? #'(var0 ... varn))
;        ;; XXX Work around the lack of hygienic toplevel identifiers
;        (with-syntax (((dummy) (generate-temporaries '(dummy))))
;          #`(begin
;              ;; Avoid mutating the user-visible variables
;              (define dummy
;                (call-with-values (lambda () expr)
;                  (lambda (var0 ... . varn)
;                    (list var0 ... varn))))
;              (define var0
;                (let ((v (car dummy)))
;                  (set! dummy (cdr dummy))
;                  v))
;              ...
;              (define varn
;                (let ((v (car dummy)))
;                  (set! dummy #f)  ; blackhole dummy
;                  v))))))))
