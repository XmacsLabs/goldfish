(define-syntax define-library
  (lambda (stx)
    (define (library-name->symbol name-stx)
      (let ((name (syntax-object->datum name-stx)))
        (if (symbol? name)
            name
            (string->symbol
              (let loop ((lst name))
                (if (null? lst)
                    ""
                    (let ((str (let ((x (car lst)))
                                 (if (symbol? x) (symbol->string x) (number->string x)))))
                      (if (null? (cdr lst))
                          str
                          (string-append str "." (loop (cdr lst)))))))))))

    ;; 辅助函数：从代码块中提取 define/define-syntax 定义的名字
    (define (extract-defined-names code-list)
      (let loop ((lst code-list) (acc '()))
        (if (null? lst)
            acc
            (let ((form (syntax-object->datum (car lst))))
              (cond
               ((and (pair? form) (memq (car form) '(define define-syntax)))
                (loop (cdr lst) (cons (if (pair? (cadr form)) (caadr form) (cadr form)) acc)))
               (else (loop (cdr lst) acc)))))))

    (define (partition-decls decl-context decls exports imports code)
      (syntax-case decls (export import begin)
        (() (values exports imports (reverse code)))
        (((export clause ...) . decls)
         (partition-decls decl-context (syntax decls) (append exports #'(clause ...)) imports code))
        (((import clause ...) . decls)
         (let ((processed-imps
                (map (lambda (imp)
                       (let ((sym (library-name->symbol imp)))
                         (datum->syntax-object decl-context sym)))
                     #'(clause ...))))
           (partition-decls decl-context (syntax decls) exports (append imports processed-imps) code)))
        (((begin expr ...) . decls)
         (partition-decls decl-context (syntax decls) exports imports
                          (append (reverse #'(expr ...)) code)))
        ((other . decls)
         (partition-decls decl-context (syntax decls) exports imports (cons (syntax other) code)))))

    (syntax-case stx ()
      ((_ name decl ...)
       (let* ((keyword (syntax-case stx () ((k . _) (syntax k))))
              (lib-sym (library-name->symbol (syntax name)))
              (mod-id  (datum->syntax-object keyword lib-sym)))
         (call-with-values
           (lambda () (partition-decls keyword #'(decl ...) '() '() '()))
           (lambda (exports imports code)
             (newline) (display "============================")
             (newline) (display "=== define-library start ===") (newline)
             (newline) (display "exports: ") (display exports) (newline)
             (newline) (display "imports: ") (display imports) (newline)
             (newline) (display "--- START OF CODE ---") (newline)
             (for-each (lambda (form)
                         (write (syntax-object->datum form))
                         (newline))
                       code)
             (display "--- END OF CODE ---") (newline)
             (newline) (display "=== define-library end ===")
             (newline) (display "==========================") (newline)

             ;; 处理导出逻辑
             (let loop ((exps exports) 
                        (final-exports '()) 
                        (rename-aliases '()))
               (if (null? exps)
                   (with-syntax ((mid mod-id)
                                 ((exp-ids ...) final-exports)
                                 ((aliases ...) rename-aliases)
                                 ((imp-ids ...) imports)
                                 ((actual-content ...) code))
                     ;; 这里的生成不再依赖 psyntax 的 rename 语法
                     (let ((so (if (null? imports)
                                 #'(module mid (exp-ids ...)
                                     aliases ...
                                     actual-content ...)
                                 #'(module mid (exp-ids ...)
                                     (import imp-ids ...)
                                     aliases ...
                                     actual-content ...))))
                       (display "----> ") (display (map syntax->datum so))
                       (newline)
                       so))

                   (syntax-case (car exps) (rename)
                     ((rename internal external)
                      (loop (cdr exps)
                            (cons (syntax external) final-exports)
                            ;; 核心：在内部用 alias 把 external 绑定到 internal
                            (cons #'(alias external internal) rename-aliases)))
                     (id
                      (loop (cdr exps)
                            (cons (syntax id) final-exports)
                            rename-aliases))))))))))))

(define *loaded-libraries* (make-hash-table))

(define (psyntax-load-r7rs filename)
  (display "p.loading: ") (display filename) (newline)
  (let ((fn (find-file-in-paths filename)))
    (unless fn (error "File not found" filename))
    (or (hash-table-ref *loaded-libraries* fn)
        (begin
          (hash-table-set! *loaded-libraries* fn #t)
          (with-input-from-file fn
            (lambda ()
              (let loop ((expr (read)))
                (unless (eof-object? expr)
                  (scan-for-imports expr)

                  (let ((expa (sc-expand expr #f #f #f)))

                    (display "expr: ") (display expr) (newline)
                    (display "expa: ") (display expa) (newline)
                    (newline)

                    ; (display "p.symbols: ") (newline)
                    ; (hash-table-for-each *symbol-properties*
                    ;   (lambda (key val)
                    ;     (display "    ") (display key)
                    ;     (display " -> ") (display val)
                    ;     (newline)))

                    (newline)
                    (%primitive-eval expa (rootlet)))
                  (loop (read))))))))))

(define (scan-for-imports expr)
  (cond
    ((and (pair? expr) (eq? (car expr) 'import))
     (for-each load-library-by-spec (cdr expr)))
    ((and (pair? expr) (eq? (car expr) 'define-library))
     (for-each (lambda (item)
                 (if (and (pair? item) (eq? (car item) 'import))
                     (for-each load-library-by-spec (cdr item))))
               (cddr expr)))))




(define hash-table-for-each
  (lambda (ht proc)
    (for-each (lambda (x)
                (proc (car x) (cdr x)))
              ht)))

;; 在模块展开时添加调试
(define (debug-import-resolution import-spec current-module)
  (display "DEBUG - Resolving import: ") (display import-spec)
  (display " for module: ") (display current-module) (newline)
  (newline))

;; 修改 load-library-by-spec 添加调试
(define (load-library-by-spec spec)
  (let* ((parts (map (lambda (x)
                       (if (symbol? x) (symbol->string x) (number->string x)))
                     spec))
         (path-str (let loop ((p parts))
                     (if (null? (cdr p))
                         (car p)
                         (string-append (car p) "/" (loop (cdr p))))))
         (filename (string-append path-str ".scm"))
         (lib-sym (string->symbol path-str)))  ;; 创建与 define-library 中相同的符号
    (display "p.symbols for ") (display spec) (display " -> ") (display lib-sym) (newline)
    (debug-import-resolution spec lib-sym)
    (psyntax-load-r7rs filename)))

; (define (load-library-by-spec spec)
;   (let* ((parts (map (lambda (x)
;                        (if (symbol? x) (symbol->string x) (number->string x)))
;                      spec))
;          (path-str (let loop ((p parts))
;                      (if (null? (cdr p))
;                          (car p)
;                          (string-append (car p) "/" (loop (cdr p))))))
;          (filename (string-append path-str ".scm")))
;     (psyntax-load-r7rs filename)))
