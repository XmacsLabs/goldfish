(define (libname->symbol stx)
  ;; 将 (scheme base) 这种列表转换成一个唯一的符号
  ;; 比如转换为 '| (scheme base) |' 保持与你旧代码的符号惯例一致
  (let ((obj (syntax->datum stx)))
    (string->symbol (object->string obj))))

(define-syntax define-library
  (lambda (x)
    (syntax-case x (export import begin)
      ((_ libname clause ...)
       (letrec ((parse-clauses
                 (lambda (cls exports imports body)
                   (if (null? cls)
                       (values exports imports body)
                       (syntax-case (car cls) (export import begin)
                         ;; 收集导出：(export a b (rename c d))
                         ((export names ...)
                          (parse-clauses (cdr cls)
                                         (append (syntax (names ...)) exports)
                                         imports body))
                         ;; 收集导入：(import (scheme base) (prefix (lib) p:))
                         ((import libs ...)
                          (parse-clauses (cdr cls) exports 
                                         (append (syntax (libs ...)) imports) body))
                         ;; 收集主体代码：(begin (define x 1) ...)
                         ((begin forms ...)
                          (parse-clauses (cdr cls) exports imports 
                                         (append (syntax (forms ...)) body)))))))
                         ;; 忽略未识别的子句（如 include-library-declarations）
                (_ (parse-clauses (cdr cls) exports imports body)))
         (let-values (((exports imports body)
                       (parse-clauses (syntax (clause ...)) '() '() '())))
           ;; 核心转换：
           ;; 将 R7RS libname (a b c) 转换为 psyntax 期望的扁平符号或结构
           (let ((module-id (libname->symbol (syntax libname))))
             (syntax `(module ,module-id ,exports
                        (import ,@imports)
                        ,@body)))))))))

