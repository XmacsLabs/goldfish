(define-library (liii oop2)
  (import (liii oop) (liii list) (liii string))
  (export define-case-class2)
  (begin
    (define-macro (define-case-class2 class-name fields . private-fields-and-methods)
      (let* ((key-fields
               (map (lambda (field) (string->symbol (string-append ":" (symbol->string (car field)))))
                    fields))
        
             (field-names (map car fields))
             (field-count (length field-names))

             (private-fields (filter (lambda (x)
                                       (and (list? x)
                                            (>= (length x) 2)
                                            (symbol? (x 1))))
                                     private-fields-and-methods))

             (methods (filter (lambda (x)
                                (and (list? x)
                                     (>= (length x) 2)
                                     (pair? (x 1))))
                              private-fields-and-methods))
         
             (method-names
               (map (lambda (method)
                      (let* ((method-sym (caadr method))
                             (method-name (symbol->string method-sym)))
                        (cond
                          ((string-starts? method-name "@")
                           (string-remove-prefix method-name "@"))
                          ((string-starts? method-name "%")
                           (string-remove-prefix method-name "%"))
                          (else method-name))))
                    methods))
         
             (conflicts-names
              (filter (lambda (method-name)
                        (let ((name (string->symbol method-name)))
                          (member name field-names)))
                      method-names))
         
             (check-conflicts-names (unless (null? conflicts-names)
                                      (let ((conflict-str (apply string-append 
                                                            (map (lambda (c) (string-append " <" c ">"))
                                                                 conflicts-names))))
                                        (error 'syntax-error (string-append "In class ["
                                                               (symbol->string class-name)
                                                               "]: Method name" 
                                                               (if (= (length conflicts-names) 1) "" "s")
                                                               conflict-str
                                                               " conflicts with field name"
                                                               (if (= (length conflicts-names) 1) "" "s"))))))
         
             (instance-methods
              (filter (lambda (method) (string-starts? (symbol->string (caadr method)) "%"))
                      methods))
             (instance-method-symbols (map caadr instance-methods))
             (instance-messages
              (map (lambda (method)
                     (let ((name (string-remove-prefix (symbol->string method) "%")))
                       (string->symbol (string-append ":" name))))
                   instance-method-symbols))
             (static-methods
              (filter (lambda (method) (string-starts? (symbol->string (caadr method)) "@"))
                      methods))
             (static-method-symbols (map caadr static-methods))
             (static-messages
              (map (lambda (method)
                     (let ((name (string-remove-prefix (symbol->string method) "@")))
                       (string->symbol (string-append ":" name))))
                   static-method-symbols))
             ;(default-static-messages '(:is-type-of))
             (internal-methods
               (filter (lambda (method) (not (or (string-starts? (symbol->string (caadr method)) "%")
                                                 (string-starts? (symbol->string (caadr method)) "@"))))
                       methods))
             (f-make-case-class (string->symbol (string-append "make-case-class-" (symbol->string class-name))))
             (object-name (string->symbol (string-append (symbol->string class-name) "-object"))))

        `(begin
           (define-object ,object-name
             (define (@to-string ,@field-names)
               (define (%to-string)
                 (let ((field-strings
                        (list ,@(map (lambda (field key-field)
                                       `(string-append
                                         ,(symbol->string key-field) " "
                                         (object->string ,(car field))))
                                     fields key-fields))))
                   (let loop ((strings field-strings)
                              (acc ""))
                     (if (null? strings)
                         (string-append "(" ,(symbol->string class-name) " " acc ")")
                         (loop (cdr strings)
                               (if (zero? (string-length acc))
                                   (car strings)
                                   (string-append acc " " (car strings))))))))
               %to-string)

             ,@(map (lambda (method)
                      (let* ((method-def (cadr method))
                             (method-name (car method-def))
                             (method-params (cdr method-def))
                             (method-body (cddr method))
                             (external-method-name (string->symbol (string-append "@" (string-remove-prefix (symbol->string method-name) "%")))))
                        `(define (,external-method-name ,@field-names)
                           ,method
                           ,method-name)))
                    instance-methods))

           (define (,class-name . args)

           (define (@is-type-of obj)
             (and (case-class? obj)
                  (obj :is-instance-of ',class-name)))
   
           ,@static-methods

           (define (is-normal-function? msg)
             (and  (symbol? msg) 
                   (char=? (string-ref (symbol->string msg) 0) #\:)))

           (define (static-dispatcher msg . args)
             (cond
              ((eq? msg :is-type-of) (apply @is-type-of args))
              ,@(map (lambda (method expected) `((eq? msg ,expected) (apply ,method args)))
                     static-method-symbols static-messages)
              (else (value-error "No such static method " msg))))

           (define* (,f-make-case-class 
                      ,@(map  
                          (lambda (param)
                            (let  ((param-name (car param))
                                   (type-pred (cadr param))
                                   (default-value (cddr param)))
                                  (if (null? default-value)
                                      param-name
                                      `(,param-name ,(car default-value)))))
                          fields))
             ,@(map (lambda (param)
                      (let* ((param-name (car param))
                             (type-pred (cadr param))
                             ;;remove the '?' in 'type?'
                             (type-name-str 
                               (let ((s (symbol->string type-pred)))
                                 (if (and (positive? (string-length s))
                                       (char=? (string-ref s (- (string-length s) 1)) #\?))
                                     (substring s 0 (- (string-length s) 1))
                                     s))))

                        `(unless 
                           (,type-pred ,param-name)
                           (type-error 
                             (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**"
                               ,f-make-case-class
                               ',field-names
                               ',param-name
                               ,type-name-str
                               (object->string ,param-name))))))
                 fields)

             (define (%is-instance-of x)
               (eq? x ',class-name))
         
             (define (%equals that)
               (unless (case-class? that) 
                 (type-error 
                   (format #f "In funtion #<~a ~a>: argument *~a* must be *~a*!    **Got ~a**" 
                     %equals '(that) 'that "case-class" (object->string that))))
               (and (that :is-instance-of ',class-name)
                    ,@(map (lambda (field) `(equal? ,(car field) (that ',(car field))))
                           fields)))
         
             (define (%apply . args)
               (cond ((null? args)
                      (value-error ,class-name "Apply on zero args is not implemented"))
                     ((equal? ((symbol->string (car args)) 0) #\:)
                      (value-error ,class-name "No such method: " (car args)))
                     (else (value-error ,class-name "No such field: " (car args)))))
         
             ,@private-fields
             ,@internal-methods

             (define (instance-dispatcher)
               (lambda (msg . args)
                 (cond
                   ((eq? msg :is-instance-of) (apply %is-instance-of args))
                   ((eq? msg :equals) (apply %equals args))
                   ((eq? msg :to-string) (apply (,object-name :to-string ,@field-names)))
                   ,@(map (lambda (field key-field)
                            `((eq? msg ,key-field)
                              (,class-name
                                ,@(map (lambda (f) (if (eq? (car f) (car field)) '(car args) (car f)))
                                    fields))))
                       fields key-fields)
                   ((is-normal-function? msg)
                    (case msg
                      ,@(map (lambda (method expected)
                               `((,expected) (apply (,object-name ,expected ,@field-names) args)))
                             instance-method-symbols instance-messages)
                      (else (value-error ,class-name "No such method: " msg))))
                   ,@(map (lambda (field) `((eq? msg ',(car field)) ,(car field))) fields)
                   (else (apply %apply (cons msg args))))))

             (instance-dispatcher)) ; end of the internal typed define

           (if (null? args)
               (,f-make-case-class)
               (let ((msg (car args)))
                 (cond ((member msg (list ,@static-messages :is-type-of))
                        (apply static-dispatcher args))
                       ((and (zero? ,field-count) (member :apply (list ,@static-messages)))
                        (apply static-dispatcher (cons :apply args)))
                       (else
                        (apply ,f-make-case-class args)))))

           ) ; end of define
        ) ; end of let
      ) ; end of define-macro
    )
  )
)
