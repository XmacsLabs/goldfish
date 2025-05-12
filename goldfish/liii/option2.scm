(define-library (liii option2)
(import (liii oop))
(export option2)
(begin

(define (option2 x)
  (lambda (msg . args)
    (let ((env (funclet option2)))
      (let-set! env 'value x)
      (let1 r (case msg
                ((value)
                 value)
                ((:value)
                 (option2 value))
                ((:get) (apply (env '%get) args))
                ((:get-or-else) (apply (env '%get-or-else) args))
                ((:or-else) (apply (env '%or-else) args))
                ((:equals) (apply (env '%equals) args))
                ((:defined?) (apply (env '%defined?) args))
                ((:empty?) (apply (env '%empty?) args))
                ((:forall) (apply (env '%forall) args))
                ((:exists) (apply (env '%exists) args))
                ((:contains) (apply (env '%contains) args))
                ((:for-each) (apply (env '%for-each) args))
                ((:map) (apply (env '%map) args))
                ((:flat-map) (apply (env '%flat-map) args))
                ((:filter) (apply (env '%filter) args)))
        r))))

(varlet (funclet option2) 'value #f)

(with-let (funclet option2)
  (define (%get)
    (if (null? value)
        (value-error "option2 is empty, cannot get value")
        value))
  
  (define (%get-or-else default)
    (cond ((not (null? value)) value)
          ((and (procedure? default) (not (case-class? default)))
           (default))
          (else default)))
  
  (define (%or-else default . args)
    ;(when (not (option2 :is-type-of default))
    ;  (type-error "The first parameter of option2%or-else must be a option2 case class"))
    
    (chain-apply args
      (if (null? value)
          default
          (option2 value))))
  
  (define (%equals that)
    (== value (that 'value)))
  
  (define (%defined?) (not (null? value)))
    
  (define (%empty?)
    (null? value))
  
  (define (%forall f)
    (if (null? value)
        #f
        (f value)))
  
  (define (%exists f)
    (if (null? value)
        #f
        (f value)))
  
  (define (%contains pred?)
    (if (null? value)
        #f
        (pred? value)))
  
  (define (%for-each f)
    (when (not (null? value))
          (f value)))
  
  (chained-define (%map f)
    (if (null? value)
        (option2 '())
        (option2 (f value))))
  
  (define (%flat-map f . args)
    (chain-apply args
      (if (null? value)
          (option2 '())
          (f value))))
  
  (define (%filter pred . args)
    (chain-apply args
      (if (or (null? value) (not (pred value)))
          (option2 '())
          (option2 value))))

  (varlet (funclet option2) '%get %get)
  (varlet (funclet option2) '%get-or-else %get-or-else)
  (varlet (funclet option2) '%or-else %or-else)
  (varlet (funclet option2) '%equals %equals)
  (varlet (funclet option2) '%defined? %defined?)
  (varlet (funclet option2) '%empty? %empty?)
  (varlet (funclet option2) '%forall %forall)
  (varlet (funclet option2) '%exists %exists)
  (varlet (funclet option2) '%contains %contains)
  (varlet (funclet option2) '%for-each %for-each)
  (varlet (funclet option2) '%map  %map)
  (varlet (funclet option2) '%flat-map %flat-map)
  (varlet (funclet option2) '%filter %filter)
)

) ; end of begin
) ; end of define-library