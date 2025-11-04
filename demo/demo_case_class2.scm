(import (liii oop) (liii check))

(define-object person-object
  (define (@to-string name age)
    (define (%to-string)
      (string-append "I am " name ", " (number->string age) " years old!"))
    %to-string)

  (define (@greet name age)
    (define (%greet other-name)
      (string-append "Hi " other-name ", " ((person-object :to-string name age))))
      ; (string-append "Hi " other-name ", " (%to-string)))
    %greet))

(define (person name age)
  (lambda (message . args)
    (case message
      ((:to-string) (apply (person-object :to-string name age) args))
      ((:greet) (apply (person-object :greet name age) args)))))

(define bob (person "bob" 21))

(display* (bob :to-string))
(newline)

(display* (bob :greet "Alice"))
(newline)
