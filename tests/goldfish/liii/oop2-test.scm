(import (liii oop2) (liii check) (liii case) (liii rich-string) (liii oop) (liii base) (liii error))

;; 测试转换函数
(let* ((object-name 'person-object)
       (field-names '(name age))
       (methods '((define (%to-string)
                    (string-append "I am " name ", " (number->string age) " years old!"))
                  (define (%greet other-name)
                    (string-append "Hi " other-name ", " (%to-string)))))
       (transformed (transform-instance-methods methods object-name field-names)))

  ;; 检查转换后的方法定义
  (check (length transformed) => 2)

  ;; 检查第一个方法 (%to-string)
  (let ((to-string-method (car transformed)))
    (check (car to-string-method) => 'define)
    (check (cadr to-string-method) => '(%to-string))
    (check (caddr to-string-method) => '(string-append "I am " name ", " (number->string age) " years old!")))

  ;; 检查第二个方法 (%greet) - 应该将 (%to-string) 转换为 ((person-object :to-string name age))
  (let ((greet-method (cadr transformed)))
    (check (car greet-method) => 'define)
    (check (cadr greet-method) => '(%greet other-name))
    (check (caddr greet-method) => '(string-append "Hi " other-name ", " ((person-object :to-string name age))))))


(define-case-class2 person
  ((name string? "Bob")
   (age integer?)))

(let1 bob (person :name "Bob" :age 21)
  (check (bob 'name) => "Bob")
  (check (bob 'age) => 21)
  (check ((bob :name "hello") 'name) => "hello")
  (check-catch 'value-error (bob 'sex))
  (check-catch 'value-error (bob :sex))
  (check-true (bob :is-instance-of 'person))
  (check-true (person :is-type-of bob))
  (check (bob :to-string) => "(person :name \"Bob\" :age 21)"))

(check-catch 'type-error (person 1 21))

(let ((bob (person "Bob" 21))
      (get-name (lambda (x)
                 (case* x
                   ((#<procedure?>) (x 'name))
                   (else (value-error))))))
  (check (get-name bob) => "Bob")
  (check-catch 'value-error (get-name 1)))

(define-case-class2 jerson
  ((name string?)
   (age integer?))
  
  (define (%to-string)
    (string-append "I am " name " " (number->string age) " years old!"))
  (define (%greet x)
    (string-append "Hi " x ", " (%to-string)))
  (define (%i-greet x)
    (string-append name ": " (%greet x))) 
)

(check-true (procedure? (jerson-object :to-string "name" 21)))

(let1 bob (jerson "Bob" 21)
  (check (bob :to-string) => "I am Bob 21 years old!")
  (check (bob :greet "Alice") => "Hi Alice, I am Bob 21 years old!")
  (check (bob :i-greet "Alice") => "Bob: Hi Alice, I am Bob 21 years old!"))



(define-case-class2 test-case-class
  ((name string?))
  
  (define (@this-is-a-static-method)
    (test-case-class "static"))
  
  (define (%this-is-a-instance-method)
    (test-case-class (string-append name "instance")))
  )

(let1 hello (test-case-class "hello ")
  (check-catch 'value-error (hello :this-is-a-static-method))
  (check (test-case-class :this-is-a-static-method) => (test-case-class "static")))

(check-catch 'syntax-error
  (eval
    '(define-case-class2 instance-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (%name)
        name))))

(check-catch 'syntax-error
  (eval
    '(define-case-class2 static-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (@name)
        name))))

(check-catch 'syntax-error
  (eval
    '(define-case-class2 internal-methods-conflict-test
       ((name string?)
        (test-name string?)
        (age integer?))
       (define (test-name str)
         (string-append str " ")))))

;; 测试自动生成的 %equals 方法
(let ()
  (define-case-class2 point
    ((x integer?)
     (y integer?)))

  (define p1 (point :x 1 :y 2))
  (define p2 (point :x 1 :y 2))
  (define p3 (point :x 3 :y 4))

  ;; 测试相同值的实例相等
  (check-true (p1 :equals p2))
  (check-true (p2 :equals p1))

  ;; 测试不同值的实例不相等
  (check-false (p1 :equals p3))
  (check-false (p3 :equals p1))

  ;; 测试实例与自身相等
  (check-true (p1 :equals p1))
  (check-true (p2 :equals p2))
  (check-true (p3 :equals p3)))

;; 测试 %equals 方法的类型检查
(let ()
  (define-case-class2 person
    ((name string?)
     (age integer?)))

  (define bob (person "Bob" 21))

  ;; 测试与非样本类对象比较抛出 type-error
  (check-catch 'type-error (bob :equals "not-a-sample-class"))
  (check-catch 'type-error (bob :equals 123))
  (check-catch 'type-error (bob :equals +)))

;; 测试不同类型样本类实例的比较
(let ()
  (define-case-class2 person
    ((name string?)
     (age integer?)))

  (define-case-class2 point
    ((x integer?)
     (y integer?)))

  (define bob (person "Bob" 21))
  (define p1 (point :x 1 :y 2))

  ;; 测试不同类型样本类实例不相等
  (check-false (bob :equals p1))
  (check-false (p1 :equals bob)))

;; 测试 %equals 方法在复杂样本类中的行为
(let ()
  (define-case-class2 complex-class
    ((name string?)
     (numbers list?)
     (flag boolean? #f)))

  (define c1 (complex-class :name "test" :numbers '(1 2 3) :flag #t))
  (define c2 (complex-class :name "test" :numbers '(1 2 3) :flag #t))
  (define c3 (complex-class :name "test" :numbers '(4 5 6) :flag #t))

  ;; 测试复杂字段的相等性比较
  (check-true (c1 :equals c2))
  (check-false (c1 :equals c3)))

;; 测试 %equals 方法在带有默认值的样本类中的行为
(let ()
  (define-case-class2 person-with-default
    ((name string? "Unknown")
     (age integer? 0)))

  (define p1 (person-with-default))
  (define p2 (person-with-default :name "Unknown" :age 0))
  (define p3 (person-with-default :name "Alice" :age 25))

  ;; 测试默认值实例的相等性
  (check-true (p1 :equals p2))
  (check-false (p1 :equals p3)))

;; 测试 %equals 方法在带有私有字段的样本类中的行为
(let ()
  (define-case-class2 person-with-private
    ((name string?)
     (age integer?))

    (define secret "private"))

  (define p1 (person-with-private "Bob" 21))
  (define p2 (person-with-private "Bob" 21))

  ;; 测试私有字段不影响相等性比较
  (check-true (p1 :equals p2)))

(check-report)
