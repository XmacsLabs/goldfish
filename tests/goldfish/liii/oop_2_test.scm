;
; Copyright (C) 2025 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii oop_2)
        (liii base)
        (liii check)
        (liii error))

;; Test basic case class without default values
(define-case-class-2 person ((name string?) (age integer?))

(define (%get-name)
  name)

(define (%get-age)
  age)

(define (%is-adult)
  (>= age 18))

(define (%greet other-name)
  (string-append "Hello " other-name ", I'm " name))

(define (%birthday . args)
  (let ((result (person name (+ age 1))))
    (if (null? args)
        result
        (apply (result :apply) args))))

)

;; Test case class with default values
(define-case-class-2 account ((owner string?) (balance integer? 0) (active boolean? #t))

(define (%get-owner)
  owner)

(define (%get-balance)
  balance)

(define (%is-active)
  active)

(define (%deposit amount . args)
  (let ((result (account owner (+ balance amount) active)))
    (if (null? args)
        result
        (apply (result :apply) args))))

(define (%withdraw amount . args)
  (let ((result (if (>= balance amount)
                    (account owner (- balance amount) active)
                    %this)))
    (if (null? args)
        result
        (apply (result :apply) args))))

(define (%deactivate . args)
  (let ((result (account owner balance #f)))
    (if (null? args)
        result
        (apply (result :apply) args))))

)

;; Test case class with mutable operations using set!
(define-case-class-2 mutable-box ((data any? 0))

(define (%get)
  (%this 'data))

(define (%set! new-data)
  (set! (%this 'data) new-data))

(define (%update! f)
  (set! (%this 'data) (f (%this 'data))))

(define (%reset! . args)
  (set! (%this 'data) 0)
  (if (null? args)
      %this
      (apply (%this :apply) args)))

)

;; Basic object creation and field access tests
(let ((p (person "Alice" 25)))
  (check (p 'name) => "Alice")
  (check (p 'age) => 25))

;; Method calls without parameters (using :apply)
(let ((p (person "Bob" 30)))
  (check (p :apply :get-name) => "Bob")
  (check (p :apply :get-age) => 30)
  (check (p :apply :is-adult) => #t))

(let ((p (person "Charlie" 16)))
  (check (p :apply :is-adult) => #f))

;; Method calls with parameters (direct call)
(let ((p (person "David" 28)))
  (check (p :greet "Emma") => "Hello Emma, I'm David"))

;; Default values test
(let ((acc1 (account "John"))
      (acc2 (account "Jane" 100))
      (acc3 (account "Jack" 200 #f)))
  (check (acc1 :apply :get-balance) => 0)
  (check (acc1 :apply :is-active) => #t)
  (check (acc2 :apply :get-balance) => 100)
  (check (acc2 :apply :is-active) => #t)
  (check (acc3 :apply :get-balance) => 200)
  (check (acc3 :apply :is-active) => #f))

;; Method calls that return new objects
(let ((p (person "Eve" 20)))
  (let ((older-p (p :birthday)))
    (check (older-p :apply :get-age) => 21)
    (check (p :apply :get-age) => 20)))

;; Chain operations
(let ((p (person "Frank" 17)))
  (let ((adult-p (p :birthday :birthday)))
    (check (adult-p :apply :get-age) => 19)
    (check (adult-p :apply :is-adult) => #t)))

;; Account operations
(let ((acc (account "Grace" 100)))
  (let ((new-acc (acc :deposit 50)))
    (check (new-acc :apply :get-balance) => 150))
  
  (let ((withdrawn-acc (acc :withdraw 30)))
    (check (withdrawn-acc :apply :get-balance) => 70))
  
  (let ((same-acc (acc :withdraw 200)))
    (check (same-acc :apply :get-balance) => 100)))

;; Chain operations on account
(let ((acc (account "Henry" 100)))
  (let ((final-acc (acc :deposit 50 :withdraw 30 :deactivate)))
    (check (final-acc :apply :get-balance) => 120)
    (check (final-acc :apply :is-active) => #f)))

;; Type checking tests
(check-catch 'type-error (person 123 25))
(check-catch 'type-error (person "Alice" "young"))
(check-catch 'type-error (account 456 100 #t))
(check-catch 'type-error (account "Bob" "rich" #t))
(check-catch 'type-error (account "Carol" 100 "yes"))

(let ((p (person "Test" 25)))
  (check-catch 'undefined-method (p :apply :nonexistent-method))
  (check (p :another-nonexistent-method) => #<undefined>))

(let ((p (person "Test" 25)))
  (check (p 'nonexistent-field) => #<undefined>))

(let ((box (mutable-box 42)))
  (check (box :apply :get) => 42)
  (check (box 'data) => 42)
  
  (box :set! 100)
  (check (box :apply :get) => 100)
  (check (box 'data) => 100)
  
  (box :update! (lambda (x) (* x 2)))
  (check (box :apply :get) => 200)
  

  (box :apply :reset!)
  (check (box :apply :get) => 0)
  
  (box :set! 50)
  (box :reset! :update! (lambda (x) (+ x 10)))
  (check (box :apply :get) => 10))

(let ((box1 (mutable-box 10))
      (box2 (mutable-box 20)))
  (box1 :set! 100)
  (box2 :set! 200)
  (check (box1 :apply :get) => 100)
  (check (box2 :apply :get) => 200)
  
  (box1 :update! (lambda (x) (* x 3)))
  (check (box1 :apply :get) => 300)
  (check (box2 :apply :get) => 200))

(let ((p (person "Test" 25)))
  (check (p 'nonexistent-field) => #<undefined>))

(check-report) 