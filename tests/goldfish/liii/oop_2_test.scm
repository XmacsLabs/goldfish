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
  (chain-apply args (person name (+ age 1))))

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
  (chain-apply args (account owner (+ balance amount) active)))

(define (%withdraw amount . args)
  (chain-apply args (if (>= balance amount)
                        (account owner (- balance amount) active)
                        %this)))

(define (%deactivate . args)
  (chain-apply args (account owner balance #f)))

)

;; Basic object creation and field access tests
(let ((p (person "Alice" 25)))
  (check (p 'name) => "Alice")
  (check (p 'age) => 25))

;; Method calls without parameters (using :apply for zero-parameter methods)
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
  (let ((older-p (p :apply :birthday)))
    (check (older-p :apply :get-age) => 21)
    (check (p :apply :get-age) => 20)))

;; Chain operations
(let ((p (person "Frank" 17)))
  (let ((adult-p (p :apply :birthday :apply :birthday)))
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
  (let ((final-acc (acc :deposit 50 :withdraw 30 :apply :deactivate)))
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

;; Test :to-string method
(let ((p (person "Alice" 25)))
  (check (p :apply :to-string) => "(person :name \"Alice\" :age 25)"))

(let ((acc (account "Bob" 100 #t)))
  (check (acc :apply :to-string) => "(account :owner \"Bob\" :balance 100 :active #t)"))

;; Test predicate functions
(let ((p (person "Charlie" 30))
      (acc (account "David" 50))
      (not-case-class 42))
  ;; Test person?
  (check (person? p) => #t)
  (check (person? acc) => #f)
  (check (person? not-case-class) => #f)
  
  ;; Test account?
  (check (account? acc) => #t)
  (check (account? p) => #f)
  (check (account? not-case-class) => #f))

;; Test :equals method
(let ((p1 (person "Eve" 28))
      (p2 (person "Eve" 28))
      (p3 (person "Frank" 28))
      (p4 (person "Eve" 30)))
  (check (p1 :equals p2) => #t)
  (check (p1 :equals p3) => #f)
  (check (p1 :equals p4) => #f))

(let ((acc1 (account "Grace"))
      (acc2 (account "Grace" 0 #t))
      (acc3 (account "Henry" 0 #t)))
  (check (acc1 :equals acc2) => #t)
  (check (acc1 :equals acc3) => #f))

;; Test equals with different types
(let ((p (person "Test" 25))
      (acc (account "Test" 25)))
  (check (p :equals acc) => #f))

;; Test type metadata
(let ((p (person "Alice" 25))
      (acc (account "Bob" 100)))
  ;; Test '*type* metadata
  (check (p '*type*) => 'case-class)
  (check (acc '*type*) => 'case-class)
  
  ;; Test '*class-name* metadata
  (check (p '*class-name*) => 'person)
  (check (acc '*class-name*) => 'account))

;; Test case-class? function
(let ((p (person "Test" 30))
      (acc (account "Test" 100))
      (not-case-class 42)
      (string-obj "hello")
      (list-obj '(1 2 3)))
  (check (case-class? p) => #t)
  (check (case-class? acc) => #t)
  (check (case-class? not-case-class) => #f)
  (check (case-class? string-obj) => #f)
  (check (case-class? list-obj) => #f))

;; Test enhanced object->string function
(let ((p (person "Charlie" 35))
      (acc (account "David" 200 #f))
      (regular-num 123)
      (regular-str "test"))
  (check (object->string p) => "(person :name \"Charlie\" :age 35)")
  (check (object->string acc) => "(account :owner \"David\" :balance 200 :active #f)")
  (check (object->string regular-num) => "123")
  (check (object->string regular-str) => "\"test\""))

(check-report) 