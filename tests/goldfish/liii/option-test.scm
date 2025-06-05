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

(import (liii option)
        (liii check))

;; Test basic option creation and get functionality
(let ((opt1 (option 42)) (opt2 (none)))
  (check (opt1 :apply :get) => 42)
  (check-catch 'value-error (opt2 :apply :get)))

;; Test get-or-else functionality
(let ((opt1 (option 42)) (opt2 (none)))
  (check (opt1 :get-or-else 0) => 42)
  (check (opt2 :get-or-else 0) => 0)

  (check (opt1 :get-or-else (lambda () 0)) => 42)
  (check (opt2 :get-or-else (lambda () 0)) => 0))

;; Test or-else functionality  
(let ((opt1 (option 42)) (opt2 (none)))
  (check-true ((opt1 :or-else (option 0)) :equals opt1))
  (check-true ((opt2 :or-else (option 0)) :equals (option 0)))
  (check-true ((opt2 :or-else (option 0) :apply :or-else (option 1)) :equals (option 0)))
  (check-catch 'type-error (opt1 :or-else 0)))

;; Test equals functionality
(check-true ((option "str") :equals (option "str")))
(check-false ((option 42) :equals (option 24)))
(check-false ((option 42) :equals (none)))
(check-true ((none) :equals (none)))

;; Test defined? functionality
(let ((opt1 (option 42)) (opt2 (none)))
  (check-true (opt1 :apply :defined?))
  (check-false (opt2 :apply :defined?)))

;; Test empty? functionality
(let ((opt1 (option 42)) (opt2 (none)))
  (check-false (opt1 :apply :empty?))
  (check-true (opt2 :apply :empty?)))

;; Test forall functionality
(let ((opt1 (option 42)) (opt2 (none)))
  (check-true (opt1 :forall (lambda (x) (= x 42))))
  (check-false (opt2 :forall (lambda (x) (= x 42)))))

;; Test exists functionality
(let ((opt1 (option 42)) (opt2 (none)))
  (check-true (opt1 :exists (lambda (x) (= x 42))))
  (check-false (opt2 :exists (lambda (x) (= x 42)))))

;; Test contains functionality
(check-true ((option "hello") :contains "hello"))
(check-true ((option 42) :contains 42))
(check-true ((option #t) :contains #t))
(check-false ((none) :contains "hello"))
(check-false ((option "hello") :contains "world"))
(check-false ((option 42) :contains 24))

;; Test map functionality with chaining
(let ((opt1 (option 42))
      (opt2 (none)))
  (check (opt1 :map (lambda (x) (+ x 1))
               :map (lambda (x) (* x 2))
               :apply :get) => 86)
  (check (opt2 :map (lambda (x) (+ x 1))
               :map (lambda (x) (* x 2))
               :apply :empty?) => #t))

;; Test flat-map functionality with chaining
(let ((opt1 (option 42))
      (opt2 (none)))
  (check (opt1 :flat-map (lambda (x) (option (+ x 1)))
               :flat-map (lambda (x) (option (* x 2)))
               :apply :get) => 86)
  (check (opt2 :flat-map (lambda (x) (option (+ x 1)))
               :flat-map (lambda (x) (option (* x 2)))
               :apply :empty?) => #t))

;; Test filter functionality with chaining
(let ((opt1 (option 42))
      (opt2 (none)))
  (check (opt1 :filter (lambda (x) (> x 40))
               :filter (lambda (x) (< x 50))
               :apply :get) => 42)
  (check (opt1 :filter (lambda (x) (> x 50))
               :filter (lambda (x) (< x 60))
               :apply :empty?) => #t)
  (check (opt2 :filter (lambda (x) (> x 40))
               :filter (lambda (x) (< x 50))
               :apply :empty?) => #t))

;; Test for-each functionality (side effects)
(let ((counter 0))
  ((option 42) :for-each (lambda (x) (set! counter (+ counter x))))
  (check counter => 42))

(let ((counter 0))
  ((none) :for-each (lambda (x) (set! counter (+ counter x))))
  (check counter => 0))

;; Test edge cases
(check ((option 0) :apply :defined?) => #t)
(check ((option #f) :apply :defined?) => #t)
(check ((option "") :apply :defined?) => #t)
(check ((option '()) :apply :defined?) => #f)  ; Special case: '() represents none

;; Test nested options
(check ((option (option 42)) :flat-map (lambda (x) x) :apply :get) => 42)

(check-report) 