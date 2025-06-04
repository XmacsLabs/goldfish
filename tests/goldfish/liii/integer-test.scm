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

(import (liii integer)
        (liii check))

;; Test basic get functionality
(check ((rich-integer 0) :apply :get) => 0)
(check ((rich-integer 42) :apply :get) => 42)
(check ((rich-integer -10) :apply :get) => -10)


;; Test to-string functionality
(check ((rich-integer 42) :apply :to-string) => "42")
(check ((rich-integer 0) :apply :to-string) => "0")
(check ((rich-integer -123) :apply :to-string) => "-123")

;; Test sqrt functionality
(check ((rich-integer 0) :apply :sqrt) => 0)       
(check ((rich-integer 1) :apply :sqrt) => 1)       
(check ((rich-integer 4) :apply :sqrt) => 2)       
(check ((rich-integer 9) :apply :sqrt) => 3)       
(check ((rich-integer 16) :apply :sqrt) => 4)       
(check ((rich-integer 15) :apply :sqrt) => 3)  ; floor(sqrt(15)) = 3
(check-catch 'value-error ((rich-integer -1) :apply :sqrt))

;; Test equals functionality
(check-true ((rich-integer 1) :equals (rich-integer 1)))
(check-false ((rich-integer 1) :equals (rich-integer 2)))
(check-false ((rich-integer -5) :equals (rich-integer 5)))

;; Test to-rich-char functionality
(check ((rich-integer 65) :apply :to-rich-char) => (rich-char 65))
(check ((rich-integer 97) :apply :to-rich-char) => (rich-char 97))

;; Test to functionality
(check ((rich-integer 1) :apply :to 5) => (rich-list '(1 2 3 4 5)))
(check ((rich-integer 3) :apply :to 3) => (rich-list '(3)))
(check ((rich-integer 5) :apply :to 3) => (rich-list '()))

;; Test until functionality  
(check ((rich-integer 1) :apply :until 5) => (rich-list '(1 2 3 4)))
(check ((rich-integer 3) :apply :until 3) => (rich-list '()))
(check ((rich-integer 5) :apply :until 3) => (rich-list '()))

;; Test static methods
(check (rich-integers :max-value) => 9223372036854775807)
(check (rich-integers :min-value) => -9223372036854775808)

;; Test type errors
(check-catch 'type-error ((rich-integer 1) :apply :to "not-integer"))
(check-catch 'type-error ((rich-integer 1) :apply :until 3.14))

(check-report) 