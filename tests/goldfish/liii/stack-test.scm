;
; Copyright (C) 2024 The Goldfish Scheme Authors
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

(import (liii base) (liii lang) (liii stack) (liii check))

(check-set-mode! 'report-failed)

(check ((stack :empty) :length) => 0)
(check ((stack (list 1 2 3)) :length) => 3)
(check ((stack (list 1)) :length) => 1)
(check ((stack (iota 100)) :length) => 100)
(check ((stack (list 1 2 3 4 5 6 7 8 9 10)) :length) => 10)

(check ((stack :empty) :size) => 0)
(check ((stack (list 1)) :size) => 1)
(check ((stack (list 1 2 3)) :size) => 3)
(check ((stack (iota 50)) :size) => 50)
(check ((stack (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) :size) => 15)

(check-catch 'out-of-range ((stack (list )) :top))
(check ((stack (list 1)) :top) => 1)
(check ((stack (list 42)) :top) => 42)
(check ((stack (list 100 200 300 400)) :top) => 100)
(check ((stack (list "string" 123 #t)) :top) => "string")

(check-catch 'out-of-range ((stack :empty) :pop))
(check ((stack (list 42)) :pop) => (stack '()))
(check ((stack (list 1 2)) :pop) => (stack (list 2)))
(check ((stack (list "a" "b" "c" "d")) :pop :pop) => (stack (list "c" "d")))
(check ((stack (iota 10)) :pop :pop :pop) => (stack (list 3 4 5 6 7 8 9)))

(check-catch 'out-of-range ((stack :empty) :pop!))
(check ((stack (list 1)) :pop!) => (stack (list)))
(check ((stack (list 1 2)) :pop!) => (stack (list 2)))
(check ((stack (list "A" "B" "C" "D" "E")) :pop! :pop! :pop!) => (stack (list "D" "E")))
(let1 t (stack (list 100))
  (check-catch 'out-of-range (t :pop! :pop!)))

(let1 t (stack (list 1 2 3))
  (check (t :push 1) => (stack (list 1 1 2 3)))
  (check (t :push 1 :push 1) => (stack (list 1 1 1 2 3))))

(let1 t (stack (list 1 2 3))
  (check (t :push! 1) => (stack (list 1 1 2 3)))
  (check (t :push! 1 :push! 1) => (stack (list 1 1 1 1 2 3)))
  (check (t :pop! :push! 2) => (stack (list 2 1 1 1 2 3))))


(check ((stack (list 1 2 3)) :to-list) => (list 1 2 3))
(check ((stack (list)) :to-list) => (list))

(check ((stack (list 1 2 3)) :to-rich-list) => ($ (list 1 2 3)))

(check-report)

