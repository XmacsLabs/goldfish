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

(import (liii list)
        (liii string)
        (liii os)
        (liii path)
        (liii lang))

(define level1-tests
  ((path :./ "tests" :/ "goldfish" :list-path)
   :filter (@ _ :dir?)
   :flat-map (lambda (x) ((x :list-path) :collect))
   :filter (@ _ :file?)
   :filter (lambda (x) (not ($ (x :to-string) :contains "srfi-78")))
   :map (@ _ :to-string)))

(define level2-tests
  ((path :./ "tests" :/ "goldfish" :list-path)
   :filter (@ _ :dir?)
   :flat-map (lambda (x) ((x :list-path) :collect))
   :filter (@ _ :dir?)
   :flat-map (lambda (x) ((x :list-path) :collect))
   :filter (@ _ :file?)
   :filter (lambda (x) (not ($ (x :to-string) :contains "srfi-78")))
   :map (@ _ :to-string)))

(display* (level1-tests :length))

(define (all-tests)
  (rich-vector :concat level1-tests level2-tests))

(define (goldfish-cmd)
  (if (os-windows?)
    "bin\\goldfish -m r7rs "
    "bin/goldfish -m r7rs "))

(let1 ret-l ((all-tests)
             :map (lambda (x) (string-append (goldfish-cmd) x))
             :map (lambda (x)
                    (newline)
                    (display "----------->") (newline)
                    (display x) (newline)
                    (os-call x)))
  (when (ret-l :exists (compose not zero?))
    (exit -1)))

