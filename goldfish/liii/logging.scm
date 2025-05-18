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

(define-library (liii logging)
(import (liii path))
(export logging)
(begin

(define-constant NOTSET 0)
(define-constant DEBUG 10)
(define-constant INFO 20)
(define-constant WARNING 30)
(define-constant ERROR 40)
(define-constant CRITICAL 50)

(define-class logging
  ((name string? "default")
   (path string? "")
   (level integer? WARNING))
  
(define (@apply p-name)
  (let ((r (logging)))
    (r :set-name! p-name)
    r))

(define (print line0)
  (let ((line (string-append line "\n")))
    (if (string-null? path)
        (display line)
        (path-append-text path line))))

(define (%debug?)
  (<= level DEBUG))

(define (%info?)
  (<= level INFO))

)

) ; end of begin
) ; end of define-library

