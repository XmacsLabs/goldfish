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

(import (liii list))
(import (scheme time))
(import (liii rich-char))

(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display* msg (number->string (- end start)) "\n")))

(define (repeat n proc)
  (when (>= n 0)
        (proc)
        (repeat (- n 1) proc)))

(define (ascii? x)
  (let ((code-point (char->integer x)))
    (and (>= code-point 0) (<= code-point 127))))

(timing "ascii?\t\t\t"
  (lambda () (repeat 10000 (lambda () (ascii? #\A)))))

(timing "rich-char%ascii?\t"
  (lambda () (repeat 10000 (lambda () ((rich-char #\A) :ascii?)))))
