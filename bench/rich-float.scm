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

(import (scheme time)
        (liii lang))

(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display* msg (number->string (- end start)) " ns\n")))

(define (repeat n proc)
  (when (>= n 0)
        (proc)
        (repeat (- n 1) proc)))

(timing "Baseline of sqrt\t\t\t\t"
  (lambda () (repeat 10000 (lambda () (sqrt 5.0)))))

(timing "Baseline of number->string\t\t\t"
  (lambda () (repeat 10000 (lambda () (number->string 5.0)))))

(timing "Old impl of rich-float%sqrt\t\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :sqrt)))))
(timing "Old impl of rich-float%to-string\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :to-string)))))

(import (liii float))
(timing "New impl of rich-float%sqrt\t\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :apply :sqrt)))))
(timing "New impl of rich-float%to-string\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :apply :to-string)))))

(import (liii lang))
(timing "Old impl of rich-float%sqrt\t\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :sqrt)))))
(timing "Old impl of rich-float%to-string\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :to-string)))))

(import (liii float))
(timing "New impl of rich-float%sqrt\t\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :apply :sqrt)))))
(timing "New impl of rich-float%to-string\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :apply :to-string)))))

(import (liii lang))
(timing "Old impl of rich-float%sqrt\t\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :sqrt)))))
(timing "Old impl of rich-float%to-string\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :to-string)))))

(import (liii float))
(timing "New impl of rich-float%sqrt\t\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :apply :sqrt)))))
(timing "New impl of rich-float%to-string\t\t"
  (lambda () (repeat 10000 (lambda () ((rich-float 5.0) :apply :to-string)))))
