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

(import (liii check)
        (liii time)
        (scheme time)
        (liii base))

(check-set-mode! 'report-failed)

; Test sleep function
(let ((t1 (current-second)))
  (sleep 1)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 1) => #t)))

(let ((t1 (current-second)))
  (sleep 0.5)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 0) => #t)))

(check-catch 'type-error (sleep 'not-a-number))

; Test current-second function
(let ((t1 (current-second)))
  (check (number? t1) => #t)
  (check (>= t1 0) => #t))

; Test current-jiffy function
(let ((j1 (current-jiffy)))
  (check (integer? j1) => #t)
  (check (>= j1 0) => #t))

; Test jiffies-per-second function
(check (jiffies-per-second) => 1000000)
(check (integer? (jiffies-per-second)) => #t)
(check (positive? (jiffies-per-second)) => #t)

; Test consistency between current-second and current-jiffy
(let* ((t1 (current-second))
       (j1 (current-jiffy))
       (t2 (current-second))
       (j2 (current-jiffy)))
  (check (>= t2 t1) => #t)
  (check (>= j2 j1) => #t))

; Test that functions are available from (liii time) module
(check-true (procedure? sleep))
(check-true (procedure? current-second))
(check-true (procedure? current-jiffy))
(check-true (procedure? jiffies-per-second))

(check-report)