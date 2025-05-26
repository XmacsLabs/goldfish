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

(import (liii check) (liii range))

(check-set-mode! 'report-failed)

(let1 r (range :inclusive 1 2)
  (check (r 'start) => 1)
  (check (r 'end) => 2)
  (check (r 'step) => 1)
  (check-true (r 'inclusive?)))

(let1 r (range :inclusive 1 3 2)
  (check (r 'start) => 1)
  (check (r 'end) => 3)
  (check (r 'step) => 2)
  (check-true (r 'inclusive?)))

(let1 r1 (range :inclusive -2 1)
  (let1 map-func (lambda (x) (* x x))
    (let1 r2 (r1 :map map-func)
      (check r2 => (rich-list (list 4 1 0 1))))))

(check-false ((range :inclusive 1 3) :empty?))
(check-true ((range :inclusive 3 1) :empty?))
(check-false ((range :inclusive 1 3 0) :empty?))

