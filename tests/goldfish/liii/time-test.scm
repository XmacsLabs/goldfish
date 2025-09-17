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

(let ((t1 (current-second)))
  (sleep 1)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 1) => #t)))

(let ((t1 (current-second)))
  (sleep 0.5)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 0) => #t)))

(check-catch 'type-error (sleep 'not-a-number))

(check-report)