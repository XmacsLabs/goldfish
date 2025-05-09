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

(import (liii check)
        (liii lang)
        (liii hashset))

(check-set-mode! 'report-failed)

(let ((hs (hashset :empty)))
  (check-false (hs :contains 1))
  (check-false (hs :contains #t))
  (check-true (hs :add! 1
                  :add! 2
                  :contains 1))
  (check-false (hs :remove! 2
                   :contains 2)))

(let ((hs1 (hashset :empty :add! 2 :add! 3))
      (hs2 (hashset :empty :add! 1 :add! 2)))
  (define hs (hs1 :concat hs2))
  (check-true (hs :contains 1))
  (check-true (hs :contains 2))
  (check-true (hs :contains 3)))

(check-report)
