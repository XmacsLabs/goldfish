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
        (scheme inexact))

(check-set-mode! 'report-failed)
(check (nan? +nan.0) => #t)
(check (nan? +nan.0+5.0i) => #t)
       
(check (nan? 32) => #f)
(check (nan? 3.14) => #f)
(check (nan? 1+2i) => #f)
(check (nan? +inf.0) => #f)
(check (nan? -inf.0) => #f)
       
(check (sqrt 9) => 3)              
(check (sqrt 25.0) => 5.0)
(check (sqrt 9/4) => 3/2) 
(check (< (abs (- (sqrt 2.0) 1.4142135623730951)) 1e-10) => #t)
       
(check (sqrt -1.0) => 0.0+1.0i)
(check (sqrt -1) => 0.0+1.0i)
       
(check (sqrt 0) => 0)
(check (sqrt 0.0) => 0.0)
       
(check (exact? (sqrt 2.0)) => #f) 
(check (exact? (sqrt -1)) => #f) 
(check (exact? (sqrt -1.0)) => #f) 
       
(check-catch 'wrong-type-arg  (sqrt "hello"))
(check-catch 'wrong-type-arg  (sqrt 'symbol))
       
(check-report)
