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

(define-library (liii coroutine)
  (export co-dispatch co-create co-yield co-sleep)
  (begin
    (define (co-dispatch f)
      (g_coroutine-dispatch f))
    (define (co-create f)
      (g_coroutine-create f))
    (define (co-yield)
      (g_coroutine-yield))
    (define (co-sleep seconds)
      (g_coroutine-sleep seconds))))