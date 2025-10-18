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

(define-library (liii unicode)
  (export
   ;; UTF-8 函数
   utf8->string string->utf8 u8-string-length u8-substring bytevector-advance-u8

   ;; Unicode 常量
   unicode-max-codepoint unicode-replacement-char)

  (import (liii base))

  (begin
    ;; ============================================================================
    ;; UTF-8 函数重新导出
    ;; ============================================================================

    ;; 这些函数已经在 (liii base) 中定义，直接重新导出

    ;; ============================================================================
    ;; Unicode 常量
    ;; ============================================================================

    (define unicode-max-codepoint #x10FFFF)
    (define unicode-replacement-char #xFFFD)))