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
   codepoint->utf8

   ;; Unicode 常量
   unicode-max-codepoint unicode-replacement-char)

  (import (liii base) (liii bitwise) (liii error))

  (begin
    ;; ============================================================================
    ;; UTF-8 函数重新导出
    ;; ============================================================================

    ;; 这些函数已经在 (liii base) 中定义，直接重新导出

    ;; ============================================================================
    ;; Unicode 码点与 UTF-8 转换函数
    ;; ============================================================================

    (define (codepoint->utf8 codepoint)
      (unless (integer? codepoint)
        (error 'type-error "codepoint->utf8: expected integer, got" codepoint))

      (when (or (< codepoint 0) (> codepoint #x10FFFF))
        (error 'value-error "codepoint->utf8: codepoint out of Unicode range" codepoint))

      (cond
        ((<= codepoint #x7F)
         (bytevector codepoint))

        ((<= codepoint #x7FF)
         (let ((byte1 (bitwise-ior #b11000000 (bitwise-and (arithmetic-shift codepoint -6) #b00011111)))
               (byte2 (bitwise-ior #b10000000 (bitwise-and codepoint #b00111111))))
           (bytevector byte1 byte2)))

        ((<= codepoint #xFFFF)
         (let ((byte1 (bitwise-ior #b11100000 (bitwise-and (arithmetic-shift codepoint -12) #b00001111)))
               (byte2 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift codepoint -6) #b00111111)))
               (byte3 (bitwise-ior #b10000000 (bitwise-and codepoint #b00111111))))
           (bytevector byte1 byte2 byte3)))

        (else
         (let ((byte1 (bitwise-ior #b11110000 (bitwise-and (arithmetic-shift codepoint -18) #b00000111)))
               (byte2 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift codepoint -12) #b00111111)))
               (byte3 (bitwise-ior #b10000000 (bitwise-and (arithmetic-shift codepoint -6) #b00111111)))
               (byte4 (bitwise-ior #b10000000 (bitwise-and codepoint #b00111111))))
           (bytevector byte1 byte2 byte3 byte4)))))

    ;; ============================================================================
    ;; Unicode 常量
    ;; ============================================================================

    (define unicode-max-codepoint #x10FFFF)
    (define unicode-replacement-char #xFFFD)))