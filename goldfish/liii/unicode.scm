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
   codepoint->utf8 utf8->codepoint

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

    (define (utf8->codepoint bytevector)
      (unless (bytevector? bytevector)
        (error 'type-error "utf8->codepoint: expected bytevector, got" bytevector))

      (let ((len (bytevector-length bytevector)))
        (when (= len 0)
          (error 'value-error "utf8->codepoint: empty bytevector"))

        (let ((first-byte (bytevector-u8-ref bytevector 0)))
          (cond
            ;; 1字节编码: 0xxxxxxx
            ((<= first-byte #x7F)
             first-byte)

            ;; 2字节编码: 110xxxxx 10xxxxxx
            ((<= #xC2 first-byte #xDF)
             (when (< len 2)
               (error 'value-error "utf8->codepoint: incomplete 2-byte sequence"))
             (let ((byte2 (bytevector-u8-ref bytevector 1)))
               (unless (<= #x80 byte2 #xBF)
                 (error 'value-error "utf8->codepoint: invalid continuation byte"))
               (bitwise-ior
                 (arithmetic-shift (bitwise-and first-byte #b00011111) 6)
                 (bitwise-and byte2 #b00111111))))

            ;; 3字节编码: 1110xxxx 10xxxxxx 10xxxxxx
            ((<= #xE0 first-byte #xEF)
             (when (< len 3)
               (error 'value-error "utf8->codepoint: incomplete 3-byte sequence"))
             (let ((byte2 (bytevector-u8-ref bytevector 1))
                   (byte3 (bytevector-u8-ref bytevector 2)))
               (unless (and (<= #x80 byte2 #xBF) (<= #x80 byte3 #xBF))
                 (error 'value-error "utf8->codepoint: invalid continuation byte"))
               (let ((codepoint (bitwise-ior
                                 (arithmetic-shift (bitwise-and first-byte #b00001111) 12)
                                 (arithmetic-shift (bitwise-and byte2 #b00111111) 6)
                                 (bitwise-and byte3 #b00111111))))
                 ;; 检查码点有效性 (避免代理对和过编码)
                 (when (or (<= #xD800 codepoint #xDFFF)  ; 代理对范围
                           (and (= first-byte #xE0) (< codepoint #x0800))  ; 过编码检查
                           (and (= first-byte #xED) (>= codepoint #xD800)))  ; 代理对检查
                   (error 'value-error "utf8->codepoint: invalid codepoint"))
                 codepoint)))

            ;; 4字节编码: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
            ((<= #xF0 first-byte #xF4)
             (when (< len 4)
               (error 'value-error "utf8->codepoint: incomplete 4-byte sequence"))
             (let ((byte2 (bytevector-u8-ref bytevector 1))
                   (byte3 (bytevector-u8-ref bytevector 2))
                   (byte4 (bytevector-u8-ref bytevector 3)))
               (unless (and (<= #x80 byte2 #xBF) (<= #x80 byte3 #xBF) (<= #x80 byte4 #xBF))
                 (error 'value-error "utf8->codepoint: invalid continuation byte"))
               (let ((codepoint (bitwise-ior
                                 (arithmetic-shift (bitwise-and first-byte #b00000111) 18)
                                 (arithmetic-shift (bitwise-and byte2 #b00111111) 12)
                                 (arithmetic-shift (bitwise-and byte3 #b00111111) 6)
                                 (bitwise-and byte4 #b00111111))))
                 ;; 检查码点有效性
                 (when (or (< codepoint #x10000)  ; 4字节编码的最小码点
                           (> codepoint #x10FFFF)  ; Unicode 最大码点
                           (and (= first-byte #xF0) (< codepoint #x10000))  ; 过编码检查
                           (and (= first-byte #xF4) (> codepoint #x10FFFF)))  ; 超出范围检查
                   (error 'value-error "utf8->codepoint: invalid codepoint"))
                 codepoint)))

            ;; 无效的起始字节
            (else
             (error 'value-error "utf8->codepoint: invalid UTF-8 sequence"))))))

    ;; ============================================================================
    ;; Unicode 常量
    ;; ============================================================================

    (define unicode-max-codepoint #x10FFFF)
    (define unicode-replacement-char #xFFFD)))