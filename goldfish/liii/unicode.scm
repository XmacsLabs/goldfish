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

   ;; UTF-16BE 函数
   codepoint->utf16be utf16be->codepoint

   ;; UTF-16LE 函数
   codepoint->utf16le utf16le->codepoint

   ;; 十六进制字符串与码点转换函数
   hexstr->codepoint codepoint->hexstr

   ;; Unicode 常量
   unicode-max-codepoint unicode-replacement-char)

  (import (liii base) (liii bitwise) (liii error))

  (begin
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
            ((<= first-byte #x7F)
             first-byte)

            ((<= #xC2 first-byte #xDF)
             (when (< len 2)
               (error 'value-error "utf8->codepoint: incomplete 2-byte sequence"))
             (let ((byte2 (bytevector-u8-ref bytevector 1)))
               (unless (<= #x80 byte2 #xBF)
                 (error 'value-error "utf8->codepoint: invalid continuation byte"))
               (bitwise-ior
                 (arithmetic-shift (bitwise-and first-byte #b00011111) 6)
                 (bitwise-and byte2 #b00111111))))

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
                 (when (or (<= #xD800 codepoint #xDFFF)
                           (and (= first-byte #xE0) (< codepoint #x0800))
                           (and (= first-byte #xED) (>= codepoint #xD800)))
                   (error 'value-error "utf8->codepoint: invalid codepoint"))
                 codepoint)))

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
                 (when (or (< codepoint #x10000)
                           (> codepoint #x10FFFF)
                           (and (= first-byte #xF0) (< codepoint #x10000))
                           (and (= first-byte #xF4) (> codepoint #x10FFFF)))
                   (error 'value-error "utf8->codepoint: invalid codepoint"))
                 codepoint)))

            (else
             (error 'value-error "utf8->codepoint: invalid UTF-8 sequence"))))))

    (define unicode-max-codepoint #x10FFFF)
    (define unicode-replacement-char #xFFFD)

    (define (hexstr->codepoint hex-string)
      (unless (string? hex-string)
        (error 'type-error "hexstr->codepoint: expected string, got" hex-string))

      (when (string=? hex-string "")
        (error 'value-error "hexstr->codepoint: empty string"))

      ;; 验证十六进制字符
      (let loop ((chars (string->list hex-string)))
        (unless (null? chars)
          (let ((c (car chars)))
            (unless (or (char-numeric? c)
                        (char<=? #\A c #\F)
                        (char<=? #\a c #\f))
              (error 'value-error "hexstr->codepoint: invalid hexadecimal string" hex-string))
            (loop (cdr chars)))))

      (let ((codepoint (string->number hex-string 16)))
        (unless codepoint
          (error 'value-error "hexstr->codepoint: invalid hexadecimal format" hex-string))

        (when (or (< codepoint 0) (> codepoint unicode-max-codepoint))
          (error 'value-error "hexstr->codepoint: codepoint out of Unicode range" codepoint))

        codepoint))

    (define (codepoint->hexstr codepoint)
      (unless (integer? codepoint)
        (error 'type-error "codepoint->hexstr: expected integer, got" codepoint))

      (when (or (< codepoint 0) (> codepoint unicode-max-codepoint))
        (error 'value-error "codepoint->hexstr: codepoint out of Unicode range" codepoint))

      (let ((hex-str (string-upcase (number->string codepoint 16))))
        (if (and (> codepoint 0) (< codepoint 16) (= (string-length hex-str) 1))
            (string-append "0" hex-str)
            hex-str)))

    (define (codepoint->utf16be codepoint)
      (unless (integer? codepoint)
        (error 'type-error "codepoint->utf16be: expected integer, got" codepoint))

      (when (or (< codepoint 0) (> codepoint #x10FFFF))
        (error 'value-error "codepoint->utf16be: codepoint out of Unicode range" codepoint))

      ;; 检查是否为代理对码点（无效）
      (when (<= #xD800 codepoint #xDFFF)
        (error 'value-error "codepoint->utf16be: codepoint in surrogate pair range" codepoint))

      (cond
        ((<= codepoint #xFFFF)
         ;; 基本多文种平面字符 - 单个码元
         (let ((high-byte (arithmetic-shift codepoint -8))
               (low-byte (bitwise-and codepoint #xFF)))
           (bytevector high-byte low-byte)))

        (else
         ;; 辅助平面字符 - 代理对
         (let* ((codepoint-prime (- codepoint #x10000))
                (high-surrogate (+ #xD800 (arithmetic-shift codepoint-prime -10)))
                (low-surrogate (+ #xDC00 (bitwise-and codepoint-prime #x3FF)))
                (high-surrogate-high (arithmetic-shift high-surrogate -8))
                (high-surrogate-low (bitwise-and high-surrogate #xFF))
                (low-surrogate-high (arithmetic-shift low-surrogate -8))
                (low-surrogate-low (bitwise-and low-surrogate #xFF)))
           (bytevector high-surrogate-high high-surrogate-low
                       low-surrogate-high low-surrogate-low)))))

    (define (utf16be->codepoint bytevector)
      (unless (bytevector? bytevector)
        (error 'type-error "utf16be->codepoint: expected bytevector, got" bytevector))

      (let ((len (bytevector-length bytevector)))
        (when (= len 0)
          (error 'value-error "utf16be->codepoint: empty bytevector"))

        (when (< len 2)
          (error 'value-error "utf16be->codepoint: incomplete UTF-16BE sequence"))

        (let* ((first-high (bytevector-u8-ref bytevector 0))
               (first-low (bytevector-u8-ref bytevector 1))
               (first-codepoint (+ (arithmetic-shift first-high 8) first-low)))

          (cond
            ((<= #xD800 first-codepoint #xDBFF)
             ;; 高代理对 - 需要低代理对
             (when (< len 4)
               (error 'value-error "utf16be->codepoint: incomplete surrogate pair"))

             (let* ((second-high (bytevector-u8-ref bytevector 2))
                    (second-low (bytevector-u8-ref bytevector 3))
                    (second-codepoint (+ (arithmetic-shift second-high 8) second-low)))

               (unless (<= #xDC00 second-codepoint #xDFFF)
                 (error 'value-error "utf16be->codepoint: invalid low surrogate"))

               (let ((codepoint-prime (+ (arithmetic-shift (- first-codepoint #xD800) 10)
                                         (- second-codepoint #xDC00))))
                 (+ codepoint-prime #x10000))))

            ((<= #xDC00 first-codepoint #xDFFF)
             ;; 低代理对作为第一个码元 - 无效
             (error 'value-error "utf16be->codepoint: invalid high surrogate"))

            (else
             ;; 基本多文种平面字符 - 单个码元
             first-codepoint))))))

    (define (codepoint->utf16le codepoint)
      (unless (integer? codepoint)
        (error 'type-error "codepoint->utf16le: expected integer, got" codepoint))

      (when (or (< codepoint 0) (> codepoint #x10FFFF))
        (error 'value-error "codepoint->utf16le: codepoint out of Unicode range" codepoint))

      ;; 检查是否为代理对码点（无效）
      (when (<= #xD800 codepoint #xDFFF)
        (error 'value-error "codepoint->utf16le: codepoint in surrogate pair range" codepoint))

      (cond
        ((<= codepoint #xFFFF)
         ;; 基本多文种平面字符 - 单个码元
         (let ((low-byte (bitwise-and codepoint #xFF))
               (high-byte (arithmetic-shift codepoint -8)))
           (bytevector low-byte high-byte)))

        (else
         ;; 辅助平面字符 - 代理对
         (let* ((codepoint-prime (- codepoint #x10000))
                (high-surrogate (+ #xD800 (arithmetic-shift codepoint-prime -10)))
                (low-surrogate (+ #xDC00 (bitwise-and codepoint-prime #x3FF)))
                (high-surrogate-low (bitwise-and high-surrogate #xFF))
                (high-surrogate-high (arithmetic-shift high-surrogate -8))
                (low-surrogate-low (bitwise-and low-surrogate #xFF))
                (low-surrogate-high (arithmetic-shift low-surrogate -8)))
           (bytevector high-surrogate-low high-surrogate-high
                       low-surrogate-low low-surrogate-high)))))

    (define (utf16le->codepoint bytevector)
      (unless (bytevector? bytevector)
        (error 'type-error "utf16le->codepoint: expected bytevector, got" bytevector))

      (let ((len (bytevector-length bytevector)))
        (when (= len 0)
          (error 'value-error "utf16le->codepoint: empty bytevector"))

        (when (< len 2)
          (error 'value-error "utf16le->codepoint: incomplete UTF-16LE sequence"))

        (let* ((first-low (bytevector-u8-ref bytevector 0))
               (first-high (bytevector-u8-ref bytevector 1))
               (first-codepoint (+ (arithmetic-shift first-high 8) first-low)))

          (cond
            ((<= #xD800 first-codepoint #xDBFF)
             ;; 高代理对 - 需要低代理对
             (when (< len 4)
               (error 'value-error "utf16le->codepoint: incomplete surrogate pair"))

             (let* ((second-low (bytevector-u8-ref bytevector 2))
                    (second-high (bytevector-u8-ref bytevector 3))
                    (second-codepoint (+ (arithmetic-shift second-high 8) second-low)))

               (unless (<= #xDC00 second-codepoint #xDFFF)
                 (error 'value-error "utf16le->codepoint: invalid low surrogate"))

               (let ((codepoint-prime (+ (arithmetic-shift (- first-codepoint #xD800) 10)
                                         (- second-codepoint #xDC00))))
                 (+ codepoint-prime #x10000))))

            ((<= #xDC00 first-codepoint #xDFFF)
             ;; 低代理对作为第一个码元 - 无效
             (error 'value-error "utf16le->codepoint: invalid high surrogate"))

            (else
             ;; 基本多文种平面字符 - 单个码元
             first-codepoint))))))