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
        (liii unicode)
        (liii base))

(check-set-mode! 'report-failed)

;; ============================================================================
;; UTF-8 编码/解码测试
;; ============================================================================

(check (utf8->string (string->utf8 "Hello")) => "Hello")
(check (utf8->string (string->utf8 "你好")) => "你好")
(check (utf8->string (string->utf8 "Hello 你好")) => "Hello 你好")

(check (u8-string-length "Hello") => 5)
(check (u8-string-length "你好") => 2)
(check (u8-string-length "Hello 你好") => 8)

(check (u8-substring "Hello 你好" 0 5) => "Hello")
(check (u8-substring "Hello 你好" 6 8) => "你好")

;; UTF-8 基础解码测试
(check (utf8->string (bytevector #x48 #x65 #x6C #x6C #x6F)) => "Hello")
(check (utf8->string #u8(#xC3 #xA4)) => "ä")
(check (utf8->string #u8(#xE4 #xB8 #xAD)) => "中")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D)) => "👍")

;; UTF-8 错误处理测试
(check-catch 'value-error (utf8->string (bytevector #xFF #x65 #x6C #x6C #x6F)))

;; UTF-8 编码测试
(check (string->utf8 "Hello") => (bytevector #x48 #x65 #x6C #x6C #x6F))
(check (string->utf8 "ä") => #u8(#xC3 #xA4))
(check (string->utf8 "中") => #u8(#xE4 #xB8 #xAD))
(check (string->utf8 "👍") => #u8(#xF0 #x9F #x91 #x8D))
(check (string->utf8 "") => #u8())

;; UTF-8 子字符串编码测试
(check (utf8->string (string->utf8 "Hello" 1 2)) => "e")
(check (utf8->string (string->utf8 "Hello" 0 2)) => "He")
(check (utf8->string (string->utf8 "Hello" 2)) => "llo")
(check (utf8->string (string->utf8 "Hello" 2 5)) => "llo")

;; 中文字符串编码测试
(check (utf8->string (string->utf8 "汉字书写")) => "汉字书写")
(check (utf8->string (string->utf8 "汉字书写" 1)) => "字书写")
(check (utf8->string (string->utf8 "汉字书写" 2)) => "书写")
(check (utf8->string (string->utf8 "汉字书写" 3)) => "写")

;; UTF-8 边界错误处理测试
(check-catch 'out-of-range (string->utf8 "Hello" 2 6))
(check-catch 'out-of-range (string->utf8 "汉字书写" 4))

;; UTF-8 子字符串操作测试
(check (u8-substring "汉字书写" 0 1) => "汉")
(check (u8-substring "汉字书写" 0 4) => "汉字书写")
(check (u8-substring "汉字书写" 0) => "汉字书写")

;; ============================================================================
;; Unicode 常量测试
;; ============================================================================

(check unicode-max-codepoint => #x10FFFF)
(check unicode-replacement-char => #xFFFD)

;; ============================================================================
;; 测试报告
;; ============================================================================

(check-report)