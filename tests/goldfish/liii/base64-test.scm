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

(import (liii check) (liii base64) (liii error))

(check-set-mode! 'report-failed)

#|
base64-encode
将字符串编码为 Base64 格式。

语法
----
(base64-encode str)

参数
----
str : string
要编码的字符串。必须是字符串类型，其他类型会引发类型错误。

返回值
-----
string
输入字符串的 Base64 编码结果。遵循 RFC 4648 标准：
- 每3个字节编码为4个字符
- 不足3字节时使用'='填充
- 使用A-Z, a-z, 0-9, '+', '/'字符集

错误
----
type-error
当输入不是字符串类型时引发

特性
----
- 空字符串编码为空字符串
- 编码结果长度总是4的倍数（通过填充实现）
- 严格遵循Base64标准规范
|#

(check (base64-encode "") => "")
(check (base64-encode "a") => "YQ==")
(check (base64-encode "z") => "eg==")
(check (base64-encode "f") => "Zg==")
(check (base64-encode "fo") => "Zm8=")
(check (base64-encode "foo") => "Zm9v")
(check (base64-encode "foob") => "Zm9vYg==")
(check (base64-encode "fooba") => "Zm9vYmE=")
(check (base64-encode "foobar") => "Zm9vYmFy")

(check-catch 'type-error (base64-encode 1))

(check (base64-decode "") => "")

(check (base64-decode "YQ==") => "a")
(check (base64-decode "eg==") => "z")
(check (base64-decode "Zg==") => "f")
(check (base64-decode "Zm8=") => "fo")
(check (base64-decode "Zm9v") => "foo")
(check (base64-decode "Zm9vYg==") => "foob")
(check (base64-decode "Zm9vYmE=") => "fooba")
(check (base64-decode "Zm9vYmFy") => "foobar")

(check-report)

