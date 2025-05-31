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
        (liii base64)
        (liii error))

(check-set-mode! 'report-failed)

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

