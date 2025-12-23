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

(import (liii check) (liii hashlib))
(check (md5 "") => "d41d8cd98f00b204e9800998ecf8427e")
(check (md5 "hello") => "5d41402abc4b2a76b9719d911017c592")
(check (md5 "The quick brown fox jumps over the lazy dog") => "9e107d9d372bb6826bd81d3542a419d6")
(check (md5 "a") => "0cc175b9c0f1b6a831c399e269772661")
(check (md5 "123456") => "e10adc3949ba59abbe56e057f20f883e")
(check (md5 "!@#$%^&*()") => "05b28d17a7b6e7024b6e5d8cc43a8bf7")
(check (md5 "Hello") => "8b1a9953c4611296a827abf8c47804d7")