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

(import (liii float)
        (liii check))

(check ((rich-float 0.0) :apply :abs) => 0.0)

(check ((rich-float 12.2) :apply :get) => 12.2)

(check ((rich-float 1.1) :apply :abs) => 1.1)
(check ((rich-float 0.0) :apply :abs) => 0.0)
(check ((rich-float -1.1) :apply :abs) => 1.1)
(check ((rich-float -inf.0) :apply :abs) => +inf.0)

(check ((rich-float 1.1) :apply :to-string) => "1.1")
(check ((rich-float 0.0) :apply :to-string) => "0.0")
(check ((rich-float -1.2) :apply :to-string) => "-1.2")
(check ((rich-float 1.0 ) :apply :to-string) => "1.0")

(check ((rich-float 0.0) :apply :sqrt) => 0.0)       
(check ((rich-float 1.0) :apply :sqrt) => 1.0)       
(check ((rich-float 1.44) :apply :sqrt) => 1.2)       
(check ((rich-float 1.69) :apply :sqrt) => 1.3)       
(check-catch 'value-error ((rich-float -1.5) :apply :sqrt))

(check-true ((rich-float 1.0) :equals (rich-float 1.0)))

(check-report)
