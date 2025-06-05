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

(define-library (liii float)
(import (liii oop)
        (liii error))
(export rich-float)
(begin

(typed-define (rich-float (data float?))

(define %this (inlet 'data data))

(define (%equals that)
  (and (let? that) ; TODO: must be a rich-float
       (equal? (%this 'data) (that 'data))))

(define (%to-string)
  (number->string data))

(define (%get) data)

(define (%abs)
  (if (< data 0)
      (- 0 data)
      data))

(define (%sqrt)
  (if (< data 0)
      (value-error
        (format #f "rich-float%sqrt of negative float is undefined!         ** Got ~a **" data))
      (sqrt data)))

(define (%apply msg . args)
  (if (defined? msg %this #t)
      (apply (%this msg) args)))

(varlet %this :equals %equals)
(varlet %this :to-string %to-string)
(varlet %this :get %get)
(varlet %this :abs %abs)
(varlet %this :sqrt %sqrt)
(varlet %this :apply %apply)

) ; end of rich-float

) ; end of begin
) ; end of define-library
