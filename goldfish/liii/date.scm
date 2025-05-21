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

(define-library (liii date)
(import (liii datetime))
(export date)
(begin

(define-case-class date
  ((year integer?)
   (month integer?)
   (day integer?))

(chained-define (@now)
  (let ((time-vec (g_date-now)))
    (date 
      :year (vector-ref time-vec 0)
      :month (vector-ref time-vec 1)
      :day (vector-ref time-vec 2))))

(define (%to-string)
  (define (pad2 n)  ; 补零到 2 位
    (if (< n 10)
        (string-append "0" (number->string n))
        (number->string n)))
  
  (let ((date-part (string-append (number->string year) "-"
                                  (pad2 month) "-"
                                  (pad2 day))))
       date-part))

)

) ; end of begin
) ; end of define-library

