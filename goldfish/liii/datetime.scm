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

(define-library (liii datetime)
(export datetime g_datetime-now)
(begin

(define-case-class datetime
  ((year integer?)
   (month integer?)
   (day integer?)
   (hour integer? 0)
   (minute integer? 0)
   (second integer? 0)
   (micro-second integer? 0))

(chained-define (@now)
  (let ((time-vec (g_datetime-now)))
    (datetime 
      :year (vector-ref time-vec 0)
      :month (vector-ref time-vec 1)
      :day (vector-ref time-vec 2)
      :hour (vector-ref time-vec 3)
      :minute (vector-ref time-vec 4)
      :second (vector-ref time-vec 5)
      :micro-second (vector-ref time-vec 6))))

(define (%to-string)
  (define (pad2 n)  ; 补零到 2 位
    (if (< n 10)
        (string-append "0" (number->string n))
        (number->string n)))
  (define (pad6 n)  ; 补零到 6 位（微秒）
    (let ((s (number->string n)))
      (string-append (make-string (- 6 (string-length s)) #\0) s)))
  
  (let ((date-part (string-append (number->string year) "-"
                                 (pad2 month) "-"
                                 (pad2 day)))
        (time-part (string-append (pad2 hour) ":"
                                 (pad2 minute) ":"
                                 (pad2 second))))
    (if (zero? micro-second)
        (string-append date-part " " time-part)
        (string-append date-part " " time-part "." (pad6 micro-second)))))

)

) ; end of begin
) ; end of define-library

