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

(define-library (liii range)
(import (liii oop) (only (liii lang) rich-list))
(export range)
(begin

(define-case-class range
  ((start integer?) (end integer?) (step integer? 1) (inclusive? boolean? #f))

(define* (@inclusive start end (step 1))
  (range start end step #t))

(define (in-range? x)
  (or (and (> step 0) (if inclusive? (<= x end) (< x end)))
      (and (< step 0) (if inclusive? (>= x end) (> x end)))))

(define (not-in-range? x)
  (or (and (> step 0) (> x end))
      (and (< step 0) (< x end))
      (and (= x end) (not inclusive?))))

(define (%empty?)
  (or (and (> start end) (> step 0))
      (and (< start end) (< step 0))
      (and (= start end) (not inclusive?))))

(define (%map map-func)
  (if (%empty?)
      (rich-list :empty)
      (let loop ((current start) (result '()))
        (if (not-in-range? current)
            (rich-list (reverse result))
            (loop (+ current step)
                  (cons (map-func current) result))))))

(define (%for-each proc)
  (when (not (%empty?))
    (let loop ((current start))
         (when (in-range? current)
               (proc current)
               (loop (+ current step))))))

(define (%filter f)
  (if (%empty?)
      (rich-list :empty)
      (let loop ((i start) (return '()))
        (if (not-in-range? i)
            (rich-list (reverse return))
            (loop (+ i step)
                  (if (f i)
                      (cons i return)
                       return))))))

(define (%contains elem)
  (if (%empty?)
      #f
      (if (<= (* (- elem start) (- elem end)) 0) ;判断是否在范围内
          (if (zero? step)
              (= elem start)
              (zero? (modulo (- elem start) (abs step))))
          #f)))

           
           

) ; define-case-cass
) ; begin
) ; define-library

