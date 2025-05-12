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

(import (liii list))
(import (scheme time))

(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display* msg (number->string (- end start)) "\n")))

(define (repeat n proc)
  (when (>= n 0)
        (proc)
        (repeat (- n 1) proc)))

(define l100 ((rich-list :range 1 10000 1) :collect))

(define (list-sum l)
  (fold + 0 l))

(define (rich-list-sum l)
  ($ l :fold 0 +))

(define (rlist x)
  (lambda (msg . args)
    (let ((env (funclet rlist)))
      (varlet env 'data x)
      (let1 r (case msg
                ((:fold)
                 (apply (env '%fold) args)))
        (cutlet env 'data)
        r))))

(with-let (funclet rlist)
  (define (%fold initial f)
    (fold f initial data))

  (varlet (funclet rlist) '%fold %fold))

(define (rlist-sum l)
  ((rlist l) :fold 0 +))

(timing "list%sum:\t" (lambda () (repeat 1000 (lambda () (list-sum l100)))))
(timing "rlist%sum:\t" (lambda () (repeat 1000 (lambda () (rlist-sum l100)))))
(timing "rich-list%sum:\t" (lambda () (repeat 1000 (lambda () (rich-list-sum l100)))))
