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
        (liii hash-table)
        (liii packrat))

(check-set-mode! 'report-failed)

(define (generator tokens)
  (let ((stream tokens))
    (lambda ()
      (if (null? stream)
        (values #f #f)
        (let ((base-token (car stream)))
          (set! stream (cdr stream))
          (values #f base-token))))))

;; simple parser

(define simple-parser
  (packrat-parser expr
    (expr ((a <- 'num) a)
          ((a <- 'id) a))))
(check-true (procedure? simple-parser))

(let* ((gen-num (generator '((num . 123))))
       (r-num (simple-parser (base-generator->results gen-num))))
  (check-true (parse-result-successful? r-num))
  (check (parse-result-semantic-value r-num) => 123))

(let* ((gen-id (generator '((id . foo))))
       (r-id (simple-parser (base-generator->results gen-id))))
  (check-true (parse-result-successful? r-id))
  (check (parse-result-semantic-value r-id) => 'foo))

(let* ((gen-invalid (generator '((foo . bar))))
       (r-invalid (simple-parser (base-generator->results gen-invalid))))
  (check-false (parse-result-successful? r-invalid)))

;; calc

(define calc-env (make-hash-table))
(define calc
  (packrat-parser expr
    (expr (('begin body <- exprs 'end) body)
          ((var <- 'id ':= val <- expr) (hash-table-set! calc-env var val))
          ((a <- mulexp '+ b <- expr) (+ a b))
          ((a <- mulexp '- b <- expr) (- a b))
          ((a <- mulexp) a))
    (mulexp ((a <- powexp '* b <- mulexp) (* a b))
            ((a <- powexp '/ b <- mulexp) (/ a b))
            ((a <- powexp) a))
    (powexp ((a <- simple '^ b <- powexp) (expt a b))
            ((a <- simple) a))
    (simple ((a <- 'num) a)
            ((a <- 'id) (calc-env a))
            (('oparen a <- expr 'cparen) a))
    (exprs ((a <- expr rest <- exprs) rest)
           ((a <- expr) a))))
(check-true (procedure? calc))

(let* ((g (generator '((num . 2) (+) (num . 3))))
       (expected (+ 2 3))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected))
(hash-table-clear! calc-env)

;; NOTE: the `calc` parser is right recursion;
;;       packrat hates left recursion
(let* ((g (generator '((num . 1) (-) (num . 2) (+) (num . 3))))
       (expected (- 1 (+ 2 3)))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected))
(hash-table-clear! calc-env)

;; ditto
(let* ((g (generator '((num . 1) (*) (num . 2) (/) (num . 3))))
       (expected (* 1 (/ 2 3)))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected))
(hash-table-clear! calc-env)

(let* ((g (generator '((oparen) (num . 2) (+) (num . 3) (cparen)
                       (*) (num . 4))))
       (expected (* (+ 2 3) 4))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected))
(hash-table-clear! calc-env)

(let* ((g (generator '((num . 2) (^) (num . 3))))
       (expected (expt 2 3))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected))
(hash-table-clear! calc-env)

(let* ((g (generator '((num . 8) (/) (num . 2))))
       (expected (/ 8 2))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected))
(hash-table-clear! calc-env)

(let* ((g (generator
            '((begin) (id . ans) (:=) (num . 42)
                      (oparen) (num . 2) (+) (id . ans) (cparen)
                      (^) (num . 3)
              (end))))
       (expected (begin (define ans 42)
                        (expt (+ 2 ans)
                              3)))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected))
(hash-table-clear! calc-env)

(let* ((g (generator '((oparen) (num . 2) (+) (num . 3) (cparen)
                       (^)
                       (oparen) (num . 1) (+) (num . 1) (cparen))))
       (expected (expt (+ 2 3) (+ 1 1)))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected))
(hash-table-clear! calc-env)

(let* ((g (generator '((begin) (id . a) (:=) (num . 10)
                       (id . b) (:=) (num . 20)
                       (id . a) (*) (id . b)
                       (end))))
       (expected (begin (define a 10) (define b 20) (* a b)))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected))
(hash-table-clear! calc-env)

(let* ((g-invalid (generator '((begin) (foo . bar) (end))))
       (r-invalid (calc (base-generator->results g-invalid))))
  (check-false (parse-result-successful? r-invalid)))
(hash-table-clear! calc-env)

(check-report)
