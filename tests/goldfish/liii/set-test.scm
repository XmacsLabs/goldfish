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

(import (liii base) (liii set) (liii check))

(check-set-mode! 'report-failed)

;; Test factory methods
(check ((hash-set :empty) :size) => 0)
(check ((hash-set :empty) :empty?) => #t)

;; Test basic operations
(let1 ht (make-hash-table)
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (hash-table-set! ht 'c #t)
  (check ((hash-set ht) :size) => 3))

(let1 ht (make-hash-table)
  (check ((hash-set ht) :empty?) => #t)
  (hash-table-set! ht 'a #t)
  (check ((hash-set ht) :empty?) => #f))

(let1 ht (make-hash-table)
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (check ((hash-set ht) :contains 'a) => #t)
  (check ((hash-set ht) :contains 'c) => #f))

;; Test non-destructive operations
(let1 ht (make-hash-table)
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let1 s (hash-set ht)
    (check (s :add-one 'c) => (let1 new-ht (make-hash-table)
                                (hash-table-set! new-ht 'a #t)
                                (hash-table-set! new-ht 'b #t)
                                (hash-table-set! new-ht 'c #t)
                                (hash-set new-ht)))
    (check (s :add-one 'd) => (let1 new-ht (make-hash-table)
                                (hash-table-set! new-ht 'a #t)
                                (hash-table-set! new-ht 'b #t)
                                (hash-table-set! new-ht 'd #t)
                                (hash-set new-ht)))))

(let1 ht (make-hash-table)
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let1 s (hash-set ht)
    (check (s :remove 'a) => (let1 new-ht (make-hash-table)
                              (hash-table-set! new-ht 'b #t)
                              (hash-set new-ht)))
    (check (s :remove 'b) => (let1 new-ht (make-hash-table)
                              (hash-table-set! new-ht 'a #t)
                              (hash-set new-ht)))))

;; Test destructive operations
(let1 ht (make-hash-table)
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let1 s (hash-set ht)
    (check (s :add-one! 'c) => (let1 new-ht (make-hash-table)
                                (hash-table-set! new-ht 'a #t)
                                (hash-table-set! new-ht 'b #t)
                                (hash-table-set! new-ht 'c #t)
                                (hash-set new-ht)))
    (check (s :add-one! 'd) => (let1 new-ht (make-hash-table)
                                (hash-table-set! new-ht 'a #t)
                                (hash-table-set! new-ht 'b #t)
                                (hash-table-set! new-ht 'c #t)
                                (hash-table-set! new-ht 'd #t)
                                (hash-set new-ht)))))

(let1 ht (make-hash-table)
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let1 s (hash-set ht)
    (check (s :remove! 'a) => (let1 new-ht (make-hash-table)
                                (hash-table-set! new-ht 'b #t)
                                (hash-set new-ht)))
    (check (s :remove! 'b) => (let1 new-ht (make-hash-table)
                                (hash-set new-ht)))))

(let1 ht (make-hash-table)
  (hash-table-set! ht 'a #t)
  (hash-table-set! ht 'b #t)
  (let1 s (hash-set ht)
    (check (s :clear!) => (hash-set (make-hash-table)))))

;;; ============================================================
;;; SRFI 113 Sets Tests
;;; ============================================================

(import (srfi srfi-128))

;; Test set constructor and set?
(let1 s (set (make-equal-comparator) 1 2 3)
  (check (set? s) => #t)
  (check (set? '(1 2 3)) => #f)
  (check (set? 42) => #f))

;; Test set-contains?
(let1 s (set (make-equal-comparator) 1 2 3)
  (check (set-contains? s 1) => #t)
  (check (set-contains? s 2) => #t)
  (check (set-contains? s 3) => #t)
  (check (set-contains? s 4) => #f)
  (check (set-contains? s 0) => #f))

;; Test set-size
(check (set-size (set (make-equal-comparator))) => 0)
(check (set-size (set (make-equal-comparator) 1)) => 1)
(check (set-size (set (make-equal-comparator) 1 2 3)) => 3)
;; Duplicates should be ignored
(check (set-size (set (make-equal-comparator) 1 2 2 3 3 3)) => 3)

;; Test set-empty?
(check (set-empty? (set (make-equal-comparator))) => #t)
(check (set-empty? (set (make-equal-comparator) 1)) => #f)

;; Test set-adjoin
(let1 s (set (make-equal-comparator) 1 2)
  (let1 s2 (set-adjoin s 3)
    (check (set-size s2) => 3)
    (check (set-contains? s2 3) => #t)
    ;; Original set unchanged
    (check (set-size s) => 2)
    (check (set-contains? s 3) => #f)))

;; Test set-adjoin with duplicates
(let1 s (set (make-equal-comparator) 1 2)
  (let1 s2 (set-adjoin s 2 3)
    (check (set-size s2) => 3)))

;; Test set-delete
(let1 s (set (make-equal-comparator) 1 2 3)
  (let1 s2 (set-delete s 2)
    (check (set-size s2) => 2)
    (check (set-contains? s2 2) => #f)
    ;; Original set unchanged
    (check (set-size s) => 3)))

;; Test set->list and list->set
(let1 s (set (make-equal-comparator) 'a 'b 'c)
  (check (length (set->list s)) => 3))

(let1 s (list->set (make-equal-comparator) '(1 2 3 2 1))
  (check (set-size s) => 3))

;; Test set-union
(let ((s1 (set (make-equal-comparator) 1 2 3))
      (s2 (set (make-equal-comparator) 2 3 4)))
  (let1 u (set-union s1 s2)
    (check (set-size u) => 4)
    (check (set-contains? u 1) => #t)
    (check (set-contains? u 4) => #t)))

;; Test set-intersection
(let ((s1 (set (make-equal-comparator) 1 2 3))
      (s2 (set (make-equal-comparator) 2 3 4)))
  (let1 i (set-intersection s1 s2)
    (check (set-size i) => 2)
    (check (set-contains? i 2) => #t)
    (check (set-contains? i 3) => #t)
    (check (set-contains? i 1) => #f)))

;; Test set-difference
(let ((s1 (set (make-equal-comparator) 1 2 3))
      (s2 (set (make-equal-comparator) 2 3 4)))
  (let1 d (set-difference s1 s2)
    (check (set-size d) => 1)
    (check (set-contains? d 1) => #t)))

;; Test set-xor
(let ((s1 (set (make-equal-comparator) 1 2 3))
      (s2 (set (make-equal-comparator) 2 3 4)))
  (let1 x (set-xor s1 s2)
    (check (set-size x) => 2)
    (check (set-contains? x 1) => #t)
    (check (set-contains? x 4) => #t)
    (check (set-contains? x 2) => #f)))

;; Test set=?
(let ((s1 (set (make-equal-comparator) 1 2 3))
      (s2 (set (make-equal-comparator) 3 2 1))
      (s3 (set (make-equal-comparator) 1 2)))
  (check (set=? s1 s2) => #t)
  (check (set=? s1 s3) => #f))

;; Test set<? (proper subset)
(let ((s1 (set (make-equal-comparator) 1 2))
      (s2 (set (make-equal-comparator) 1 2 3)))
  (check (set<? s1 s2) => #t)
  (check (set<? s2 s1) => #f)
  (check (set<? s1 s1) => #f))

;; Test set-disjoint?
(let ((s1 (set (make-equal-comparator) 1 2))
      (s2 (set (make-equal-comparator) 3 4))
      (s3 (set (make-equal-comparator) 2 3)))
  (check (set-disjoint? s1 s2) => #t)
  (check (set-disjoint? s1 s3) => #f))

;; Test set-fold
(let1 s (set (make-equal-comparator) 1 2 3 4 5)
  (check (set-fold + 0 s) => 15))

;; Test set-map
(let1 s (set (make-equal-comparator) 1 2 3)
  (let1 s2 (set-map (make-equal-comparator) (lambda (x) (* x 2)) s)
    (check (set-size s2) => 3)
    (check (set-contains? s2 2) => #t)
    (check (set-contains? s2 4) => #t)
    (check (set-contains? s2 6) => #t)))

;; Test set-filter
(let1 s (set (make-equal-comparator) 1 2 3 4 5)
  (let1 s2 (set-filter odd? s)
    (check (set-size s2) => 3)
    (check (set-contains? s2 1) => #t)
    (check (set-contains? s2 3) => #t)
    (check (set-contains? s2 5) => #t)))

;; Test set-count
(let1 s (set (make-equal-comparator) 1 2 3 4 5)
  (check (set-count odd? s) => 3)
  (check (set-count even? s) => 2))

;; Test set-any? and set-every?
(let1 s (set (make-equal-comparator) 2 4 6)
  (check (set-any? odd? s) => #f)
  (check (set-any? even? s) => #t)
  (check (set-every? even? s) => #t)
  (check (set-every? odd? s) => #f))

;; Test set-unfold
(let1 s (set-unfold (make-equal-comparator)
                    (lambda (x) (> x 5))    ; stop?
                    (lambda (x) x)          ; mapper
                    (lambda (x) (+ x 1))    ; successor
                    1)                      ; seed
  (check (set-size s) => 5)
  (check (set-contains? s 1) => #t)
  (check (set-contains? s 5) => #t)
  (check (set-contains? s 6) => #f))

;; Test set-replace
(let1 s (set (make-equal-comparator) 1 2 3)
  ;; Replace existing element
  (let1 s2 (set-replace s 2)
    (check (set-size s2) => 3)
    (check (set-contains? s2 2) => #t))
  ;; Replace non-existing element (should return unchanged)
  (let1 s3 (set-replace s 5)
    (check (set-size s3) => 3)
    (check (set-contains? s3 5) => #f)))

;; Test set-search! with element found
(let1 s (set (make-equal-comparator) 1 2 3)
  ;; Test update continuation
  (let-values (((new-set obj) 
                (set-search! s 2
                             (lambda (insert ignore) (insert 'not-called))
                             (lambda (elem update remove) (update 20 'updated)))))
    (check (set-contains? new-set 20) => #t)
    (check (set-contains? new-set 2) => #f)
    (check obj => 'updated))
  ;; Test remove continuation
  (let-values (((new-set obj)
                (set-search! s 2
                             (lambda (insert ignore) (insert 'not-called))
                             (lambda (elem update remove) (remove 'removed)))))
    (check (set-size new-set) => 2)
    (check (set-contains? new-set 2) => #f)
    (check obj => 'removed)))

;; Test set-search! with element not found
(let1 s (set (make-equal-comparator) 1 2 3)
  ;; Test insert continuation
  (let-values (((new-set obj)
                (set-search! s 5
                             (lambda (insert ignore) (insert 'inserted))
                             (lambda (elem update remove) (update 'not-called 'x)))))
    (check (set-contains? new-set 5) => #t)
    (check (set-size new-set) => 4)
    (check obj => 'inserted))
  ;; Test ignore continuation
  (let-values (((new-set obj)
                (set-search! s 5
                             (lambda (insert ignore) (ignore 'ignored))
                             (lambda (elem update remove) (update 'not-called 'x)))))
    (check (set-size new-set) => 3)
    (check (set-contains? new-set 5) => #f)
    (check obj => 'ignored)))

;; Test set-member
(let1 s (set (make-equal-comparator) 1 2 3)
  (check (set-member s 2 'not-found) => 2)
  (check (set-member s 5 'not-found) => 'not-found))

;; Test set-find
(let1 s (set (make-equal-comparator) 1 2 3 4 5)
  (check (> (set-find even? s (lambda () #f)) 0) => #t)
  (check (set-find (lambda (x) (> x 10)) s (lambda () 'not-found)) => 'not-found))

;; Test set-remove (opposite of set-filter)
(let1 s (set (make-equal-comparator) 1 2 3 4 5)
  (let1 s2 (set-remove odd? s)
    (check (set-size s2) => 2)
    (check (set-contains? s2 2) => #t)
    (check (set-contains? s2 4) => #t)))

;; Test set-partition
(let1 s (set (make-equal-comparator) 1 2 3 4 5)
  (let-values (((evens odds) (set-partition even? s)))
    (check (set-size evens) => 2)
    (check (set-size odds) => 3)
    (check (set-contains? evens 2) => #t)
    (check (set-contains? odds 1) => #t)))

;; Test set-copy
(let1 s (set (make-equal-comparator) 1 2 3)
  (let1 s2 (set-copy s)
    (check (set=? s s2) => #t)))

;; Test set-element-comparator
(let1 s (set (make-equal-comparator) 1 2 3)
  (check (comparator? (set-element-comparator s)) => #t))

;; Test set-comparator
(check (comparator? set-comparator) => #t)
(let ((s1 (set (make-equal-comparator) 1 2 3))
      (s2 (set (make-equal-comparator) 1 2 3)))
  (check ((comparator-equality-predicate set-comparator) s1 s2) => #t))

;; Test set<=? and set>=?
(let ((s1 (set (make-equal-comparator) 1 2))
      (s2 (set (make-equal-comparator) 1 2 3))
      (s3 (set (make-equal-comparator) 1 2)))
  (check (set<=? s1 s2) => #t)
  (check (set<=? s1 s3) => #t)
  (check (set<=? s2 s1) => #f)
  (check (set>=? s2 s1) => #t)
  (check (set>=? s1 s3) => #t))

;; Test set>?
(let ((s1 (set (make-equal-comparator) 1 2 3))
      (s2 (set (make-equal-comparator) 1 2)))
  (check (set>? s1 s2) => #t)
  (check (set>? s2 s1) => #f))

;; Test set-for-each
(let ((s (set (make-equal-comparator) 1 2 3))
      (sum 0))
  (set-for-each (lambda (x) (set! sum (+ sum x))) s)
  (check sum => 6))

;; Test list->set!
(let1 s (set (make-equal-comparator) 1 2)
  (let1 s2 (list->set! s '(3 4))
    (check (set-size s2) => 4)
    (check (set-contains? s2 3) => #t)))

;; Test set-delete-all
(let1 s (set (make-equal-comparator) 1 2 3 4 5)
  (let1 s2 (set-delete-all s '(2 4))
    (check (set-size s2) => 3)
    (check (set-contains? s2 2) => #f)
    (check (set-contains? s2 4) => #f)))

(check-report)

