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

;; Test set-delete! (linear update - mutates in place)
(let1 s (set (make-equal-comparator) 1 2 3)
  (let1 s2 (set-delete! s 2)
    (check (set-size s2) => 2)
    (check (set-contains? s2 2) => #f)
    ;; set-delete! is permitted to mutate s, verify s and s2 are eq?
    (check (eq? s s2) => #t)
    ;; Verify original set was mutated
    (check (set-size s) => 2)
    (check (set-contains? s 2) => #f)))

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
    (check obj => 'updated)
    ;; Verify set-search! mutates in place (s and new-set are eq?)
    (check (eq? s new-set) => #t)
    (check (set-contains? s 20) => #t)))

;; Test set-search! with element found - remove continuation
(let1 s (set (make-equal-comparator) 1 2 3)
  ;; Test remove continuation
  (let-values (((new-set obj)
                (set-search! s 2
                             (lambda (insert ignore) (insert 'not-called))
                             (lambda (elem update remove) (remove 'removed)))))
    (check (set-size new-set) => 2)
    (check (set-contains? new-set 2) => #f)
    (check obj => 'removed)
    ;; Verify set-search! mutates in place
    (check (eq? s new-set) => #t)
    (check (set-size s) => 2)))

;; Test set-search! with element not found
(let1 s (set (make-equal-comparator) 1 2 3)
  ;; Test insert continuation
  (let-values (((new-set obj)
                (set-search! s 5
                             (lambda (insert ignore) (insert 'inserted))
                             (lambda (elem update remove) (update 'not-called 'x)))))
    (check (set-contains? new-set 5) => #t)
    (check (set-size new-set) => 4)
    (check obj => 'inserted)
    ;; Verify set-search! mutates in place
    (check (eq? s new-set) => #t)
    (check (set-contains? s 5) => #t)))

;; Test set-search! with element not found - ignore continuation  
(let1 s (set (make-equal-comparator) 1 2 3)
  ;; Test ignore continuation
  (let-values (((new-set obj)
                (set-search! s 5
                             (lambda (insert ignore) (ignore 'ignored))
                             (lambda (elem update remove) (update 'not-called 'x)))))
    (check (set-size new-set) => 3)
    (check (set-contains? new-set 5) => #f)
    (check obj => 'ignored)
    ;; Verify set is unchanged and returned as-is
    (check (eq? s new-set) => #t)))

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

;;; ============================================================
;;; SRFI 113 Bags tests
;;; ============================================================

;; Test bag constructor and bag?
(check (bag? (bag (make-equal-comparator))) => #t)
(check (bag? (bag (make-equal-comparator) 1 2 3)) => #t)
(check (bag? '(1 2 3)) => #f)
(check (bag? (set (make-equal-comparator) 1 2 3)) => #f)

;; Test bag with duplicates
(let1 b (bag (make-equal-comparator) 1 1 1 2 2 3)
  (check (bag-size b) => 6)
  (check (bag-unique-size b) => 3)
  (check (bag-element-count b 1) => 3)
  (check (bag-element-count b 2) => 2)
  (check (bag-element-count b 3) => 1))

;; Test bag-empty?
(check (bag-empty? (bag (make-equal-comparator))) => #t)
(check (bag-empty? (bag (make-equal-comparator) 1)) => #f)

;; Test bag-contains?
(let1 b (bag (make-equal-comparator) 1 2 3)
  (check (bag-contains? b 1) => #t)
  (check (bag-contains? b 4) => #f))

;; Test bag-disjoint?
(let ((b1 (bag (make-equal-comparator) 1 2))
      (b2 (bag (make-equal-comparator) 3 4))
      (b3 (bag (make-equal-comparator) 2 5)))
  (check (bag-disjoint? b1 b2) => #t)
  (check (bag-disjoint? b1 b3) => #f))

;; Test bag-member
(let1 b (bag (make-equal-comparator) 1 2 3)
  (check (bag-member b 2 #f) => 2)
  (check (bag-member b 5 'not-found) => 'not-found))

;; Test bag-element-comparator
(let1 comp (make-equal-comparator)
  (check (bag-element-comparator (bag comp 1 2)) => comp))

;; Test bag-adjoin
(let1 b (bag (make-equal-comparator) 1 2)
  (let1 b2 (bag-adjoin b 2 3)
    (check (bag-element-count b2 2) => 2)  ; 2 was already there, now has count 2
    (check (bag-element-count b2 3) => 1)
    (check (bag-size b2) => 4)))

;; Test bag-adjoin! (linear update - mutates in place)
(let1 b (bag (make-equal-comparator) 1 2)
  (let1 b2 (bag-adjoin! b 2 3)
    (check (bag-element-count b2 2) => 2)
    (check (bag-element-count b2 3) => 1)
    ;; Verify bag-adjoin! mutates in place (b and b2 are eq?)
    (check (eq? b b2) => #t)
    (check (bag-element-count b 2) => 2)))

;; Test bag-delete
(let1 b (bag (make-equal-comparator) 1 1 1 2)
  (let1 b2 (bag-delete b 1)
    (check (bag-element-count b2 1) => 2)  ; deleted one 1
    (check (bag-size b2) => 3)))

;; Test bag-delete! (linear update - mutates in place)
(let1 b (bag (make-equal-comparator) 1 1 1 2)
  (let1 b2 (bag-delete! b 1)
    (check (bag-element-count b2 1) => 2)
    (check (bag-size b2) => 3)
    ;; Verify bag-delete! mutates in place
    (check (eq? b b2) => #t)
    (check (bag-element-count b 1) => 2)))

;; Test bag-delete-all
(let1 b (bag (make-equal-comparator) 1 1 2 2 3)
  (let1 b2 (bag-delete-all b '(1 2))
    (check (bag-element-count b2 1) => 0)
    (check (bag-element-count b2 2) => 0)
    (check (bag-element-count b2 3) => 1)))

;; Test bag-delete-all! (linear update - mutates in place)
(let1 b (bag (make-equal-comparator) 1 1 2 2 3)
  (let1 b2 (bag-delete-all! b '(1 2))
    (check (bag-element-count b2 1) => 0)
    (check (bag-element-count b2 2) => 0)
    ;; Verify bag-delete-all! mutates in place
    (check (eq? b b2) => #t)))

;; Test bag-size
(check (bag-size (bag (make-equal-comparator))) => 0)
(check (bag-size (bag (make-equal-comparator) 1 2 3)) => 3)
(check (bag-size (bag (make-equal-comparator) 1 1 1)) => 3)

;; Test bag-find
(let1 b (bag (make-equal-comparator) 1 2 3)
  (check (bag-find even? b (lambda () 'not-found)) => 2)
  (check (bag-find (lambda (x) (> x 10)) b (lambda () 'not-found)) => 'not-found))

;; Test bag-count
(let1 b (bag (make-equal-comparator) 1 2 2 3 4 4 4)
  (check (bag-count even? b) => 5))  ; 2+2+4+4+4 = 5 even elements

;; Test bag-any?
(let1 b (bag (make-equal-comparator) 1 3 5)
  (check (bag-any? even? b) => #f)
  (check (bag-any? odd? b) => #t))

;; Test bag-every?
(let1 b (bag (make-equal-comparator) 2 4 6)
  (check (bag-every? even? b) => #t)
  (check (bag-every? (lambda (x) (> x 5)) b) => #f))

;; Test bag-map
(let1 b (bag (make-equal-comparator) 1 2 2 3)
  (let1 b2 (bag-map (make-equal-comparator) (lambda (x) (* x 10)) b)
    (check (bag-element-count b2 10) => 1)
    (check (bag-element-count b2 20) => 2)
    (check (bag-element-count b2 30) => 1)))

;; Test bag-for-each
(let* ((b (bag (make-equal-comparator) 1 1 2))
       (sum 0))
  (bag-for-each (lambda (x) (set! sum (+ sum x))) b)
  (check sum => 4))  ; 1+1+2 = 4

;; Test bag-fold
(let1 b (bag (make-equal-comparator) 1 1 2 3)
  (check (bag-fold + 0 b) => 7))  ; 1+1+2+3 = 7

;; Test bag-filter
(let1 b (bag (make-equal-comparator) 1 2 2 3 4 4 4)
  (let1 b2 (bag-filter even? b)
    (check (bag-unique-size b2) => 2)  ; 2 and 4
    (check (bag-element-count b2 2) => 2)
    (check (bag-element-count b2 4) => 3)))

;; Test bag-remove
(let1 b (bag (make-equal-comparator) 1 2 2 3 4)
  (let1 b2 (bag-remove even? b)
    (check (bag-element-count b2 1) => 1)
    (check (bag-element-count b2 3) => 1)
    (check (bag-contains? b2 2) => #f)))

;; Test bag-partition
(let1 b (bag (make-equal-comparator) 1 2 2 3 4)
  (let-values (((evens odds) (bag-partition even? b)))
    (check (bag-element-count evens 2) => 2)
    (check (bag-element-count evens 4) => 1)
    (check (bag-element-count odds 1) => 1)
    (check (bag-element-count odds 3) => 1)))

;; Test bag-copy
(let* ((b1 (bag (make-equal-comparator) 1 1 2))
       (b2 (bag-copy b1)))
  (check (bag=? b1 b2) => #t))

;; Test bag->list
(let1 b (bag (make-equal-comparator) 1 1 2)
  (let1 lst (bag->list b)
    (check (length lst) => 3)))

;; Test list->bag
(let1 b (list->bag (make-equal-comparator) '(1 1 2 2 2))
  (check (bag-element-count b 1) => 2)
  (check (bag-element-count b 2) => 3))

;; Test bag=?
(let ((b1 (bag (make-equal-comparator) 1 1 2))
      (b2 (bag (make-equal-comparator) 1 1 2))
      (b3 (bag (make-equal-comparator) 1 2 2)))
  (check (bag=? b1 b2) => #t)
  (check (bag=? b1 b3) => #f))

;; Test bag<?
(let ((b1 (bag (make-equal-comparator) 1 2))
      (b2 (bag (make-equal-comparator) 1 1 2 3)))
  (check (bag<? b1 b2) => #t)
  (check (bag<? b2 b1) => #f))

;; Test bag<=?
(let ((b1 (bag (make-equal-comparator) 1 2))
      (b2 (bag (make-equal-comparator) 1 1 2)))
  (check (bag<=? b1 b2) => #t)
  (check (bag<=? b1 b1) => #t))

;; Test bag-union (max of counts)
(let ((b1 (bag (make-equal-comparator) 1 1 2))
      (b2 (bag (make-equal-comparator) 1 2 2 3)))
  (let1 b (bag-union b1 b2)
    (check (bag-element-count b 1) => 2)  ; max(2,1)
    (check (bag-element-count b 2) => 2)  ; max(1,2)
    (check (bag-element-count b 3) => 1)))

;; Test bag-intersection (min of counts)
(let ((b1 (bag (make-equal-comparator) 1 1 1 2 2))
      (b2 (bag (make-equal-comparator) 1 1 2 2 2 3)))
  (let1 b (bag-intersection b1 b2)
    (check (bag-element-count b 1) => 2)  ; min(3,2)
    (check (bag-element-count b 2) => 2)  ; min(2,3)
    (check (bag-element-count b 3) => 0))) ; not in b1

;; Test bag-difference
(let ((b1 (bag (make-equal-comparator) 1 1 1 2 2))
      (b2 (bag (make-equal-comparator) 1 2)))
  (let1 b (bag-difference b1 b2)
    (check (bag-element-count b 1) => 2)  ; 3-1
    (check (bag-element-count b 2) => 1))) ; 2-1

;; Test bag-xor (absolute difference of counts)
(let ((b1 (bag (make-equal-comparator) 1 1 1 2))
      (b2 (bag (make-equal-comparator) 1 2 2 2)))
  (let1 b (bag-xor b1 b2)
    (check (bag-element-count b 1) => 2)  ; |3-1|
    (check (bag-element-count b 2) => 2))) ; |1-3|

;; Test bag-sum (sum of counts)
(let ((b1 (bag (make-equal-comparator) 1 2))
      (b2 (bag (make-equal-comparator) 1 2 3)))
  (let1 b (bag-sum b1 b2)
    (check (bag-element-count b 1) => 2)
    (check (bag-element-count b 2) => 2)
    (check (bag-element-count b 3) => 1)))

;; Test bag-product
(let1 b (bag (make-equal-comparator) 1 2 2)
  (let1 b2 (bag-product 3 b)
    (check (bag-element-count b2 1) => 3)
    (check (bag-element-count b2 2) => 6)))

;; Test bag-unique-size
(let1 b (bag (make-equal-comparator) 1 1 1 2 2 3)
  (check (bag-unique-size b) => 3))

;; Test bag-element-count
(let1 b (bag (make-equal-comparator) 'a 'a 'a 'b)
  (check (bag-element-count b 'a) => 3)
  (check (bag-element-count b 'b) => 1)
  (check (bag-element-count b 'c) => 0))

;; Test bag-for-each-unique
(let* ((b (bag (make-equal-comparator) 1 1 2 2 2))
       (pairs '()))
  (bag-for-each-unique 
   (lambda (elem count) (set! pairs (cons (cons elem count) pairs)))
   b)
  (check (length pairs) => 2))

;; Test bag-fold-unique
(let1 b (bag (make-equal-comparator) 1 1 2 2 2)
  (let1 total (bag-fold-unique (lambda (elem count acc) (+ acc count)) 0 b)
    (check total => 5)))

;; Test bag-increment!
(let1 b (bag (make-equal-comparator) 1 2)
  (let1 b2 (bag-increment! b 1 5)
    (check (bag-element-count b2 1) => 6)
    ;; Verify bag-increment! mutates in place
    (check (eq? b b2) => #t)
    (check (bag-element-count b 1) => 6)))

;; Test bag-decrement!
(let1 b (bag (make-equal-comparator) 1 1 1 1 1)
  (let1 b2 (bag-decrement! b 1 3)
    (check (bag-element-count b2 1) => 2)
    ;; Verify bag-decrement! mutates in place
    (check (eq? b b2) => #t)
    (check (bag-element-count b 1) => 2)))

;; Test bag->set
(let1 b (bag (make-equal-comparator) 1 1 2 2 2 3)
  (let1 s (bag->set b)
    (check (set? s) => #t)
    (check (set-size s) => 3)
    (check (set-contains? s 1) => #t)
    (check (set-contains? s 2) => #t)
    (check (set-contains? s 3) => #t)))

;; Test set->bag
(let1 s (set (make-equal-comparator) 1 2 3)
  (let1 b (set->bag s)
    (check (bag? b) => #t)
    (check (bag-size b) => 3)
    (check (bag-element-count b 1) => 1)
    (check (bag-element-count b 2) => 1)))

;; Test set->bag!
(let ((b (bag (make-equal-comparator) 1 1))
      (s (set (make-equal-comparator) 2 3)))
  (let1 b2 (set->bag! b s)
    (check (bag-element-count b2 1) => 2)
    (check (bag-element-count b2 2) => 1)
    (check (bag-element-count b2 3) => 1)
    ;; Verify set->bag! mutates in place
    (check (eq? b b2) => #t)))

;; Test bag->alist
(let1 b (bag (make-equal-comparator) 1 1 2 2 2)
  (let1 alist (bag->alist b)
    (check (length alist) => 2)))

;; Test alist->bag
(let1 b (alist->bag (make-equal-comparator) '((a . 3) (b . 2)))
  (check (bag-element-count b 'a) => 3)
  (check (bag-element-count b 'b) => 2))

;; Test bag-unfold
(let1 b (bag-unfold (make-equal-comparator)
                    (lambda (x) (> x 5))
                    (lambda (x) x)
                    (lambda (x) (+ x 1))
                    1)
  (check (bag-size b) => 5)
  (check (bag-contains? b 1) => #t)
  (check (bag-contains? b 5) => #t)
  (check (bag-contains? b 6) => #f))

;; Test bag-comparator
(check (comparator? bag-comparator) => #t)
(let ((b1 (bag (make-equal-comparator) 1 2))
      (b2 (bag (make-equal-comparator) 1 2))
      (b3 (bag (make-equal-comparator) 1 2 2)))
  (check ((comparator-equality-predicate bag-comparator) b1 b2) => #t)
  (check ((comparator-equality-predicate bag-comparator) b1 b3) => #f))

;;; ============================================================
;;; Edge Cases / Boundary Tests
;;; ============================================================

;;; ----- Set Edge Cases -----

;; Empty set operations
(let1 empty-s (set (make-equal-comparator))
  (check (set-size empty-s) => 0)
  (check (set-empty? empty-s) => #t)
  (check (set-contains? empty-s 1) => #f)
  (check (set->list empty-s) => '())
  (check (set-count (lambda (x) #t) empty-s) => 0)
  (check (set-any? (lambda (x) #t) empty-s) => #f)
  (check (set-every? (lambda (x) #t) empty-s) => #t)  ; vacuously true
  (check (set-find (lambda (x) #t) empty-s (lambda () 'not-found)) => 'not-found))

;; Empty set theory operations
(let ((empty-s (set (make-equal-comparator)))
      (s (set (make-equal-comparator) 1 2 3)))
  (check (set-size (set-union empty-s s)) => 3)
  (check (set-size (set-union s empty-s)) => 3)
  (check (set-size (set-intersection empty-s s)) => 0)
  (check (set-size (set-difference s empty-s)) => 3)
  (check (set-size (set-difference empty-s s)) => 0)
  (check (set-size (set-xor empty-s s)) => 3))

;; Single element set
(let1 single-s (set (make-equal-comparator) 42)
  (check (set-size single-s) => 1)
  (check (set-contains? single-s 42) => #t)
  (check (set-size (set-delete single-s 42)) => 0)
  (check (set-empty? (set-delete single-s 42)) => #t))

;; Self operations
(let1 s (set (make-equal-comparator) 1 2 3)
  (check (set=? s s) => #t)
  (check (set<? s s) => #f)
  (check (set<=? s s) => #t)
  (check (set-size (set-union s s)) => 3)
  (check (set-size (set-intersection s s)) => 3)
  (check (set-size (set-difference s s)) => 0)
  (check (set-size (set-xor s s)) => 0))

;; Delete non-existent element
(let1 s (set (make-equal-comparator) 1 2 3)
  (let1 s2 (set-delete s 999)
    (check (set-size s2) => 3)
    (check (set=? s s2) => #t)))

;; Adjoin already-existing element (no change)
(let1 s (set (make-equal-comparator) 1 2 3)
  (let1 s2 (set-adjoin s 2)
    (check (set-size s2) => 3)))

;; Empty list->set
(let1 s (list->set (make-equal-comparator) '())
  (check (set-empty? s) => #t))

;; set-delete-all with empty list
(let1 s (set (make-equal-comparator) 1 2 3)
  (check (set-size (set-delete-all s '())) => 3))

;; set-delete-all with non-existent elements
(let1 s (set (make-equal-comparator) 1 2 3)
  (check (set-size (set-delete-all s '(99 100))) => 3))

;; set-filter/remove returning empty or full
(let1 s (set (make-equal-comparator) 1 2 3)
  (check (set-empty? (set-filter (lambda (x) #f) s)) => #t)
  (check (set-size (set-filter (lambda (x) #t) s)) => 3)
  (check (set-size (set-remove (lambda (x) #f) s)) => 3)
  (check (set-empty? (set-remove (lambda (x) #t) s)) => #t))

;; Single argument comparisons (should return #t)
(let1 s (set (make-equal-comparator) 1 2)
  (check (set=? s) => #t)
  (check (set<? s) => #t)
  (check (set>? s) => #t)
  (check (set<=? s) => #t)
  (check (set>=? s) => #t))

;; set-disjoint? with empty set
(let ((empty-s (set (make-equal-comparator)))
      (s (set (make-equal-comparator) 1 2 3)))
  (check (set-disjoint? empty-s s) => #t)
  (check (set-disjoint? s empty-s) => #t)
  (check (set-disjoint? empty-s empty-s) => #t))

;; set-unfold with immediate stop (empty result)
(let1 s (set-unfold (make-equal-comparator)
                    (lambda (x) #t)  ; stop immediately
                    (lambda (x) x)
                    (lambda (x) (+ x 1))
                    0)
  (check (set-empty? s) => #t))

;; set-map on empty set
(let1 empty-s (set (make-equal-comparator))
  (let1 s2 (set-map (make-equal-comparator) (lambda (x) (* x 2)) empty-s)
    (check (set-empty? s2) => #t)))

;; set-map causing duplicates (should collapse)
(let1 s (set (make-equal-comparator) 1 2 3 4)
  (let1 s2 (set-map (make-equal-comparator) (lambda (x) (quotient x 2)) s)
    (check (set-size s2) => 3)))  ; 0, 1, 2

;; set-for-each on empty set (should do nothing)
(let* ((empty-s (set (make-equal-comparator)))
       (count 0))
  (set-for-each (lambda (x) (set! count (+ count 1))) empty-s)
  (check count => 0))

;; set-fold on empty set
(let1 empty-s (set (make-equal-comparator))
  (check (set-fold + 0 empty-s) => 0)
  (check (set-fold cons '() empty-s) => '()))

;; set-partition on empty set
(let1 empty-s (set (make-equal-comparator))
  (let-values (((yes no) (set-partition even? empty-s)))
    (check (set-empty? yes) => #t)
    (check (set-empty? no) => #t)))

;; set-partition all pass/fail
(let1 s (set (make-equal-comparator) 2 4 6)
  (let-values (((yes no) (set-partition even? s)))
    (check (set-size yes) => 3)
    (check (set-empty? no) => #t)))

(let1 s (set (make-equal-comparator) 1 3 5)
  (let-values (((yes no) (set-partition even? s)))
    (check (set-empty? yes) => #t)
    (check (set-size no) => 3)))

;; set-copy on empty set
(let* ((empty-s (set (make-equal-comparator)))
       (copy-s (set-copy empty-s)))
  (check (set-empty? copy-s) => #t))

;; set-member edge cases
(let1 empty-s (set (make-equal-comparator))
  (check (set-member empty-s 1 'not-found) => 'not-found))

(let1 s (set (make-equal-comparator) 1 2 3)
  (check (set-member s 2 'not-found) => 2)
  (check (set-member s 99 'default) => 'default))

;; Multiple set comparisons (3+ sets)
(let ((s1 (set (make-equal-comparator) 1))
      (s2 (set (make-equal-comparator) 1 2))
      (s3 (set (make-equal-comparator) 1 2 3)))
  (check (set<? s1 s2 s3) => #t)
  (check (set<? s1 s3 s2) => #f)
  (check (set>? s3 s2 s1) => #t)
  (check (set<=? s1 s2 s3) => #t)
  (check (set<=? s1 s1 s2) => #t)
  (check (set>=? s3 s2 s1) => #t)
  (check (set=? s1 s1 s1) => #t))

;; set-replace on empty set
(let1 empty-s (set (make-equal-comparator))
  (let1 s2 (set-replace empty-s 1)
    (check (set-empty? s2) => #t)))

;; set-search! on empty set
(let1 empty-s (set (make-equal-comparator))
  (let-values (((new-set obj) 
                (set-search! empty-s 1
                  (lambda (insert ignore) (insert 'inserted))
                  (lambda (elem update remove) (remove 'removed)))))
    (check (set-contains? new-set 1) => #t)
    (check obj => 'inserted)))

;; set-adjoin multiple same elements (should still be 1)
(let1 s (set (make-equal-comparator))
  (let1 s2 (set-adjoin s 1 1 1 1 1)
    (check (set-size s2) => 1)))

;; set-delete multiple times
(let1 s (set (make-equal-comparator) 1 2 3)
  (let1 s2 (set-delete s 1 1 1)  ; deleting same element multiple times
    (check (set-size s2) => 2)
    (check (set-contains? s2 1) => #f)))

;; set-xor with identical sets
(let1 s (set (make-equal-comparator) 1 2 3)
  (check (set-empty? (set-xor s s)) => #t))

;; list->set with duplicates (should remove them)
(let1 s (list->set (make-equal-comparator) '(1 1 2 2 3 3 3))
  (check (set-size s) => 3))

;; set-union/intersection/difference with multiple sets
(let ((s1 (set (make-equal-comparator) 1 2))
      (s2 (set (make-equal-comparator) 2 3))
      (s3 (set (make-equal-comparator) 3 4)))
  (check (set-size (set-union s1 s2 s3)) => 4)
  (check (set-size (set-intersection s1 s2)) => 1)
  (check (set-contains? (set-intersection s1 s2) 2) => #t)
  (check (set-size (set-difference s1 s2 s3)) => 1)  ; {1}
  (check (set-contains? (set-difference s1 s2 s3) 1) => #t))

;;; ----- Bag Edge Cases -----

;; Empty bag operations
(let1 empty-b (bag (make-equal-comparator))
  (check (bag-size empty-b) => 0)
  (check (bag-unique-size empty-b) => 0)
  (check (bag-empty? empty-b) => #t)
  (check (bag-contains? empty-b 1) => #f)
  (check (bag-element-count empty-b 1) => 0)
  (check (bag->list empty-b) => '())
  (check (bag->alist empty-b) => '())
  (check (bag-count (lambda (x) #t) empty-b) => 0)
  (check (bag-any? (lambda (x) #t) empty-b) => #f)
  (check (bag-every? (lambda (x) #t) empty-b) => #t))

;; Empty bag theory operations
(let ((empty-b (bag (make-equal-comparator)))
      (b (bag (make-equal-comparator) 1 1 2)))
  (check (bag-size (bag-union empty-b b)) => 3)
  (check (bag-size (bag-intersection empty-b b)) => 0)
  (check (bag-size (bag-difference b empty-b)) => 3)
  (check (bag-size (bag-difference empty-b b)) => 0)
  (check (bag-size (bag-sum empty-b b)) => 3))

;; Single element bag
(let1 single-b (bag (make-equal-comparator) 42)
  (check (bag-size single-b) => 1)
  (check (bag-unique-size single-b) => 1)
  (check (bag-element-count single-b 42) => 1))

;; Self operations
(let1 b (bag (make-equal-comparator) 1 1 2)
  (check (bag=? b b) => #t)
  (check (bag<? b b) => #f)
  (check (bag<=? b b) => #t)
  (check (bag-size (bag-union b b)) => 3)  ; max(2,2)=2, max(1,1)=1
  (check (bag-size (bag-intersection b b)) => 3)
  (check (bag-size (bag-difference b b)) => 0)
  (check (bag-size (bag-xor b b)) => 0)
  (check (bag-size (bag-sum b b)) => 6))  ; sum doubles

;; bag-decrement to zero
(let1 b (bag (make-equal-comparator) 1 1)
  (let1 b2 (bag-decrement! b 1 2)
    (check (bag-element-count b2 1) => 0)
    (check (bag-contains? b2 1) => #f)))

;; bag-decrement beyond zero (should clamp at 0)
(let1 b (bag (make-equal-comparator) 1 1)
  (let1 b2 (bag-decrement! b 1 100)
    (check (bag-element-count b2 1) => 0)))

;; bag-increment non-existent element
(let1 b (bag (make-equal-comparator) 1)
  (let1 b2 (bag-increment! b 999 5)
    (check (bag-element-count b2 999) => 5)))

;; bag-product with 0
(let1 b (bag (make-equal-comparator) 1 2 2)
  (let1 b2 (bag-product 0 b)
    (check (bag-empty? b2) => #t)))

;; bag-product with 1 (identity)
(let1 b (bag (make-equal-comparator) 1 2 2)
  (let1 b2 (bag-product 1 b)
    (check (bag=? b b2) => #t)))

;; alist->bag with zero count (should be ignored)
(let1 b (alist->bag (make-equal-comparator) '((a . 2) (b . 0) (c . 3)))
  (check (bag-element-count b 'a) => 2)
  (check (bag-element-count b 'b) => 0)  ; zero count = not present
  (check (bag-element-count b 'c) => 3)
  (check (bag-unique-size b) => 2))  ; only a and c

;; Empty list->bag
(let1 b (list->bag (make-equal-comparator) '())
  (check (bag-empty? b) => #t))

;; bag-delete non-existent element
(let1 b (bag (make-equal-comparator) 1 2)
  (let1 b2 (bag-delete b 999)
    (check (bag-size b2) => 2)))

;; bag-delete-all with empty list
(let1 b (bag (make-equal-comparator) 1 1 2)
  (check (bag-size (bag-delete-all b '())) => 3))

;; bag->set and set->bag roundtrip
(let1 b (bag (make-equal-comparator) 1 1 2 2 2 3)
  (let* ((s (bag->set b))
         (b2 (set->bag s)))
    (check (set-size s) => 3)
    (check (bag-unique-size b2) => 3)
    (check (bag-element-count b2 1) => 1)  ; counts reset to 1
    (check (bag-element-count b2 2) => 1)))

;; Single argument bag comparisons
(let1 b (bag (make-equal-comparator) 1 2)
  (check (bag=? b) => #t)
  (check (bag<? b) => #t)
  (check (bag>? b) => #t)
  (check (bag<=? b) => #t)
  (check (bag>=? b) => #t))

;; bag-disjoint? with empty bag
(let ((empty-b (bag (make-equal-comparator)))
      (b (bag (make-equal-comparator) 1 2)))
  (check (bag-disjoint? empty-b b) => #t)
  (check (bag-disjoint? b empty-b) => #t)
  (check (bag-disjoint? empty-b empty-b) => #t))

;; set-comparator basic test
(check (comparator? set-comparator) => #t)
(let ((s1 (set (make-equal-comparator) 1 2))
      (s2 (set (make-equal-comparator) 1 2))
      (s3 (set (make-equal-comparator) 1 2 3)))
  (check ((comparator-equality-predicate set-comparator) s1 s2) => #t)
  (check ((comparator-equality-predicate set-comparator) s1 s3) => #f))

;;; ============================================================
;;; Advanced Tests: Different Types, Performance, Error Cases
;;; ============================================================

;;; ----- Different Element Types -----

;; Set with strings
(let1 s (set (make-equal-comparator) "apple" "banana" "cherry")
  (check (set-size s) => 3)
  (check (set-contains? s "banana") => #t)
  (check (set-contains? s "date") => #f))

;; Set with symbols
(let1 s (set (make-eq-comparator) 'foo 'bar 'baz)
  (check (set-size s) => 3)
  (check (set-contains? s 'bar) => #t))

;; Set with mixed numbers (integers and reals)
(let1 s (set (make-equal-comparator) 1 2.5 3 4.7 5)
  (check (set-size s) => 5)
  (check (set-contains? s 2.5) => #t))

;; Set with pairs/lists
(let1 s (set (make-equal-comparator) '(1 2) '(3 4) '(5 6))
  (check (set-size s) => 3)
  (check (set-contains? s '(3 4)) => #t))

;; Bag with strings (with duplicates)
(let1 b (bag (make-equal-comparator) "a" "a" "b" "b" "b" "c")
  (check (bag-size b) => 6)
  (check (bag-element-count b "a") => 2)
  (check (bag-element-count b "b") => 3))

;; Bag with symbols
(let1 b (bag (make-eq-comparator) 'x 'x 'y 'z 'z 'z)
  (check (bag-unique-size b) => 3)
  (check (bag-element-count b 'x) => 2)
  (check (bag-element-count b 'z) => 3))

;; Set operations with string elements
(let ((s1 (set (make-equal-comparator) "a" "b" "c"))
      (s2 (set (make-equal-comparator) "b" "c" "d")))
  (check (set-size (set-union s1 s2)) => 4)
  (check (set-size (set-intersection s1 s2)) => 2)
  (check (set-contains? (set-difference s1 s2) "a") => #t))

;; Bag operations with symbol elements
(let ((b1 (bag (make-eq-comparator) 'a 'a 'b))
      (b2 (bag (make-eq-comparator) 'b 'b 'c)))
  (check (bag-element-count (bag-union b1 b2) 'a) => 2)
  (check (bag-element-count (bag-union b1 b2) 'b) => 2)  ; max(1,2)
  (check (bag-element-count (bag-sum b1 b2) 'b) => 3))   ; 1+2

;;; ----- Performance Tests (Large Sets/Bags) -----

;; Large set creation and operations
(let1 large-s (apply set (make-equal-comparator) (iota 1000))
  (check (set-size large-s) => 1000)
  (check (set-contains? large-s 500) => #t)
  (check (set-contains? large-s 1000) => #f)
  ;; Test operations on large set
  (let1 filtered (set-filter (lambda (x) (< x 100)) large-s)
    (check (set-size filtered) => 100))
  (let1 sum (set-fold + 0 large-s)
    (check sum => 499500)))  ; sum of 0..999

;; Large bag with many duplicates
(let1 large-b (apply bag (make-equal-comparator) 
                    (append (iota 100) (iota 100) (iota 100)))
  (check (bag-size large-b) => 300)
  (check (bag-unique-size large-b) => 100)
  (check (bag-element-count large-b 50) => 3))

;; Large set union
(let ((s1 (apply set (make-equal-comparator) (iota 500)))       ; 0..499
      (s2 (apply set (make-equal-comparator) (iota 500 400))))  ; 400..899
  (let1 union-s (set-union s1 s2)
    (check (set-size union-s) => 900)))

;; Large set intersection
(let ((s1 (apply set (make-equal-comparator) (iota 500 100)))  ; 100..599
      (s2 (apply set (make-equal-comparator) (iota 200))))     ; 0..199
  (let1 inter-s (set-intersection s1 s2)
    (check (set-size inter-s) => 100)))  ; 100..199 overlap

;; Large bag operations
(let ((b1 (apply bag (make-equal-comparator) (append (iota 50) (iota 50))))  ; 2x (0..49) = 100 elements
      (b2 (apply bag (make-equal-comparator) (iota 50 25))))  ; 25..74 = 50 elements
  (check (bag-size (bag-sum b1 b2)) => 150))  ; 100 + 50

;; Performance: map over large set
(let* ((large-s (apply set (make-equal-comparator) (iota 500)))
       (mapped-s (set-map (make-equal-comparator) (lambda (x) (* x 2)) large-s)))
  (check (set-size mapped-s) => 500)
  (check (set-contains? mapped-s 998) => #t))

;; Performance: fold over large bag
(let1 large-b (apply bag (make-equal-comparator) (iota 1000))
  (let1 sum (bag-fold + 0 large-b)
    (check sum => 499500)))

;;; ----- Complex Data Types -----

;; Set with nested lists
(let1 s (set (make-equal-comparator) 
             '((a 1) (b 2)) 
             '((c 3) (d 4))
             '((a 1) (b 2)))  ; duplicate
  (check (set-size s) => 2))  ; duplicates removed

;; Bag with vector-like lists
(let1 b (bag (make-equal-comparator)
             '(1 2 3) '(1 2 3) '(4 5 6))
  (check (bag-element-count b '(1 2 3)) => 2)
  (check (bag-element-count b '(4 5 6)) => 1))

;; Set operations with complex elements
(let ((s1 (set (make-equal-comparator) '(a . 1) '(b . 2)))
      (s2 (set (make-equal-comparator) '(b . 2) '(c . 3))))
  (check (set-size (set-union s1 s2)) => 3)
  (check (set-contains? (set-intersection s1 s2) '(b . 2)) => #t))

;; Bag with mixed complex types
(let1 b (bag (make-equal-comparator)
             "string" 'symbol 123 '(a b c) '(a b c))
  (check (bag-unique-size b) => 4)
  (check (bag-element-count b '(a b c)) => 2))

;; Set-map transforming types (numbers to strings)
(let1 s (set (make-equal-comparator) 1 2 3)
  (let1 s2 (set-map (make-equal-comparator) number->string s)
    (check (set-size s2) => 3)
    (check (set-contains? s2 "2") => #t)))

;; Bag-map transforming types
(let1 b (bag (make-equal-comparator) 'a 'a 'b 'b 'b)
  (let1 b2 (bag-map (make-equal-comparator) symbol->string b)
    (check (bag-element-count b2 "a") => 2)
    (check (bag-element-count b2 "b") => 3)))

;; Case-insensitive string comparator test
(let1 ci-comp (make-comparator string? string-ci=? #f string-ci-hash)
  (let1 s (set ci-comp "Apple" "BANANA" "cherry")
    (check (set-size s) => 3)
    (check (set-contains? s "CHERRY") => #t)
    (check (set-contains? s "apple") => #t)))  ; case-insensitive

(check-report)

