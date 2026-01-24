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

(define-library (liii set)
  (import (liii lang)
          (liii list)
          (liii hash-table)
          (srfi srfi-128))
  (export
    ; Goldfish hash-set (OOP style)
    hash-set
    ; SRFI 113 Sets (functional style)
    ; Constructors
    set set-unfold
    ; Predicates
    set? set-contains? set-empty? set-disjoint?
    ; Accessors
    set-member set-element-comparator
    ; Updaters
    set-adjoin set-adjoin!
    set-replace set-replace!
    set-delete set-delete! set-delete-all set-delete-all!
    set-search!
    ; The whole set
    set-size set-find set-count set-any? set-every?
    ; Mapping and folding
    set-map set-for-each set-fold
    set-filter set-filter! set-remove set-remove!
    set-partition set-partition!
    ; Copying and conversion
    set-copy set->list list->set list->set!
    ; Subsets
    set=? set<? set>? set<=? set>=?
    ; Set theory operations
    set-union set-intersection set-difference set-xor
    set-union! set-intersection! set-difference! set-xor!
    ; Comparator
    set-comparator
    )
  (begin

    ;;; ============================================================
    ;;; Goldfish hash-set (OOP style)
    ;;; ============================================================

    (define-case-class hash-set ((data hash-table?))

      ;; Factory methods
      (chained-define (@empty) 
        (hash-set (make-hash-table)))
                   
      ;; Basic operations
      (define (%size) (hash-table-size data))

      (define (%empty?) (hash-table-empty? data))

      (define (%contains element)
        (hash-table-contains? data element))

      ;; Modification operations
      (chained-define (%add-one! element)
        (hash-table-set! data element #t)
        (%this))

      (chained-define (%remove! element)
        (hash-table-delete! data element)
        (%this))

      (chained-define (%add-one element)
        (let ((ht (make-hash-table)))
          (hash-table-for-each
            (lambda (k v) (hash-table-set! ht k v))
            data)
          (hash-table-set! ht element #t)
          (hash-set ht)))

      (chained-define (%remove element)
        (let ((ht (make-hash-table)))
          (hash-table-for-each
            (lambda (k v) (hash-table-set! ht k v))
            data)
          (hash-table-delete! ht element)
          (hash-set ht)))

      (chained-define (%clear!)
        (hash-table-clear! data)
        (%this))

      ) ; end of define-case-class

    ;;; ============================================================
    ;;; SRFI 113 Sets (functional style)
    ;;; ============================================================
    ;;; 
    ;;; Internal representation:
    ;;; A set is a tagged list: ('srfi-113-set comparator . elements)
    ;;; where elements is a list of unique elements determined by
    ;;; the comparator's equality predicate.

    ;; Internal: set tag for type identification
    (define %set-tag 'srfi-113-set)

    ;; Internal constructor
    (define (%make-set comparator elements)
      (cons %set-tag (cons comparator elements)))

    ;; Internal accessors
    (define (%set-comparator s)
      (cadr s))

    (define (%set-elements s)
      (cddr s))

    ;; Internal: check if element exists using comparator's equality
    (define (%member element elements comparator)
      (let ((elem=? (comparator-equality-predicate comparator)))
        (let loop ((elems elements))
          (cond
            ((null? elems) #f)
            ((elem=? element (car elems)) #t)
            (else (loop (cdr elems)))))))

    ;; Internal: find element in list, return the element or #f
    (define (%find-member element elements comparator)
      (let ((elem=? (comparator-equality-predicate comparator)))
        (let loop ((elems elements))
          (cond
            ((null? elems) #f)
            ((elem=? element (car elems)) (car elems))
            (else (loop (cdr elems)))))))

    ;; Internal: adjoin a single element to the list (if not already present)
    (define (%adjoin-one element elements comparator)
      (if (%member element elements comparator)
          elements
          (cons element elements)))

    ;;; ========== Constructors ==========

    ;; (set comparator element ...)
    ;; Returns a newly allocated set containing the given elements
    (define (set comparator . elements)
      (let loop ((elems elements) (result '()))
        (if (null? elems)
            (%make-set comparator result)
            (loop (cdr elems)
                  (%adjoin-one (car elems) result comparator)))))

    ;; (set-unfold comparator stop? mapper successor seed)
    ;; Create a set by unfolding from a seed value
    (define (set-unfold comparator stop? mapper successor seed)
      (let loop ((seed seed) (result '()))
        (if (stop? seed)
            (%make-set comparator result)
            (loop (successor seed)
                  (%adjoin-one (mapper seed) result comparator)))))

    ;;; ========== Predicates ==========

    ;; (set? obj)
    ;; Returns #t if obj is a SRFI 113 set
    (define (set? obj)
      (and (pair? obj)
           (eq? (car obj) %set-tag)
           (pair? (cdr obj))
           (comparator? (cadr obj))))

    ;; (set-contains? set element)
    ;; Returns #t if element is a member of set
    (define (set-contains? s element)
      (%member element (%set-elements s) (%set-comparator s)))

    ;; (set-empty? set)
    ;; Returns #t if set has no elements
    (define (set-empty? s)
      (null? (%set-elements s)))

    ;; (set-disjoint? set1 set2)
    ;; Returns #t if set1 and set2 have no elements in common
    (define (set-disjoint? s1 s2)
      (let ((comparator (%set-comparator s1)))
        (let loop ((elems (%set-elements s1)))
          (cond
            ((null? elems) #t)
            ((%member (car elems) (%set-elements s2) comparator) #f)
            (else (loop (cdr elems)))))))

    ;;; ========== Accessors ==========

    ;; (set-member set element default)
    ;; Returns the element of set equal to element, or default
    (define (set-member s element default)
      (let ((found (%find-member element (%set-elements s) (%set-comparator s))))
        (if found found default)))

    ;; (set-element-comparator set)
    ;; Returns the comparator of set
    (define (set-element-comparator s)
      (%set-comparator s))

    ;;; ========== Updaters ==========

    ;; (set-adjoin set element ...)
    ;; Returns a new set with all elements added
    (define (set-adjoin s . elements)
      (let ((comparator (%set-comparator s)))
        (let loop ((elems elements) (result (%set-elements s)))
          (if (null? elems)
              (%make-set comparator result)
              (loop (cdr elems)
                    (%adjoin-one (car elems) result comparator))))))

    ;; (set-adjoin! set element ...)
    ;; Linear update version: permitted to mutate and return the set argument
    (define (set-adjoin! s . elements)
      (let ((comparator (%set-comparator s)))
        (let loop ((elems elements) (result (%set-elements s)))
          (if (null? elems)
              (begin
                (set-cdr! (cdr s) result)
                s)
              (loop (cdr elems)
                    (%adjoin-one (car elems) result comparator))))))

    ;; (set-replace set element)
    ;; Replace an element equal to element with element itself
    ;; If no such element exists, return set unchanged
    (define (set-replace s element)
      (let* ((comparator (%set-comparator s))
             (elem=? (comparator-equality-predicate comparator)))
        (if (%member element (%set-elements s) comparator)
            ;; Element exists, replace it
            (%make-set comparator
                       (cons element
                             (filter (lambda (x) (not (elem=? x element)))
                                     (%set-elements s))))
            ;; Element doesn't exist, return unchanged
            s)))

    ;; (set-replace! set element)
    ;; Linear update version: permitted to mutate and return the set argument
    (define (set-replace! s element)
      (let* ((comparator (%set-comparator s))
             (elem=? (comparator-equality-predicate comparator)))
        (if (%member element (%set-elements s) comparator)
            ;; Element exists, replace it in place
            (begin
              (set-cdr! (cdr s)
                        (cons element
                              (filter (lambda (x) (not (elem=? x element)))
                                      (%set-elements s))))
              s)
            ;; Element doesn't exist, return unchanged
            s)))

    ;; (set-delete set element ...)
    ;; Returns a new set with elements removed
    (define (set-delete s . elements)
      (let* ((comparator (%set-comparator s))
             (elem=? (comparator-equality-predicate comparator)))
        (let ((new-elements
               (fold (lambda (to-remove result)
                       (filter (lambda (x) (not (elem=? x to-remove))) result))
                     (%set-elements s)
                     elements)))
          (%make-set comparator new-elements))))

    ;; (set-delete! set element ...)
    ;; Linear update version: permitted to mutate and return the set argument
    (define (set-delete! s . elements)
      (let* ((comparator (%set-comparator s))
             (elem=? (comparator-equality-predicate comparator)))
        (let ((new-elements
               (fold (lambda (to-remove result)
                       (filter (lambda (x) (not (elem=? x to-remove))) result))
                     (%set-elements s)
                     elements)))
          ;; Mutate the set in place by updating the elements
          (set-cdr! (cdr s) new-elements)
          s)))

    ;; (set-delete-all set element-list)
    (define (set-delete-all s element-list)
      (apply set-delete s element-list))

    ;; (set-delete-all! set element-list)
    ;; Linear update version: permitted to mutate and return the set argument
    (define (set-delete-all! s element-list)
      (apply set-delete! s element-list))

    ;; (set-search! set element failure success)
    ;; Search for element in set, calling failure or success continuation
    ;; Returns two values: the (possibly updated) set and an additional value
    ;; Linear update version: permitted to mutate and return the set argument
    (define (set-search! s element failure success)
      (let* ((comparator (%set-comparator s))
             (elem=? (comparator-equality-predicate comparator))
             (found (%find-member element (%set-elements s) comparator)))
        (if found
            ;; Element found: call success with element and update/remove continuations
            (success found
                     ;; update continuation: (update new-element obj)
                     ;; Mutates the set in place
                     (lambda (new-element obj)
                       (let ((new-elements
                              (cons new-element
                                    (filter (lambda (x) (not (elem=? x found)))
                                            (%set-elements s)))))
                         (set-cdr! (cdr s) new-elements)
                         (values s obj)))
                     ;; remove continuation: (remove obj)
                     ;; Mutates the set in place
                     (lambda (obj)
                       (let ((new-elements
                              (filter (lambda (x) (not (elem=? x found)))
                                      (%set-elements s))))
                         (set-cdr! (cdr s) new-elements)
                         (values s obj))))
            ;; Element not found: call failure with insert/ignore continuations
            (failure
             ;; insert continuation: (insert obj)
             ;; Mutates the set in place
             (lambda (obj)
               (set-cdr! (cdr s) (cons element (%set-elements s)))
               (values s obj))
             ;; ignore continuation: (ignore obj)
             (lambda (obj)
               (values s obj))))))

    ;;; ========== The whole set ==========

    ;; (set-size set)
    ;; Returns the number of elements
    (define (set-size s)
      (length (%set-elements s)))

    ;; (set-find predicate set failure)
    ;; Returns an element satisfying predicate, or (failure)
    (define (set-find predicate s failure)
      (let loop ((elems (%set-elements s)))
        (cond
          ((null? elems) (failure))
          ((predicate (car elems)) (car elems))
          (else (loop (cdr elems))))))

    ;; (set-count predicate set)
    ;; Returns count of elements satisfying predicate
    (define (set-count predicate s)
      (fold (lambda (elem count)
              (if (predicate elem) (+ count 1) count))
            0
            (%set-elements s)))

    ;; (set-any? predicate set)
    (define (set-any? predicate s)
      (let loop ((elems (%set-elements s)))
        (cond
          ((null? elems) #f)
          ((predicate (car elems)) #t)
          (else (loop (cdr elems))))))

    ;; (set-every? predicate set)
    (define (set-every? predicate s)
      (let loop ((elems (%set-elements s)))
        (cond
          ((null? elems) #t)
          ((not (predicate (car elems))) #f)
          (else (loop (cdr elems))))))

    ;;; ========== Mapping and folding ==========

    ;; (set-map comparator proc set)
    (define (set-map comparator proc s)
      (let loop ((elems (%set-elements s)) (result '()))
        (if (null? elems)
            (%make-set comparator result)
            (loop (cdr elems)
                  (%adjoin-one (proc (car elems)) result comparator)))))

    ;; (set-for-each proc set)
    (define (set-for-each proc s)
      (for-each proc (%set-elements s)))

    ;; (set-fold proc nil set)
    (define (set-fold proc nil s)
      (fold proc nil (%set-elements s)))

    ;; (set-filter predicate set)
    (define (set-filter predicate s)
      (%make-set (%set-comparator s)
                 (filter predicate (%set-elements s))))

    ;; (set-filter! predicate set)
    ;; Linear update version: permitted to mutate and return the set argument
    (define (set-filter! predicate s)
      (set-cdr! (cdr s) (filter predicate (%set-elements s)))
      s)

    ;; (set-remove predicate set)
    (define (set-remove predicate s)
      (%make-set (%set-comparator s)
                 (filter (lambda (x) (not (predicate x))) (%set-elements s))))

    ;; (set-remove! predicate set)
    ;; Linear update version: permitted to mutate and return the set argument
    (define (set-remove! predicate s)
      (set-cdr! (cdr s)
                (filter (lambda (x) (not (predicate x))) (%set-elements s)))
      s)

    ;; (set-partition predicate set)
    ;; Returns two values: elements satisfying and not satisfying predicate
    (define (set-partition predicate s)
      (let ((comparator (%set-comparator s)))
        (let loop ((elems (%set-elements s)) (yes '()) (no '()))
          (cond
            ((null? elems)
             (values (%make-set comparator yes)
                     (%make-set comparator no)))
            ((predicate (car elems))
             (loop (cdr elems) (cons (car elems) yes) no))
            (else
             (loop (cdr elems) yes (cons (car elems) no)))))))

    ;; (set-partition! predicate set)
    ;; Linear update version: permitted to mutate and return the set argument
    ;; Returns two sets: first is mutated from s, second is newly allocated
    (define (set-partition! predicate s)
      (let ((comparator (%set-comparator s)))
        (let loop ((elems (%set-elements s)) (yes '()) (no '()))
          (cond
            ((null? elems)
             ;; Mutate s to contain elements satisfying predicate
             (set-cdr! (cdr s) yes)
             (values s (%make-set comparator no)))
            ((predicate (car elems))
             (loop (cdr elems) (cons (car elems) yes) no))
            (else
             (loop (cdr elems) yes (cons (car elems) no)))))))

    ;;; ========== Copying and conversion ==========

    ;; (set-copy set)
    (define (set-copy s)
      (%make-set (%set-comparator s)
                 (list-copy (%set-elements s))))

    ;; (set->list set)
    (define (set->list s)
      (list-copy (%set-elements s)))

    ;; (list->set comparator list)
    (define (list->set comparator lst)
      (apply set comparator lst))

    ;; (list->set! set list)
    (define (list->set! s lst)
      (apply set-adjoin! s lst))

    ;;; ========== Subsets ==========

    ;; (set=? set1 set2 ...)
    (define (set=? s1 . sets)
      (if (null? sets)
          #t
          (and (= (set-size s1) (set-size (car sets)))
               (let loop ((elems (%set-elements s1)))
                 (cond
                   ((null? elems) #t)
                   ((not (set-contains? (car sets) (car elems))) #f)
                   (else (loop (cdr elems)))))
               (apply set=? sets))))

    ;; (set<? set1 set2 ...)
    ;; Each set is a proper subset of the next
    (define (set<? s1 . sets)
      (if (null? sets)
          #t
          (and (< (set-size s1) (set-size (car sets)))
               (let loop ((elems (%set-elements s1)))
                 (cond
                   ((null? elems) #t)
                   ((not (set-contains? (car sets) (car elems))) #f)
                   (else (loop (cdr elems)))))
               (apply set<? sets))))

    ;; (set>? set1 set2 ...)
    (define (set>? s1 . sets)
      (if (null? sets)
          #t
          (and (> (set-size s1) (set-size (car sets)))
               (let loop ((elems (%set-elements (car sets))))
                 (cond
                   ((null? elems) #t)
                   ((not (set-contains? s1 (car elems))) #f)
                   (else (loop (cdr elems)))))
               (apply set>? sets))))

    ;; (set<=? set1 set2 ...)
    (define (set<=? s1 . sets)
      (if (null? sets)
          #t
          (and (let loop ((elems (%set-elements s1)))
                 (cond
                   ((null? elems) #t)
                   ((not (set-contains? (car sets) (car elems))) #f)
                   (else (loop (cdr elems)))))
               (apply set<=? sets))))

    ;; (set>=? set1 set2 ...)
    (define (set>=? s1 . sets)
      (if (null? sets)
          #t
          (and (let loop ((elems (%set-elements (car sets))))
                 (cond
                   ((null? elems) #t)
                   ((not (set-contains? s1 (car elems))) #f)
                   (else (loop (cdr elems)))))
               (apply set>=? sets))))

    ;;; ========== Set theory operations ==========

    ;; (set-union set1 set2 ...)
    (define (set-union s1 . sets)
      (let ((comparator (%set-comparator s1)))
        (fold (lambda (s result)
                (set-fold (lambda (elem acc)
                            (set-adjoin acc elem))
                          result
                          s))
              s1
              sets)))

    ;; (set-union! set1 set2 ...)
    ;; Linear update version: permitted to mutate and return set1
    (define (set-union! s1 . sets)
      (let ((comparator (%set-comparator s1)))
        (let ((new-elements
               (fold (lambda (s result)
                       (set-fold (lambda (elem acc)
                                   (%adjoin-one elem acc comparator))
                                 result
                                 s))
                     (%set-elements s1)
                     sets)))
          (set-cdr! (cdr s1) new-elements)
          s1)))

    ;; (set-intersection set1 set2 ...)
    (define (set-intersection s1 . sets)
      (if (null? sets)
          (set-copy s1)
          (set-filter (lambda (elem)
                        (every (lambda (s) (set-contains? s elem)) sets))
                      s1)))

    ;; (set-intersection! set1 set2 ...)
    ;; Linear update version: permitted to mutate and return set1
    (define (set-intersection! s1 . sets)
      (if (null? sets)
          s1
          (begin
            (set-cdr! (cdr s1)
                      (filter (lambda (elem)
                                (every (lambda (s) (set-contains? s elem)) sets))
                              (%set-elements s1)))
            s1)))

    ;; (set-difference set1 set2 ...)
    (define (set-difference s1 . sets)
      (if (null? sets)
          (set-copy s1)
          (set-filter (lambda (elem)
                        (not (any (lambda (s) (set-contains? s elem)) sets)))
                      s1)))

    ;; (set-difference! set1 set2 ...)
    ;; Linear update version: permitted to mutate and return set1
    (define (set-difference! s1 . sets)
      (if (null? sets)
          s1
          (begin
            (set-cdr! (cdr s1)
                      (filter (lambda (elem)
                                (not (any (lambda (s) (set-contains? s elem)) sets)))
                              (%set-elements s1)))
            s1)))

    ;; (set-xor set1 set2)
    (define (set-xor s1 s2)
      (set-union (set-difference s1 s2)
                 (set-difference s2 s1)))

    ;; (set-xor! set1 set2)
    ;; Linear update version: permitted to mutate and return set1
    (define (set-xor! s1 s2)
      (let* ((comparator (%set-comparator s1))
             (diff1 (filter (lambda (elem) (not (set-contains? s2 elem)))
                            (%set-elements s1)))
             (diff2 (filter (lambda (elem) (not (set-contains? s1 elem)))
                            (%set-elements s2))))
        ;; Note: compute diff2 before mutating s1
        (set-cdr! (cdr s1)
                  (fold (lambda (elem acc) (%adjoin-one elem acc comparator))
                        diff1
                        diff2))
        s1))

    ;;; ========== Comparator ==========

    ;; set-comparator
    ;; A comparator for comparing sets
    ;; Note: Sets don't have a natural ordering, so only equality is provided
    (define set-comparator
      (make-comparator
       set?                                    ; type test
       (lambda (s1 s2) (set=? s1 s2))         ; equality
       #f                                      ; no ordering
       (lambda (s)                             ; hash function
         (set-fold (lambda (elem acc)
                     (+ acc (comparator-hash (set-element-comparator s) elem)))
                   0
                   s))))

    ) ; end of begin
  ) ; end of define-library

