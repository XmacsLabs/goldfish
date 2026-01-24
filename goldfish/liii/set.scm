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
          (liii alist)
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
    ; SRFI 113 Bags (functional style)
    ; Constructors
    bag bag-unfold
    ; Predicates
    bag? bag-contains? bag-empty? bag-disjoint?
    ; Accessors
    bag-member bag-element-comparator
    ; Updaters
    bag-adjoin bag-adjoin!
    bag-replace bag-replace!
    bag-delete bag-delete! bag-delete-all bag-delete-all!
    bag-search!
    ; The whole bag
    bag-size bag-find bag-count bag-any? bag-every?
    ; Mapping and folding
    bag-map bag-for-each bag-fold
    bag-filter bag-filter! bag-remove bag-remove!
    bag-partition bag-partition!
    ; Copying and conversion
    bag-copy bag->list list->bag list->bag!
    ; Subbags
    bag=? bag<? bag>? bag<=? bag>=?
    ; Bag theory operations
    bag-union bag-intersection bag-difference bag-xor
    bag-union! bag-intersection! bag-difference! bag-xor!
    ; Additional bag procedures
    bag-sum bag-sum! bag-product bag-product!
    bag-unique-size bag-element-count
    bag-for-each-unique bag-fold-unique
    bag-increment! bag-decrement!
    bag->set set->bag set->bag!
    bag->alist alist->bag
    ; Comparator
    bag-comparator
    )
  (begin


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

    ;;; ============================================================
    ;;; SRFI 113 Bags (functional style)
    ;;; ============================================================
    ;;;
    ;;; Bags are like sets but can contain duplicate elements.
    ;;; Internal representation:
    ;;; A bag is a tagged list: ('srfi-113-bag comparator . alist)
    ;;; where alist is an association list of (element . count) pairs.

    ;; Internal: bag tag for type identification
    (define %bag-tag 'srfi-113-bag)

    ;; Internal constructor
    (define (%make-bag comparator alist)
      (cons %bag-tag (cons comparator alist)))

    ;; Internal accessors
    (define (%bag-comparator b)
      (cadr b))

    (define (%bag-alist b)
      (cddr b))

    ;; Internal: get count of element in alist
    (define (%bag-get-count element alist comparator)
      (let ((elem=? (comparator-equality-predicate comparator)))
        (let loop ((lst alist))
          (cond
            ((null? lst) 0)
            ((elem=? element (caar lst)) (cdar lst))
            (else (loop (cdr lst)))))))

    ;; Internal: set count of element in alist (returns new alist)
    (define (%bag-set-count element count alist comparator)
      (let ((elem=? (comparator-equality-predicate comparator)))
        (if (<= count 0)
            ;; Remove element
            (filter (lambda (pair) (not (elem=? element (car pair)))) alist)
            ;; Update or add element
            (let loop ((lst alist) (found #f) (result '()))
              (cond
                ((null? lst)
                 (if found
                     (reverse result)
                     (reverse (cons (cons element count) result))))
                ((elem=? element (caar lst))
                 (loop (cdr lst) #t (cons (cons element count) result)))
                (else
                 (loop (cdr lst) found (cons (car lst) result))))))))

    ;; Internal: increment count of element
    (define (%bag-increment element n alist comparator)
      (let ((current (%bag-get-count element alist comparator)))
        (%bag-set-count element (+ current n) alist comparator)))

    ;; Internal: decrement count of element (but not below 0)
    (define (%bag-decrement element n alist comparator)
      (let ((current (%bag-get-count element alist comparator)))
        (%bag-set-count element (max 0 (- current n)) alist comparator)))

    ;;; ========== Bag Constructors ==========

    ;; (bag comparator element ...)
    ;; Returns a newly allocated bag containing the given elements
    (define (bag comparator . elements)
      (let loop ((elems elements) (alist '()))
        (if (null? elems)
            (%make-bag comparator alist)
            (loop (cdr elems)
                  (%bag-increment (car elems) 1 alist comparator)))))

    ;; (bag-unfold comparator stop? mapper successor seed)
    ;; Create a bag by unfolding from a seed value
    (define (bag-unfold comparator stop? mapper successor seed)
      (let loop ((seed seed) (alist '()))
        (if (stop? seed)
            (%make-bag comparator alist)
            (loop (successor seed)
                  (%bag-increment (mapper seed) 1 alist comparator)))))

    ;;; ========== Bag Predicates ==========

    ;; (bag? obj)
    ;; Returns #t if obj is a SRFI 113 bag
    (define (bag? obj)
      (and (pair? obj)
           (eq? (car obj) %bag-tag)
           (pair? (cdr obj))
           (comparator? (cadr obj))))

    ;; (bag-contains? bag element)
    ;; Returns #t if element is a member of bag
    (define (bag-contains? b element)
      (> (%bag-get-count element (%bag-alist b) (%bag-comparator b)) 0))

    ;; (bag-empty? bag)
    ;; Returns #t if bag has no elements
    (define (bag-empty? b)
      (null? (%bag-alist b)))

    ;; (bag-disjoint? bag1 bag2)
    ;; Returns #t if bag1 and bag2 have no elements in common
    (define (bag-disjoint? b1 b2)
      (let ((comparator (%bag-comparator b1)))
        (let loop ((alist (%bag-alist b1)))
          (cond
            ((null? alist) #t)
            ((bag-contains? b2 (caar alist)) #f)
            (else (loop (cdr alist)))))))

    ;;; ========== Bag Accessors ==========

    ;; (bag-member bag element default)
    ;; Returns the element of bag equal to element, or default
    (define (bag-member b element default)
      (let ((elem=? (comparator-equality-predicate (%bag-comparator b))))
        (let loop ((alist (%bag-alist b)))
          (cond
            ((null? alist) default)
            ((elem=? element (caar alist)) (caar alist))
            (else (loop (cdr alist)))))))

    ;; (bag-element-comparator bag)
    ;; Returns the comparator of bag
    (define (bag-element-comparator b)
      (%bag-comparator b))

    ;;; ========== Bag Updaters ==========

    ;; (bag-adjoin bag element ...)
    ;; Returns a new bag with all elements added (increments count)
    (define (bag-adjoin b . elements)
      (let ((comparator (%bag-comparator b)))
        (let loop ((elems elements) (alist (%bag-alist b)))
          (if (null? elems)
              (%make-bag comparator alist)
              (loop (cdr elems)
                    (%bag-increment (car elems) 1 alist comparator))))))

    ;; (bag-adjoin! bag element ...)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-adjoin! b . elements)
      (let ((comparator (%bag-comparator b)))
        (let loop ((elems elements) (alist (%bag-alist b)))
          (if (null? elems)
              (begin
                (set-cdr! (cdr b) alist)
                b)
              (loop (cdr elems)
                    (%bag-increment (car elems) 1 alist comparator))))))

    ;; (bag-replace bag element)
    ;; Replace an element equal to element with element itself
    (define (bag-replace b element)
      (let* ((comparator (%bag-comparator b))
             (count (%bag-get-count element (%bag-alist b) comparator)))
        (if (> count 0)
            (%make-bag comparator
                       (%bag-set-count element count (%bag-alist b) comparator))
            b)))

    ;; (bag-replace! bag element)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-replace! b element)
      (let* ((comparator (%bag-comparator b))
             (count (%bag-get-count element (%bag-alist b) comparator)))
        (when (> count 0)
          (set-cdr! (cdr b)
                    (%bag-set-count element count (%bag-alist b) comparator)))
        b))

    ;; (bag-delete bag element ...)
    ;; Returns a new bag with one occurrence of each element removed
    (define (bag-delete b . elements)
      (let ((comparator (%bag-comparator b)))
        (let loop ((elems elements) (alist (%bag-alist b)))
          (if (null? elems)
              (%make-bag comparator alist)
              (loop (cdr elems)
                    (%bag-decrement (car elems) 1 alist comparator))))))

    ;; (bag-delete! bag element ...)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-delete! b . elements)
      (let ((comparator (%bag-comparator b)))
        (let loop ((elems elements) (alist (%bag-alist b)))
          (if (null? elems)
              (begin
                (set-cdr! (cdr b) alist)
                b)
              (loop (cdr elems)
                    (%bag-decrement (car elems) 1 alist comparator))))))

    ;; (bag-delete-all bag element-list)
    ;; Remove all occurrences of elements in list
    (define (bag-delete-all b element-list)
      (let ((comparator (%bag-comparator b)))
        (fold (lambda (elem alist)
                (%bag-set-count elem 0 alist comparator))
              (%bag-alist b)
              element-list)
        (%make-bag comparator
                   (fold (lambda (elem alist)
                           (%bag-set-count elem 0 alist comparator))
                         (%bag-alist b)
                         element-list))))

    ;; (bag-delete-all! bag element-list)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-delete-all! b element-list)
      (let ((comparator (%bag-comparator b)))
        (set-cdr! (cdr b)
                  (fold (lambda (elem alist)
                          (%bag-set-count elem 0 alist comparator))
                        (%bag-alist b)
                        element-list))
        b))

    ;; (bag-search! bag element failure success)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-search! b element failure success)
      (let* ((comparator (%bag-comparator b))
             (count (%bag-get-count element (%bag-alist b) comparator)))
        (if (> count 0)
            ;; Element found
            (success (bag-member b element #f)
                     ;; update continuation: mutates in place
                     (lambda (new-element obj)
                       (set-cdr! (cdr b)
                                 (%bag-set-count new-element count
                                                 (%bag-set-count element 0 (%bag-alist b) comparator)
                                                 comparator))
                       (values b obj))
                     ;; remove continuation: mutates in place
                     (lambda (obj)
                       (set-cdr! (cdr b)
                                 (%bag-set-count element 0 (%bag-alist b) comparator))
                       (values b obj)))
            ;; Element not found
            (failure
             ;; insert continuation: mutates in place
             (lambda (obj)
               (set-cdr! (cdr b)
                         (%bag-increment element 1 (%bag-alist b) comparator))
               (values b obj))
             ;; ignore continuation
             (lambda (obj)
               (values b obj))))))

    ;;; ========== The whole bag ==========

    ;; (bag-size bag)
    ;; Returns total count of all elements
    (define (bag-size b)
      (fold (lambda (pair acc) (+ acc (cdr pair))) 0 (%bag-alist b)))

    ;; (bag-find predicate bag failure)
    (define (bag-find predicate b failure)
      (let loop ((alist (%bag-alist b)))
        (cond
          ((null? alist) (failure))
          ((predicate (caar alist)) (caar alist))
          (else (loop (cdr alist))))))

    ;; (bag-count predicate bag)
    (define (bag-count predicate b)
      (fold (lambda (pair acc)
              (if (predicate (car pair)) (+ acc (cdr pair)) acc))
            0
            (%bag-alist b)))

    ;; (bag-any? predicate bag)
    (define (bag-any? predicate b)
      (let loop ((alist (%bag-alist b)))
        (cond
          ((null? alist) #f)
          ((predicate (caar alist)) #t)
          (else (loop (cdr alist))))))

    ;; (bag-every? predicate bag)
    (define (bag-every? predicate b)
      (let loop ((alist (%bag-alist b)))
        (cond
          ((null? alist) #t)
          ((not (predicate (caar alist))) #f)
          (else (loop (cdr alist))))))

    ;;; ========== Bag Mapping and folding ==========

    ;; (bag-map comparator proc bag)
    (define (bag-map comparator proc b)
      (let loop ((alist (%bag-alist b)) (result '()))
        (if (null? alist)
            (%make-bag comparator result)
            (let* ((elem (caar alist))
                   (count (cdar alist))
                   (new-elem (proc elem)))
              (loop (cdr alist)
                    (%bag-increment new-elem count result comparator))))))

    ;; (bag-for-each proc bag)
    (define (bag-for-each proc b)
      (for-each (lambda (pair)
                  (let ((elem (car pair))
                        (count (cdr pair)))
                    (do ((i 0 (+ i 1)))
                        ((>= i count))
                      (proc elem))))
                (%bag-alist b)))

    ;; (bag-fold proc nil bag)
    (define (bag-fold proc nil b)
      (fold (lambda (pair acc)
              (let ((elem (car pair))
                    (count (cdr pair)))
                (let loop ((i 0) (result acc))
                  (if (>= i count)
                      result
                      (loop (+ i 1) (proc elem result))))))
            nil
            (%bag-alist b)))

    ;; (bag-filter predicate bag)
    (define (bag-filter predicate b)
      (%make-bag (%bag-comparator b)
                 (filter (lambda (pair) (predicate (car pair)))
                         (%bag-alist b))))

    ;; (bag-filter! predicate bag)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-filter! predicate b)
      (set-cdr! (cdr b)
                (filter (lambda (pair) (predicate (car pair)))
                        (%bag-alist b)))
      b)

    ;; (bag-remove predicate bag)
    (define (bag-remove predicate b)
      (%make-bag (%bag-comparator b)
                 (filter (lambda (pair) (not (predicate (car pair))))
                         (%bag-alist b))))

    ;; (bag-remove! predicate bag)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-remove! predicate b)
      (set-cdr! (cdr b)
                (filter (lambda (pair) (not (predicate (car pair))))
                        (%bag-alist b)))
      b)

    ;; (bag-partition predicate bag)
    (define (bag-partition predicate b)
      (let ((comparator (%bag-comparator b)))
        (let loop ((alist (%bag-alist b)) (yes '()) (no '()))
          (cond
            ((null? alist)
             (values (%make-bag comparator yes)
                     (%make-bag comparator no)))
            ((predicate (caar alist))
             (loop (cdr alist) (cons (car alist) yes) no))
            (else
             (loop (cdr alist) yes (cons (car alist) no)))))))

    ;; (bag-partition! predicate bag)
    ;; Linear update version: permitted to mutate and return the bag argument
    ;; Returns two bags: first is mutated from b, second is newly allocated
    (define (bag-partition! predicate b)
      (let ((comparator (%bag-comparator b)))
        (let loop ((alist (%bag-alist b)) (yes '()) (no '()))
          (cond
            ((null? alist)
             ;; Mutate b to contain elements satisfying predicate
             (set-cdr! (cdr b) yes)
             (values b (%make-bag comparator no)))
            ((predicate (caar alist))
             (loop (cdr alist) (cons (car alist) yes) no))
            (else
             (loop (cdr alist) yes (cons (car alist) no)))))))

    ;;; ========== Bag Copying and conversion ==========

    ;; (bag-copy bag)
    (define (bag-copy b)
      (%make-bag (%bag-comparator b)
                 (map (lambda (pair) (cons (car pair) (cdr pair)))
                      (%bag-alist b))))

    ;; (bag->list bag)
    ;; Returns list with elements repeated by count
    (define (bag->list b)
      (fold (lambda (pair acc)
              (let ((elem (car pair))
                    (count (cdr pair)))
                (let loop ((i 0) (result acc))
                  (if (>= i count)
                      result
                      (loop (+ i 1) (cons elem result))))))
            '()
            (%bag-alist b)))

    ;; (list->bag comparator list)
    (define (list->bag comparator lst)
      (apply bag comparator lst))

    ;; (list->bag! bag list)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (list->bag! b lst)
      (apply bag-adjoin! b lst))

    ;;; ========== Subbags ==========

    ;; (bag=? bag1 bag2 ...)
    (define (bag=? b1 . bags)
      (if (null? bags)
          #t
          (let ((comparator (%bag-comparator b1)))
            (and (= (bag-unique-size b1) (bag-unique-size (car bags)))
                 (let loop ((alist (%bag-alist b1)))
                   (cond
                     ((null? alist) #t)
                     ((not (= (cdar alist)
                              (%bag-get-count (caar alist) (%bag-alist (car bags)) comparator)))
                      #f)
                     (else (loop (cdr alist)))))
                 (apply bag=? bags)))))

    ;; (bag<? bag1 bag2 ...)
    (define (bag<? b1 . bags)
      (if (null? bags)
          #t
          (let ((comparator (%bag-comparator b1)))
            (and (< (bag-size b1) (bag-size (car bags)))
                 (let loop ((alist (%bag-alist b1)))
                   (cond
                     ((null? alist) #t)
                     ((> (cdar alist)
                         (%bag-get-count (caar alist) (%bag-alist (car bags)) comparator))
                      #f)
                     (else (loop (cdr alist)))))
                 (apply bag<? bags)))))

    ;; (bag>? bag1 bag2 ...)
    (define (bag>? b1 . bags)
      (if (null? bags)
          #t
          (apply bag<? (car bags) b1 (cdr bags))))

    ;; (bag<=? bag1 bag2 ...)
    (define (bag<=? b1 . bags)
      (if (null? bags)
          #t
          (let ((comparator (%bag-comparator b1)))
            (and (let loop ((alist (%bag-alist b1)))
                   (cond
                     ((null? alist) #t)
                     ((> (cdar alist)
                         (%bag-get-count (caar alist) (%bag-alist (car bags)) comparator))
                      #f)
                     (else (loop (cdr alist)))))
                 (apply bag<=? bags)))))

    ;; (bag>=? bag1 bag2 ...)
    (define (bag>=? b1 . bags)
      (if (null? bags)
          #t
          (apply bag<=? (car bags) b1 (cdr bags))))

    ;;; ========== Bag theory operations ==========

    ;; (bag-union bag1 bag2 ...)
    ;; Count = max of counts
    (define (bag-union b1 . bags)
      (let ((comparator (%bag-comparator b1)))
        (fold (lambda (b result)
                (let loop ((alist (%bag-alist b)) (result-alist (%bag-alist result)))
                  (if (null? alist)
                      (%make-bag comparator result-alist)
                      (let* ((elem (caar alist))
                             (count (cdar alist))
                             (current (%bag-get-count elem result-alist comparator)))
                        (loop (cdr alist)
                              (%bag-set-count elem (max count current) result-alist comparator))))))
              b1
              bags)))

    ;; (bag-union! bag1 bag2 ...)
    ;; Linear update version: permitted to mutate and return bag1
    (define (bag-union! b1 . bags)
      (let ((comparator (%bag-comparator b1)))
        (let ((new-alist
               (fold (lambda (b result-alist)
                       (let loop ((alist (%bag-alist b)) (acc result-alist))
                         (if (null? alist)
                             acc
                             (let* ((elem (caar alist))
                                    (count (cdar alist))
                                    (current (%bag-get-count elem acc comparator)))
                               (loop (cdr alist)
                                     (%bag-set-count elem (max count current) acc comparator))))))
                     (%bag-alist b1)
                     bags)))
          (set-cdr! (cdr b1) new-alist)
          b1)))

    ;; (bag-intersection bag1 bag2 ...)
    ;; Count = min of counts
    (define (bag-intersection b1 . bags)
      (if (null? bags)
          (bag-copy b1)
          (let ((comparator (%bag-comparator b1)))
            (let loop ((alist (%bag-alist b1)) (result '()))
              (if (null? alist)
                  (%make-bag comparator result)
                  (let* ((elem (caar alist))
                         (count (cdar alist))
                         (min-count (fold (lambda (b acc)
                                            (min acc (%bag-get-count elem (%bag-alist b) comparator)))
                                          count
                                          bags)))
                    (loop (cdr alist)
                          (if (> min-count 0)
                              (cons (cons elem min-count) result)
                              result))))))))

    ;; (bag-intersection! bag1 bag2 ...)
    ;; Linear update version: permitted to mutate and return bag1
    (define (bag-intersection! b1 . bags)
      (if (null? bags)
          b1
          (let ((comparator (%bag-comparator b1)))
            (let loop ((alist (%bag-alist b1)) (result '()))
              (if (null? alist)
                  (begin
                    (set-cdr! (cdr b1) result)
                    b1)
                  (let* ((elem (caar alist))
                         (count (cdar alist))
                         (min-count (fold (lambda (b acc)
                                            (min acc (%bag-get-count elem (%bag-alist b) comparator)))
                                          count
                                          bags)))
                    (loop (cdr alist)
                          (if (> min-count 0)
                              (cons (cons elem min-count) result)
                              result))))))))

    ;; (bag-difference bag1 bag2 ...)
    ;; Count = count in b1 minus sum of counts in others
    (define (bag-difference b1 . bags)
      (if (null? bags)
          (bag-copy b1)
          (let ((comparator (%bag-comparator b1)))
            (let loop ((alist (%bag-alist b1)) (result '()))
              (if (null? alist)
                  (%make-bag comparator result)
                  (let* ((elem (caar alist))
                         (count (cdar alist))
                         (subtract (fold (lambda (b acc)
                                           (+ acc (%bag-get-count elem (%bag-alist b) comparator)))
                                         0
                                         bags))
                         (new-count (max 0 (- count subtract))))
                    (loop (cdr alist)
                          (if (> new-count 0)
                              (cons (cons elem new-count) result)
                              result))))))))

    ;; (bag-difference! bag1 bag2 ...)
    ;; Linear update version: permitted to mutate and return bag1
    (define (bag-difference! b1 . bags)
      (if (null? bags)
          b1
          (let ((comparator (%bag-comparator b1)))
            (let loop ((alist (%bag-alist b1)) (result '()))
              (if (null? alist)
                  (begin
                    (set-cdr! (cdr b1) result)
                    b1)
                  (let* ((elem (caar alist))
                         (count (cdar alist))
                         (subtract (fold (lambda (b acc)
                                           (+ acc (%bag-get-count elem (%bag-alist b) comparator)))
                                         0
                                         bags))
                         (new-count (max 0 (- count subtract))))
                    (loop (cdr alist)
                          (if (> new-count 0)
                              (cons (cons elem new-count) result)
                              result))))))))

    ;; (bag-xor bag1 bag2)
    ;; Count = |count1 - count2|
    (define (bag-xor b1 b2)
      (let ((comparator (%bag-comparator b1)))
        ;; Collect all unique elements from both bags
        (let* ((all-elems (append (map car (%bag-alist b1))
                                  (filter (lambda (e)
                                            (not (bag-contains? b1 e)))
                                          (map car (%bag-alist b2)))))
               (result-alist
                (fold (lambda (elem alist)
                        (let* ((c1 (%bag-get-count elem (%bag-alist b1) comparator))
                               (c2 (%bag-get-count elem (%bag-alist b2) comparator))
                               (diff (abs (- c1 c2))))
                          (if (> diff 0)
                              (%bag-set-count elem diff alist comparator)
                              alist)))
                      '()
                      all-elems)))
          (%make-bag comparator result-alist))))

    ;; (bag-xor! bag1 bag2)
    ;; Linear update version: permitted to mutate and return bag1
    (define (bag-xor! b1 b2)
      (let ((comparator (%bag-comparator b1)))
        ;; Collect all unique elements from both bags
        (let* ((all-elems (append (map car (%bag-alist b1))
                                  (filter (lambda (e)
                                            (not (bag-contains? b1 e)))
                                          (map car (%bag-alist b2)))))
               (result-alist
                (fold (lambda (elem alist)
                        (let* ((c1 (%bag-get-count elem (%bag-alist b1) comparator))
                               (c2 (%bag-get-count elem (%bag-alist b2) comparator))
                               (diff (abs (- c1 c2))))
                          (if (> diff 0)
                              (%bag-set-count elem diff alist comparator)
                              alist)))
                      '()
                      all-elems)))
          (set-cdr! (cdr b1) result-alist)
          b1)))

    ;;; ========== Additional bag procedures ==========

    ;; (bag-sum bag1 bag2 ...)
    ;; Count = sum of counts
    (define (bag-sum b1 . bags)
      (let ((comparator (%bag-comparator b1)))
        (fold (lambda (b result)
                (bag-fold (lambda (elem acc)
                            (bag-adjoin acc elem))
                          result
                          b))
              b1
              bags)))

    ;; (bag-sum! bag1 bag2 ...)
    ;; Linear update version: permitted to mutate and return bag1
    (define (bag-sum! b1 . bags)
      (let ((comparator (%bag-comparator b1)))
        (let ((new-alist
               (fold (lambda (b result-alist)
                       (bag-fold (lambda (elem acc)
                                   (%bag-increment elem 1 acc comparator))
                                 result-alist
                                 b))
                     (%bag-alist b1)
                     bags)))
          (set-cdr! (cdr b1) new-alist)
          b1)))

    ;; (bag-product n bag)
    ;; Multiply all counts by n
    (define (bag-product n b)
      (%make-bag (%bag-comparator b)
                 (filter (lambda (pair) (> (cdr pair) 0))
                         (map (lambda (pair)
                                (cons (car pair) (* n (cdr pair))))
                              (%bag-alist b)))))

    ;; (bag-product! n bag)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-product! n b)
      (set-cdr! (cdr b)
                (filter (lambda (pair) (> (cdr pair) 0))
                        (map (lambda (pair)
                               (cons (car pair) (* n (cdr pair))))
                             (%bag-alist b))))
      b)

    ;; (bag-unique-size bag)
    ;; Number of unique elements
    (define (bag-unique-size b)
      (length (%bag-alist b)))

    ;; (bag-element-count bag element)
    ;; Returns count of element in bag
    (define (bag-element-count b element)
      (%bag-get-count element (%bag-alist b) (%bag-comparator b)))

    ;; (bag-for-each-unique proc bag)
    ;; Apply proc to each unique element and its count
    (define (bag-for-each-unique proc b)
      (for-each (lambda (pair) (proc (car pair) (cdr pair)))
                (%bag-alist b)))

    ;; (bag-fold-unique proc nil bag)
    ;; Fold over unique elements with their counts
    (define (bag-fold-unique proc nil b)
      (fold (lambda (pair acc)
              (proc (car pair) (cdr pair) acc))
            nil
            (%bag-alist b)))

    ;; (bag-increment! bag element count)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-increment! b element count)
      (set-cdr! (cdr b)
                (%bag-increment element count (%bag-alist b) (%bag-comparator b)))
      b)

    ;; (bag-decrement! bag element count)
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (bag-decrement! b element count)
      (set-cdr! (cdr b)
                (%bag-decrement element count (%bag-alist b) (%bag-comparator b)))
      b)

    ;; (bag->set bag)
    ;; Convert bag to set (unique elements only)
    (define (bag->set b)
      (apply set (%bag-comparator b) (map car (%bag-alist b))))

    ;; (set->bag set)
    ;; Convert set to bag (each element has count 1)
    (define (set->bag s)
      (apply bag (set-element-comparator s) (set->list s)))

    ;; (set->bag! bag set)
    ;; Add set elements to bag
    ;; Linear update version: permitted to mutate and return the bag argument
    (define (set->bag! b s)
      (apply bag-adjoin! b (set->list s)))

    ;; (bag->alist bag)
    ;; Return alist of (element . count) pairs
    (define (bag->alist b)
      (map (lambda (pair) (cons (car pair) (cdr pair)))
           (%bag-alist b)))

    ;; (alist->bag comparator alist)
    ;; Create bag from alist of (element . count) pairs
    (define (alist->bag comparator alist)
      (%make-bag comparator
                 (filter (lambda (pair) (> (cdr pair) 0)) alist)))

    ;; bag-comparator
    ;; A comparator for comparing bags
    (define bag-comparator
      (make-comparator
       bag?                                    ; type test
       (lambda (b1 b2) (bag=? b1 b2))         ; equality
       #f                                      ; no ordering
       (lambda (b)                             ; hash function
         (bag-fold-unique (lambda (elem count acc)
                            (+ acc (* count (comparator-hash (bag-element-comparator b) elem))))
                          0
                          b))))

    ) ; end of begin
  ) ; end of define-library

