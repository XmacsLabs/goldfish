; Copyright (C) John Cowan (2015). All Rights Reserved.
;
; Permission is hereby granted, free of charge, to any person obtaining a copy of
; this software and associated documentation files (the "Software"), to deal in
; the Software without restriction, including without limitation the rights to
; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
; of the Software, and to permit persons to whom the Software is furnished to do
; so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
;

(define-library (srfi srfi-113)
  (import (scheme base)
          (scheme case-lambda)
          (liii hash-table)
          (srfi srfi-128))
  (export set set-unfold list->set set-copy
          set? set-contains? set-empty? set-disjoint?
          set-element-comparator
          set=? set<? set>? set<=? set>=?)
  (begin

    (define-record-type set-impl
      (%make-set hash-table comparator) 
      set?                             
      (hash-table set-hash-table)      
      (comparator set-element-comparator)) 

    (define (check-set obj)
      (if (not (set? obj)) (error "not a set" obj)))

    (define (check-same-comparator a b)
      (if (not (eq? (set-element-comparator a) (set-element-comparator b)))
          (error "different comparators" a b)))

    (define (make-set/comparator comparator)
      (%make-set (make-hash-table comparator) comparator))

    (define (set-increment! s element)
      (hash-table-set! (set-hash-table s) element 1))

    (define (set comparator . elements)
      (let ((result (make-set/comparator comparator)))
        (for-each (lambda (x) (set-increment! result x)) elements)
        result))
    
    (define (list->set comparator elements)
      (apply set comparator elements))

    (define (set-unfold stop? mapper successor seed comparator)
      (let ((result (make-set/comparator comparator)))
        (let loop ((seed seed))
          (if (stop? seed)
              result
              (begin
                (set-increment! result (mapper seed))
                (loop (successor seed)))))))
    
    (define (set-copy s)
      (check-set s)
      (list->set (set-element-comparator s) (hash-table-keys (set-hash-table s))))

    
    (define (set-size s)
      (check-set s)
      (hash-table-size (set-hash-table s)))


    (define (set-contains? s member)
      (check-set s)
      (hash-table-contains? (set-hash-table s) member))

    (define (set-empty? s)
      (check-set s)
      (hash-table-empty? (set-hash-table s)))

    (define (set-disjoint? a b)
      (check-set a)
      (check-set b)
      (check-same-comparator a b)
      (let ((na (set-size a))
            (nb (set-size b)))
        (if (< na nb)
            (not (any-in-other? a b))
            (not (any-in-other? b a)))))
            
    (define (any-in-other? small big)
      (let ((ht-small (set-hash-table small))
            (ht-big (set-hash-table big)))
         (call/cc (lambda (return)
            (hash-table-for-each
               (lambda (k v)
                  (if (hash-table-contains? ht-big k) (return #t)))
               ht-small)
            #f))))


    (define (binary-set<=? s1 s2)
      (check-set s1)
      (check-set s2)
      (check-same-comparator s1 s2)
      (let ((n1 (set-size s1))
            (n2 (set-size s2)))
        (cond
          ((> n1 n2) #f)
          (else
           (let ((ht1 (set-hash-table s1))
                 (ht2 (set-hash-table s2)))
             (call/cc
              (lambda (return)
                (hash-table-for-each
                 (lambda (k v)
                   (unless (hash-table-contains? ht2 k)
                     (return #f)))
                 ht1)
                #t)))))))
                
    (define (set<=? . sets)
      (if (null? sets)
          #t
          (let loop ((head (car sets)) (tail (cdr sets)))
            (if (null? tail)
                #t
                (let ((next (car tail)))
                  (and (binary-set<=? head next)
                       (loop next (cdr tail))))))))
                       
    (define (set=? . sets)
      (if (null? sets)
          #t
          (let loop ((head (car sets)) (tail (cdr sets)))
            (if (null? tail)
                #t
                (let ((next (car tail)))
                  (and (binary-set=? head next)
                       (loop next (cdr tail))))))))
                       
    (define (binary-set=? s1 s2)
       (check-set s1)
       (check-set s2)
       (check-same-comparator s1 s2)
       (let ((n1 (set-size s1))
             (n2 (set-size s2)))
         (and (= n1 n2)
              (binary-set<=? s1 s2))))
              
    (define (set<? . sets)
      (if (null? sets)
          #t
          (let loop ((head (car sets)) (tail (cdr sets)))
            (if (null? tail)
                #t
                (let ((next (car tail)))
                  (and (binary-set<? head next)
                       (loop next (cdr tail))))))))

    (define (binary-set<? s1 s2)
      (check-set s1)
      (check-set s2)
      (check-same-comparator s1 s2)
      (and (< (set-size s1) (set-size s2))
           (binary-set<=? s1 s2)))

    (define (set>=? . sets)
      (if (null? sets)
          #t
          (let loop ((head (car sets)) (tail (cdr sets)))
            (if (null? tail)
                #t
                (let ((next (car tail)))
                  (and (binary-set<=? next head)
                       (loop next (cdr tail))))))))

    (define (set>? . sets)
      (if (null? sets)
          #t
          (let loop ((head (car sets)) (tail (cdr sets)))
            (if (null? tail)
                #t
                (let ((next (car tail)))
                  (and (binary-set<? next head)
                       (loop next (cdr tail))))))))

    ) ; end of begin
  ) ; end of define-library