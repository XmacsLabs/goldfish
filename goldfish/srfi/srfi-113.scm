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
  (import (liii set))
  (export
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

    ) ; end of begin
  ) ; end of define-library

