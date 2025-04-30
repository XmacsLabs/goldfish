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

(define-library (liii path)
(export
  path-dir? path-file? path-exists?
  path-getsize path-read-text path-read-bytes path-write-text
  path
)
(import (liii base) (liii error) (liii vector) (liii string) (liii list)
        (liii os))
(begin

(define-record-type :path
  (%make-path parts type drive)
  path?
  (parts path-parts)
  (type path-type)
  (drive path-drive))

(define (path-dir? path)
  (g_isdir path))

(define (path-file? path)
  (g_isfile path))

(define (path-exists? path)
  (file-exists? path))

(define path-getsize
  (typed-lambda ((path string?))
    (if (not (file-exists? path))
      (file-not-found-error
        (string-append "No such file or directory: '" path "'"))
      (g_path-getsize path))))

(define path-read-text
  (typed-lambda ((path string?))
    (if (not (file-exists? path))
      (file-not-found-error
        (string-append "No such file or directory: '" path "'"))
      (g_path-read-text path))))

(define path-read-bytes
  (typed-lambda ((path string?))
    (if (not (file-exists? path))
      (file-not-found-error
        (string-append "No such file or directory: '" path "'"))
      (g_path-read-bytes path))))

(define path-write-text
  (typed-lambda ((path string?) (content string?))
    (g_path-write-text path content)))

(define-case-class path
  ((parts vector?)
   (type symbol? 'posix)
   (drive string? ""))

(define (%dir?)
  (path-dir? (%to-string)))

(define (%absolute?)
  (if (eq? type 'posix)
      (string-starts? (parts 0) "/")
      (???)))

(define (@from-vector v)
  (define (check-posix-parts parts)
    (when (vector-empty? parts)
      (value-error "make-path: parts must not be emtpy for posix path"))
    (let1 N (vector-length parts)
      (let loop ((i 0))
        (when (< i (- N 1))
              (when (string-null? (parts i))
                    (value-error "make-path: part of path must not be empty string, index" i))
              (loop (+ i 1))))
      (let loop ((i 1))
        (when (< i N)
              (when (string-index (parts i) #\/)
                    (value-error "make-path: non-first part of path must not contains /"))
              (loop (+ i 1))))))
  
  (cond ((vector? v)
         (begin
           (check-posix-parts v)
           (path v)))
        ((rich-vector :is-type-of v)
         (@from-vector (v :collect)))
        (else (type-error "input must be vector or rich-vector"))))

(define (@from-string s)
  (cond ((os-linux?)
         (if (string-starts? s "/")
             (@from-vector (append #("/")
                                   (($ (string-drop s 1) :split "/") :collect)))
             (@from-vector ($ s :split "/"))))
        (else (???))))

(define (%to-string)
  (if (eq? type 'posix)
      (let1 s ($ parts :make-string "/")
        (if (string-starts? s "//")
            (string-drop s 1)
            s))
      (value-error "invalid type of path" type)))

(define (@cwd)
  (@from-string (getcwd)))

(chained-define (@/ x)
  (path (append #("/") (vector x))))

(chained-define (%/ x)
  (path (append parts (vector x))))

)

) ; end of begin
) ; end of define-library

