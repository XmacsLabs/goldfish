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

(define-library (liii logging)
(import (liii path)
        (liii datetime))
(export logging)
(begin

(define-constant NOTSET 0)
(define-constant DEBUG 10)
(define-constant INFO 20)
(define-constant WARNING 30)
(define-constant ERROR 40)
(define-constant CRITICAL 50)

(define loggers-registry (make-hash-table))
(define-class logging
  ((name string? "default")
   (path string? "")
   (level integer? WARNING))
  

(define (@apply p-name)
  ;; Check if logger with this name already exists in registry
  (let ((existing-logger (hash-table-ref loggers-registry p-name)))
    (if (eq? existing-logger #f)
        ;; If not, create a new logger and store in registry
        (let ((new-logger (logging)))
          (new-logger :set-name! p-name)
          (hash-table-set! loggers-registry p-name new-logger)
          new-logger)
        ;; If exists, return existing logger
        existing-logger)))

(define (format-timestamp)
  (let ((now (datetime :now)))
    (now :to-string)))

(define (print-log level-name . args)
  (let* ((timestamp (format-timestamp))
         (prefix (string-append timestamp " [" level-name "] " name ": "))
         (message (apply string-append 
                         (map (lambda (arg) 
                                (if (string? arg) 
                                    arg 
                                    (arg :get)))
                              args))))
    (let ((line (string-append prefix message "\n")))
      (if (string=? path "")
          (display line)
          (path-append-text path line)))))

(define (%debug?)
  (<= level DEBUG))

(define (%info?)
  (<= level INFO))

(define (%warning?)
  (<= level WARNING))

(define (%error?)
  (<= level ERROR))

(define (%critical?)
  (<= level CRITICAL))

(define (%debug . args)
  (when (%debug?)
    (apply print-log "DEBUG" args)))

(define (%info . args)
  (when (%info?)
    (apply print-log "INFO" args)))

(define (%warning . args)
  (when (%warning?)
    (apply print-log "WARNING" args)))

(define (%error . args)
  (when (%error?)
    (apply print-log "ERROR" args)))

(define (%critical . args)
  (when (%critical?)
    (apply print-log "CRITICAL" args)))

)

) ; end of begin
) ; end of define-library

