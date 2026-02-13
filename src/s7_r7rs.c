/* s7_r7rs.c - R7RS specific implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_r7rs.h"
#include <string.h>
#include <time.h>

#if WITH_R7RS

/* R7RS specific symbols */
s7_pointer unlink_symbol, access_symbol, time_symbol, clock_gettime_symbol,
           getenvs_symbol, uname_symbol;

/* R7RS Scheme code string */
const char r7rs_scm[] = "(provide 'r7rs.scm) \n\
(define (vector-map p . args) (apply vector (apply map p args))) \n\
(define (string-map p . args) (apply string (apply map p args))) \n\
(define vector-for-each for-each) \n\
(define string-for-each for-each) \n\
(define* (vector->string v (start 0) end) \n\
  (let ((stop (or end (length v)))) \n\
    (copy v (make-string (- stop start)) start stop))) \n\
(define* (string->vector s (start 0) end) \n\
  (let ((stop (or end (length s)))) \n\
    (copy s (make-vector (- stop start)) start stop))) \n\
(define list-copy copy) \n\
(define vector-copy string->vector) \n\
(define r7rs-string-copy substring) \n\
(define* (vector-copy! dest at src (start 0) end) \n\
  (if (not at) \n\
      (copy dest) \n\
      (if (not src) \n\
	  (copy (subvector dest at)) \n\
	  (if (integer? src) \n\
	      (copy (subvector dest at src)) \n\
	      (let ((len (or end (length src)))) \n\
		(if (or (not (eq? dest src)) \n\
			(<= at start)) \n\
		    (do ((i at (+ i 1)) \n\
			 (k start (+ k 1))) \n\
			((= k len) dest) \n\
		      (set! (dest i) (src k))) \n\
		    (do ((i (- (+ at len) start 1) (- i 1)) \n\
			 (k (- len 1) (- k 1))) \n\
			((< k start) dest) \n\
		      (set! (dest i) (src k))))))))) \n\
(define string-copy! vector-copy!) \n\
(define bytevector byte-vector) \n\
(define bytevector? byte-vector?) \n\
(define make-bytevector make-byte-vector) \n\
(define bytevector-ref byte-vector-ref) \n\
(define bytevector-set! byte-vector-set!) \n\
(define bytevector-copy! vector-copy!) \n\
(define (bytevector->list bv) (copy bv (make-list (length bv)))) \n\
(define (boolean=? . args) \n\
  (or (null? args) \n\
      (and (boolean? (car args)) \n\
	   (let loop ((obj (car args)) (lst (cdr args))) \n\
	     (or (null? lst) \n\
		 (and (eq? obj (car lst)) \n\
		      (loop obj (cdr lst)))))))) \n\
(define (symbol=? . args) \n\
  (or (null? args) \n\
      (and (symbol? (car args)) \n\
	   (let loop ((obj (car args)) (lst (cdr args))) \n\
	     (or (null? lst) \n\
		 (and (eq? obj (car lst)) \n\
		      (loop obj (cdr lst)))))))) \n\
(define char-foldcase char-downcase) \n\
(define string-foldcase string-downcase) \n\
(define (digit-value c) (and (char-numeric? c) (- (char->integer c) (char->integer #\\0)))) \n\
(define (finite? n) (and (number? n) (not (nan? n)) (not (infinite? n)))) \n\
(define exact-integer? integer?) \n\
(define (exact-integer-sqrt i) (let ((sq (floor (sqrt i)))) (values sq (- i (* sq sq))))) \n\
(define inexact exact->inexact) \n\
(define exact inexact->exact) \n\
(define (square x) (* x x)) \n\
(define truncate-quotient quotient) \n\
(define truncate-remainder remainder) \n\
(define floor-remainder modulo) \n\
(define (floor-quotient x y) (floor (/ x y))) \n\
(define (input-port-open? p) (not (port-closed? p))) \n\
(define (output-port-open? p) (not (port-closed? p))) \n\
(define (port? p) (or (input-port? p) (output-port? p))) \n\
(define binary-port? port?) \n\
(define textual-port? port?) \n\
(define (close-port p) (if (input-port? p) (close-input-port p) (close-output-port p))) \n\
(define open-binary-input-file open-input-file) \n\
(define open-binary-output-file open-output-file) \n\
(define (call-with-port port proc) (let ((res (proc port))) (if res (close-port port)) res)) \n\
(define bytevector-u8-ref byte-vector-ref) \n\
(define bytevector-u8-set! byte-vector-set!) \n\
(define bytevector-u8 (dilambda (lambda (b k) (b k)) (lambda (b k c) (set! (b k) c)))) \n\
(define bytevector-length length) \n\
(define bytevector-copy vector-copy!) \n\
(define bytevector-append append) \n\
(define* (write-bytevector bv port start end) \n\
  (if (not port) \n\
      (write bv) \n\
      (if (not start) \n\
	  (write bv port) \n\
	  (write (subvector bv start (or end (length bv))))))) \n\
(define* (read-bytevector! bv port (start 0) end) \n\
  (let ((lim (or end (length bv))) \n\
	(pt (or port (current-input-port)))) \n\
    (do ((i start (+ i 1)) \n\
	 (c (read-byte pt) (read-byte pt))) \n\
	((or (>= i lim) \n\
	     (eof-object? c)) \n\
	 (if (= i start) #<eof> (- i start))) \n\
      (set! (bv i) c)))) \n\
(define* (read-bytevector k port) \n\
  (let* ((buf (make-byte-vector k)) \n\
	 (bytes (read-bytevector! buf port))) \n\
    (if (eof-object? bytes) \n\
	bytes \n\
	(if (= k bytes) \n\
	    buf \n\
	    (subvector buf 0 bytes))))) \n\
(define (get-output-bytevector port) (string->byte-vector (get-output-string port))) \n\
(define (open-input-bytevector bv) (open-input-string (copy bv (make-string (length bv))))) \n\
(define open-output-bytevector open-output-string) \n\
(define read-u8 read-byte) \n\
(define write-u8 write-byte) \n\
(define u8-ready? char-ready?) \n\
(define peek-u8 peek-char) \n\
(define* (utf8->string v (start 0) end) \n\
  (if (string? v) \n\
      (substring v start (or end (length v))) \n\
      (substring (byte-vector->string v) start (or end (length v))))) \n\
(define* (string->utf8 s (start 0) end) \n\
  (if (byte-vector? s) \n\
      (copy (subvector s start (or end (length s)))) \n\
      (string->byte-vector (utf8->string s start end)))) \n\
(define write-simple write) \n\
(define (eof-object) #<eof>) \n\
(define-macro (features) '*features*) \n\
(define (with-exception-handler handler thunk) \n\
  (catch #t thunk \n\
	 (lambda args \n\
	   (if (aritable? handler (length args)) \n\
	       (apply handler args) \n\
	       (handler (cadr args)))))) \n\
(define raise error) \n\
(define raise-continuable error) \n\
(define (error-object? obj) #f) \n\
(define (error-object-message . args) #f) \n\
(define (error-object-irritants . args) #f) \n\
(define-macro (guard results . body) \n\
  `(let ((,(car results) (catch #t (lambda () ,@body) (lambda args (car args))))) \n\
     (cond ,@(cdr results)))) \n\
(define (read-error? obj) (eq? (car obj) 'read-error)) \n\
(define (file-error? obj) (eq? (car obj) 'io-error)) \n\
(define (error-message obj) (apply format #f (cadr obj))) \n\
(define error-irritants cdadr) \n\
(define write-shared write) \n\
(define write-simple write) \n\
(define interaction-environment curlet) \n\
(define-macro (include . files) \n\
  `(begin \n\
     ,@(map (lambda (file) \n\
	      `(load ,file (outlet (curlet)))) \n\
	    files))) \n\
(set! *#readers* (cons (cons #\\; (lambda (s) (read) (values))) *#readers*)) \n\
(define-macro (define-values vars expression) \n\
  `(if (not (null? ',vars)) \n\
       (varlet (curlet) ((lambda ,vars (curlet)) ,expression)))) \n\
(define-macro (let-values vars . body) \n\
  (if (null? vars) \n\
      `(let () ,@body) \n\
      (if (and (pair? vars) \n\
	       (pair? (car vars)) \n\
	       (null? (cdar vars))) \n\
	  `((lambda ,(caar vars) \n\
	      ,@body) \n\
	    ,(cadar vars)) \n\
	  `(with-let (apply sublet (curlet) \n\
			    (list ,@(map (lambda (v) \n\
					   `((lambda ,(car v) \n\
					       (values ,@(map (lambda (name) \n\
								(values (symbol->keyword name) name)) \n\
							      (let args->proper-list ((args (car v))) \n\
								(cond ((symbol? args) \n\
								       (list args)) \n\
								      ((not (pair? args)) \n\
								       args) \n\
								      ((pair? (car args)) \n\
								       (cons (caar args) (args->proper-list (cdr args)))) \n\
								      (else \n\
								       (cons (car args) (args->proper-list (cdr args))))))))) \n\
					     ,(cadr v))) \n\
					 vars))) \n\
	     ,@body)))) \n\
(define-macro (let*-values vals . body) \n\
  (if (null? vals) \n\
      `(let () ,@body) \n\
      (let ((args ()) \n\
	    (exprs ())) \n\
	(for-each \n\
	 (lambda (arg+expr) \n\
	   (set! args (cons (car arg+expr) args)) \n\
	   (set! exprs (cons (cadr arg+expr) exprs))) \n\
	 vals) \n\
	(let ((form `((lambda ,(car args) ,@body) ,(car exprs)))) \n\
	  (if (not (null? (cdr args))) \n\
	      (for-each \n\
	       (lambda (arg expr) \n\
		 (set! form (list (list 'lambda arg form) expr))) \n\
	       (cdr args) \n\
	       (cdr exprs))) \n\
	  form)))) \n\
(define-macro (case-lambda . choices) \n\
  `(lambda args \n\
     (cond ,@(map (lambda (choice) \n\
		    (if (symbol? (car choice)) \n\
			`(else (apply (lambda ,(car choice) ,@(cdr choice)) args)) \n\
			(if (negative? (length (car choice))) \n\
			    `((>= (length args) ,(abs (length (car choice)))) \n\
			      (apply (lambda ,(car choice) ,@(cdr choice)) args)) \n\
			    `((= (length args) ,(length (car choice))) \n\
			      (apply (lambda ,(car choice) ,@(cdr choice)) args))))) \n\
		  choices)))) \n\
(define* (make-parameter init converter) \n\
  (let* ((convert (or converter (lambda (x) x))) \n\
	 (old-values ()) \n\
	 (value (convert init))) \n\
    (lambda () value))) \n\
(define-macro (parameterize vars . body) \n\
  `(dynamic-wind \n\
       (lambda () \n\
	 ,@(map (lambda (var) \n\
		  `(with-let (funclet ,(car var)) \n\
		     (set! old-values (cons value old-values)) \n\
		     (set! value (convert ,(cadr var))))) \n\
		vars)) \n\
       (lambda () \n\
         ,@body) \n\
       (lambda () \n\
	 ,@(map (lambda (var) \n\
		  `(with-let (funclet ,(car var)) \n\
		     (set! value (car old-values)) \n\
		     (set! old-values (cdr old-values)))) \n\
		vars)))) \n\
(apply define (symbol (object->string '(scheme base))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme r5rs))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme read))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme write))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme time))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme file))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme cxr))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme inexact))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme char))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme complex))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme eval))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme process-context))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme case-lambda))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme lazy))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme load))) (inlet) ()) \n\
(apply define (symbol (object->string '(scheme repl))) (inlet) ()) \n\
(define-macro (define-library libname . body) \n\
  `(define ,(symbol (object->string libname)) \n\
     (with-let (sublet (unlet) \n\
		       (cons 'import import) \n\
		       (cons '*export* ()) \n\
		       (cons 'export (define-macro (,(gensym) . names) \n\
				       `(set! *export* (append ',names *export*))))) \n\
       ,@body \n\
       (apply inlet \n\
	      (map (lambda (entry) \n\
		     (if (or (member (car entry) '(*export* export import)) \n\
			     (and (pair? *export*) \n\
				  (not (member (car entry) *export*)))) \n\
			 (values) \n\
			 entry)) \n\
		   (curlet)))))) \n\
(unless (defined? 'r7rs-import-library-filename) \n\
  (define (r7rs-import-library-filename libs) \n\
    (when (pair? libs) \n\
      (unless (eq? (caar libs) 'scheme) \n\
        (let ((lib-filename (let loop ((lib (if (memq (caar libs) '(only except prefix rename)) \n\
						(cadar libs) \n\
						(car libs))) \n\
				       (name \"\")) \n\
			      (set! name (string-append name (symbol->string (car lib)))) \n\
			      (if (null? (cdr lib)) \n\
				  (string-append name \".scm\") \n\
				  (begin \n\
				    (set! name (string-append name \"/\")) \n\
				    (loop (cdr lib) name)))))) \n\
	  (unless (member lib-filename (*s7* 'file-names)) \n\
	    (load lib-filename)))) \n\
      (r7rs-import-library-filename (cdr libs))))) \n\
(define-macro (import . libs) \n\
  `(begin \n\
     (r7rs-import-library-filename ',libs) \n\
     (varlet (curlet) \n\
       ,@(map (lambda (lib) \n\
		(case (car lib) \n\
		  ((only) \n\
		   `((lambda (e names) \n\
		       (apply inlet \n\
			      (map (lambda (name) \n\
				     (cons name (e name))) \n\
				   names))) \n\
		     (symbol->value (symbol (object->string (cadr ',lib)))) \n\
		     (cddr ',lib))) \n\
		  ((except) \n\
		   `((lambda (e names) \n\
		       (apply inlet \n\
			      (map (lambda (entry) \n\
				     (if (member (car entry) names) \n\
					 (values) \n\
					 entry)) \n\
				   e))) \n\
		     (symbol->value (symbol (object->string (cadr ',lib)))) \n\
		     (cddr ',lib))) \n\
		  ((prefix) \n\
		   `((lambda (e prefx) \n\
		       (apply inlet \n\
			      (map (lambda (entry) \n\
				     (cons (string->symbol \n\
					    (string-append (symbol->string prefx) \n\
							   (symbol->string (car entry)))) \n\
					   (cdr entry))) \n\
				   e))) \n\
		     (symbol->value (symbol (object->string (cadr ',lib)))) \n\
		     (caddr ',lib))) \n\
		  ((rename) \n\
		   `((lambda (e names) \n\
		       (apply inlet \n\
			      (map (lambda (entry) \n\
				     (let ((info (assoc (car entry) names))) \n\
				       (if info \n\
					   (cons (cadr info) (cdr entry)) \n\
					   entry))) \n\
				   e))) \n\
		     (symbol->value (symbol (object->string (cadr ',lib)))) \n\
		     (cddr ',lib))) \n\
		  (else \n\
		   `(let ((sym (symbol (object->string ',lib)))) \n\
		      (if (not (defined? sym)) \n\
			  (format () \"~A not loaded~%\" sym) \n\
			  (symbol->value sym)))))) \n\
	      libs)))) \n\
(define (make-promise obj) \n\
  (if (promise? obj) \n\
      obj \n\
      (list (cons #t (lambda () obj)) '+promise+))) \n\
(define (promise? obj) \n\
  (and (pair? obj) \n\
       (pair? (cdr obj)) \n\
       (eq? (cadr obj) '+promise+))) \n\
(define-macro (r7rs-delay expr) \n\
  `(list (cons #f (lambda () ,expr)) '+promise+)) \n\
(define-macro (delay-force expr) \n\
  `(make-promise ,expr)) \n\
(define (force promise) \n\
  (if (caar promise) \n\
      (if (procedure? (cdar promise)) \n\
	  ((cdar promise)) \n\
	  (cdar promise)) \n\
      (let ((promise* ((cdar promise)))) \n\
        (set-car! (car promise) #t) \n\
	(set-cdr! (car promise) promise*) \n\
	promise*))) \n\
(define (jiffies-per-second) 1000000000) \n\
(define (current-jiffy) \n\
  (let ((res (clock_gettime CLOCK_REALTIME))) \n	\
    (+ (* 1000000000 (cadr res)) (caddr res)))) \n\
(define (current-second) (* 1.0 (time (c-pointer 0 'time_t*)))) \n\
(define get-environment-variable getenv) \n\
(define get-environment-variables getenvs) \n\
(define (os-type) (car (uname))) \n\
(define (cpu-architecture) (cadr (uname))) \n\
(define (machine-name) (caddr (uname))) \n\
(define (os-version) (string-append (list-ref (uname) 3) \" \" (list-ref (uname) 4))) \n\
(define (implementation-name) (copy \"s7\")) \n\
(define (implementation-version) (substring (*s7* 'version) 3 7)) \n\
(unless (defined? 'null-environment) \n\
  (define (null-environment . args) (rootlet))) \n\
(define (environment . args) (rootlet)) \n\
(define (command-line) \n\
  (let ((lst ())) \n\
    (with-input-from-file \"/proc/self/cmdline\" \n\
      (lambda () \n\
	(do ((c (read-char) (read-char)) \n\
	     (s \"\")) \n\
	    ((eof-object? c) \n\
	     (reverse lst)) \n\
	  (if (char=? c #\\null) \n\
	      (begin \n\
		(set! lst (cons s lst)) \n\
		(set! s \"\")) \n\
	      (set! s (string-append s (string c))))))))) \n\
(define-macro (define-record-type type make ? . fields) \n\
  (let ((obj (gensym)) \n\
	(typ (gensym)) \n\
	(args (map (lambda (field) \n\
		     (values (list 'quote (car field)) \n\
			     (let ((par (memq (car field) (cdr make)))) \n\
			       (and (pair? par) (car par))))) \n\
		   fields))) \n\
    `(begin \n\
       (define (,? ,obj) \n\
	 (and (let? ,obj) \n\
	      (eq? (let-ref ,obj ',typ) ',type))) \n\
       (define ,make \n\
         (inlet ',typ ',type ,@args)) \n\
       ,@(map \n\
	  (lambda (field) \n\
	    (when (pair? field) \n\
	      (if (null? (cdr field)) \n\
		  (values) \n\
		  (if (null? (cddr field)) \n\
		      `(define (,(cadr field) ,obj) \n\
			 (let-ref ,obj ',(car field))) \n\
		      `(begin \n\
			 (define (,(cadr field) ,obj) \n\
			   (let-ref ,obj ',(car field))) \n\
			 (define (,(caddr field) ,obj val) \n\
			   (let-set! ,obj ',(car field) val))))))) \n\
	  fields) \n\
       ',type))) \n\
(define-record-type box-type (box value) box? (value unbox set-box!)) \n\
";

/* -------------------------------- getenvs -------------------------------- */
extern char **environ;

s7_pointer g_getenvs(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_nil(sc);
  for (int32_t i = 0; environ[i]; i++)
    {
      const char *eq;
      s7_pointer name, value;
      eq = strchr((const char *)environ[i], (int)'=');
      name = s7_make_string_with_length(sc, environ[i], eq - environ[i]);
      value = s7_make_string(sc, (char *)(eq + 1));
      p = s7_cons(sc, s7_cons(sc, name, value), p);
    }
  return(p);
}

#endif /* WITH_R7RS */
