(import (liii range))

((range 1 10) :for-each
 (lambda (x) (display x) (newline)))

((range :inclusive 1 10) :for-each
 (lambda (x) (display x) (newline)))
