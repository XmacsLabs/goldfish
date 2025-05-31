(import (liii range))

((range :inclusive 1 10) :for-each
 (lambda (x) (display x) (newline)))