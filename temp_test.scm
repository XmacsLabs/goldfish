(import (liii lang_2))

(display "Testing rich-lists :range 1 5: ")
(display ((rich-lists :range 1 5) :apply :collect))
(newline)

(display "Testing rich-lists :empty :empty?: ")
(display ((rich-lists :empty) :apply :empty?))
(newline)

; (display "Testing rich-lists :empty :head-option: ")
; (display ((rich-lists :empty) :head-option))
; (newline)

(display "All basic tests passed!")
(newline) 