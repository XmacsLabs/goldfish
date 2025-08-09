(import (liii check)
        (liii lint)
        (liii path))

(check-set-mode! 'report-failed)

; Test direct cases
(check (car (lint-check-brackets "()")) => 'matched)
(check (car (lint-check-brackets "(")) => 'unmatched)
(check (car (lint-check-brackets ")")) => 'unmatched)
(check (car (lint-check-brackets "()()")) => 'matched)
(check (car (lint-check-brackets "(define hello)")) => 'matched)
(check (car (lint-check-brackets "(define (hello world)")) => 'unmatched)
(check (car (lint-check-brackets "(define hello))")) => 'unmatched)

; Test all resource files
(check (car (lint-check-brackets (path-read-text "tests/resources/200_14_valid.scm"))) => 'matched)
(check (car (lint-check-brackets (path-read-text "tests/resources/200_14_with_strings.scm"))) => 'matched)
(check (car (lint-check-brackets (path-read-text "tests/resources/200_14_test1.scm"))) => 'unmatched)
(check (car (lint-check-brackets (path-read-text "tests/resources/200_14_unmatched_close.scm"))) => 'unmatched)
(check (car (lint-check-brackets (path-read-text "tests/resources/200_14_bad.scm"))) => 'unmatched)

; Test new #racket cases
(check (car (lint-check-brackets (path-read-text "tests/resources/200_14_hash_valid.scm"))) => 'matched)
(check (car (lint-check-brackets (path-read-text "tests/resources/200_14_hash_unmatched.scm"))) => 'unmatched)
(check (car (lint-check-brackets (path-read-text "tests/resources/200_14_nested_constants.scm"))) => 'matched)

(check-report)