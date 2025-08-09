(import (liii check)
        (liii lint)
        (liii path))

(check-set-mode! 'report-failed)

; Test direct cases - 完整结果包括行号列号
(check (lint-check-brackets "()") => '(matched))
(check (lint-check-brackets "(") => '(unmatched (unclosed (1 1))))
(check (lint-check-brackets ")") => '(unmatched (unmatched-close (1 1))))
(check (lint-check-brackets "()()") => '(matched))
(check (lint-check-brackets "(define hello)") => '(matched))
(check (lint-check-brackets "(define (hello world)") => '(unmatched (unclosed (1 1))))
(check (lint-check-brackets "(define hello))") => '(unmatched (unmatched-close (1 15))))

; 测试不同时段和列号
(check (lint-check-brackets "()
(") => '(unmatched (unclosed (2 1))))
(check (lint-check-brackets "()
   (") => '(unmatched (unclosed (2 4))))
(check (lint-check-brackets "(a)
   )") => '(unmatched (unmatched-close (2 4))))
(check (lint-check-brackets "(define x)
    ") => '(matched))

; 测试字符串中的括号被忽略
(check (lint-check-brackets "(display \"(hello)\")") => '(matched))
(check (lint-check-brackets "(display \"(hello)") => '(unmatched (unclosed (1 1))))

; 测试注释中的括号被忽略
(check (lint-check-brackets "(display 1 ; (comment)
)") => '(matched))

; Test all resource files - 检查完整结果
(check (lint-check-brackets (path-read-text "tests/resources/200_14_valid.scm")) => '(matched))
(check (lint-check-brackets (path-read-text "tests/resources/200_14_with_strings.scm")) => '(matched))
(check (lint-check-brackets (path-read-text "tests/resources/200_14_test1.scm")) => '(unmatched (unclosed (4 1))))
(check (lint-check-brackets (path-read-text "tests/resources/200_14_unmatched_close.scm")) => '(unmatched (unmatched-close (5 32))))
(check (lint-check-brackets (path-read-text "tests/resources/200_14_bad.scm")) => '(unmatched (unclosed (2 14))))

; 测试字符串中的括号处理问题 - 这些应该被认为是平衡的
(check (lint-check-brackets "(string->utf8 \"abc\")") => '(matched))
(check (lint-check-brackets "(string->utf8 \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/\")") => '(matched))
(check (lint-check-brackets "(display \"hello (world)\")") => '(matched))
(check (lint-check-brackets "(string->utf8 \"(here are brackets)\")") => '(matched))

; Test Goldfish核心代码文件 (期望这些是可运行的)
(check (lint-check-brackets (path-read-text "goldfish/liii/base64.scm")) => '(matched))

; Test new #hashtable cases with complete results
(check (lint-check-brackets (path-read-text "tests/resources/200_14_hash_valid.scm")) => '(matched))
(check (lint-check-brackets (path-read-text "tests/resources/200_14_hash_unmatched.scm")) => '(unmatched (unclosed (6 5))))
(check (lint-check-brackets (path-read-text "tests/resources/200_14_nested_constants.scm")) => '(matched))

; 测试字符字面量#\的使用 - 确保正确处理#\"字符
(check (lint-check-brackets (path-read-text "tests/resources/200_14_char_literal_correct.scm")) => '(matched))

(check-report)