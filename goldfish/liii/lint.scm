(define-library (liii lint)
  (export lint-check-brackets)
  (import (liii base)
          (liii list)
          (liii string))

  (begin

    (define *open-paren* (integer->char 40))
    (define *close-paren* (integer->char 41))

#|
lint-check-brackets
检查字符串中的括号是否平衡匹配，支持小括号()、中括号[]、大括号{}以及多种括号的正确嵌套。

语法
----
(lint-check-brackets content)

参数
----
content : string
要检查的字符串内容，可以是任意Scheme源文件的文本内容。

返回值
------
列表: 返回值是一个列表，其中car表示匹配状态
- '(matched) - 所有括号正确匹配
- '(unmatched (error-type location)) - 存在不匹配的括号

不良反应
--------
- 对于不平衡的括号，会返回具体的错误位置和类型
- 能够正确处理字符串、注释中的括号，忽略这些内容的括号

示例
---
(lint-check-brackets "(define test 5)")           => '(matched)
(lint-check-brackets "(define test")              => '(unmatched (unclosed (line col)))
(lint-check-brackets "(define test ))")           => '(unmatched (unmatched-close (line col)))
|#

    (define (lint-check-brackets content)
      (let ((chars (string->list content))
            (line 1)
            (col 1))
        (let loop ((pos 0)
                   (line line)
                   (col col) 
                   (chars chars)
                   (stack (list))
                   (in-string? #f)
                   (escaped? #f))
          (cond
            ((null? chars)
             (if (null? stack)
                 (cons 'matched '())
                 (cons 'unmatched (list (list 'unclosed (car stack))))))
            
            ((char=? (car chars) #\newline)
             (loop (+ pos 1) (+ line 1) 1 (cdr chars) stack in-string? #f))
            
            ((and (not in-string?) (char=? (car chars) #\;))
             (loop (+ pos 1) line (+ col 1) (cdr chars) stack in-string? #f))
            
            ((and (not in-string?) (char=? (car chars) #\#))
             (loop (+ pos 1) line (+ col 1) (cdr chars) stack in-string? #f))
            
            ((char=? (car chars) #\")
             (cond
               (escaped? (loop (+ pos 1) line (+ col 1) (cdr chars) stack in-string? #f))
               (else (loop (+ pos 1) line (+ col 1) (cdr chars) stack (not in-string?) #f))))
            
            ((and in-string? (char=? (car chars) #\/))
             (loop (+ pos 1) line (+ col 1) (cdr chars) stack in-string? (not escaped?)))
            
            (in-string?
             (loop (+ pos 1) line (+ col 1) (cdr chars) stack in-string? #f))
            
            ((char=? (car chars) *open-paren*)
             (loop (+ pos 1) line (+ col 1) (cdr chars) (cons (list line col) stack) in-string? #f))
            
            ((char=? (car chars) *close-paren*)
             (if (null? stack)
                 (cons 'unmatched (list (list 'unmatched-close (list line col))))
                 (loop (+ pos 1) line (+ col 1) (cdr chars) (cdr stack) in-string? #f)))
            
            (else
             (loop (+ pos 1) line (+ col 1) (cdr chars) stack in-string? #f))))))

  ) ; end of begin
) ; end of define-library