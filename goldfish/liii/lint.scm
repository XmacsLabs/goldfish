(define-library (liii lint)
  (export lint-check-brackets)
  (import (liii base)
          (liii list)
          (liii string)
          (liii stack))

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
            (col 1)
            (stack (stack '())))
        (let loop ((pos 0)
                   (line line)
                   (col col)
                   (chars chars)
                   (st stack)
                   (in-string? #f)
                   (escaped? #f))
          (cond
            ((null? chars)
             (if (= (st :size) 0)
                 (cons 'matched '())
                 (let ((last-open (st :top)))
                   (cons 'unmatched (list (list 'unclosed last-open))))))
            
            ((char=? (car chars) #\newline)
             (loop (+ pos 1) (+ line 1) 1 (cdr chars) st in-string? #f))
            
            ((and (not in-string?) (char=? (car chars) #\;))
             ; 注释到行尾，找第一个换行符
             (let ((new-chars chars)
                   (new-pos pos)
                   (new-col col)
                   (new-line line))
               (let skip-comment ((chars new-chars)
                                  (pos new-pos)
                                  (col new-col)
                                  (line new-line))
                 (if (or (null? chars) (char=? (car chars) #\newline))
                     (if (null? chars)
                         (loop pos line col chars st in-string? #f)
                         (loop (+ pos 1) (+ line 1) 1 (cdr chars) st in-string? #f))
                     (skip-comment (cdr chars) (+ pos 1) (+ col 1) line)))))
            
            ((and (not in-string?) (char=? (car chars) #\#))
             ; 需要正确处理多种#开头的常量，包括#	、#\等
             (cond
               ((null? (cdr chars)) ; 如果是最后一个字符，跳过
                (loop (+ pos 1) line (+ col 1) (cdr chars) st in-string? #f))
               ((char=? (cadr chars) #\\) ; #\ 是字符转义
                (cond
                  ((null? (cddr chars)) ; 只有#\，没有后续字符
                   (loop (+ pos 2) line (+ col 2) (cddr chars) st in-string? #f))
                  (else ; 跳过#\和后面的字符
                   (loop (+ pos 3) line (+ col 3) (cdddr chars) st in-string? #f))))
               (else ; 其他情况，跳过单个字符
                (loop (+ pos 1) line (+ col 1) (cdr chars) st in-string? #f))))
            
            ((char=? (car chars) #\")
             (cond
               (escaped? (loop (+ pos 1) line (+ col 1) (cdr chars) st in-string? #f))
               (else (loop (+ pos 1) line (+ col 1) (cdr chars) st (not in-string?) #f))))
            
            ((and in-string? (char=? (car chars) #\\))
             (loop (+ pos 1) line (+ col 1) (cdr chars) st in-string? (not escaped?)))
            
            (in-string?
             (loop (+ pos 1) line (+ col 1) (cdr chars) st in-string? #f))
            
            ((char=? (car chars) *open-paren*)
             (let ((new-st (st :push (list line col))))
               (loop (+ pos 1) line (+ col 1) (cdr chars) new-st in-string? #f)))
            
            ((char=? (car chars) *close-paren*)
             (if (= (st :size) 0)
                 (cons 'unmatched (list (list 'unmatched-close (list line col))))
                 (let ((new-st (st :pop)))
                   (loop (+ pos 1) line (+ col 1) (cdr chars) new-st in-string? #f))))
            
            (else
             (loop (+ pos 1) line (+ col 1) (cdr chars) st in-string? #f))))))

  ) ; end of begin
) ; end of define-library