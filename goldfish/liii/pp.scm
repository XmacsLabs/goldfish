(define-library (liii pp)
(export pp pp-parse pp-format pp-post format-file format-single-file format-file-in-place)
(import (liii base)
        (liii string)
        (liii sys)
				(liii list)
				(liii pretty-print)
				(liii path)) 
(begin

(define (pp obj)
  (call-with-output-string
   (lambda (p)
     ((if (keyword? obj) display pretty-print) obj p))))

(define (is-newline? str pos)
  (char=? (str pos) #\newline))

(define (count-newline str pos)
  (let loop ((n 0) (i pos))
    (cond ((>= i (string-length str))
           n)
          ((not (is-newline? str i))
           n)
          (else
           (loop (+ n 1) (+ i 1))))))

(define (encode-newlines n)
  (cond ((= n 1)
         "\n")
        ((>= n 2)
         (string-append "\n" (object->string (list '*PP_NEWLINE* n)) "\n"))
        (else (value-error "encode-newline: n must >= 1, but got" n))))

;; 当前状态：start
;; 含义：位置在代码的行首
(define (next-state-from-start str pos result)
  ; (display* "start: " pos result "\n")
  (if (>= pos (string-length str))
      (values 'end pos result)
      (cond ((is-newline? str pos)
             ;; start -> start | #\newline
             (let* ((n (count-newline str pos))
                    (next-pos (+ pos n)))
               (values 'start next-pos (string-append result (encode-newlines n)))))
            ((char=? (str pos) #\;)
             ;; start -> comment | ;
             (values 'comment (+ pos 1) result))
            ((and (< (+ pos 1) (string-length str))
                  (char=? (str pos) #\#)
                  (char=? (str (+ pos 1)) #\|))
             ;; start -> multi-comment | #|
             (values 'multi-comment (+ pos 2) result))
            (else
             (values 'normal (+ pos 1) (string-append result (string (str pos))))))))

;; 当前状态： normal
;; 含义：位置在代码的行中
(define (next-state-from-normal str pos result)
  ; (display* "normal: " pos result "\n")
  (if (>= pos (string-length str))
      (values 'end pos result)
      (cond ((is-newline? str pos)
             (let* ((n (count-newline str pos))
                    (next-pos (+ pos n)))
               (values 'start next-pos (string-append result (encode-newlines n)))))
            ((and (< (+ pos 1) (string-length str))
                  (char=? (str pos) #\#)
                  (char=? (str (+ pos 1)) #\|))
             ;; normal -> multi-comment | #| 
             (values 'multi-comment (+ pos 2) result))
            (else (values 'normal (+ pos 1) (string-append result (string (str pos))))))))

(define (next-state-from-comment str pos result)
  (let loop ((pos pos) (current "") (started? #f))
    (cond ((>= pos (string-length str))
           ;; End reached, terminate with comment content
           (values 'end pos (string-append result (object->string (list '*PP_SINGLE_COMMENT* current)))))
          ((is-newline? str pos)
           ;; Found newline, comment ends, return to start state
           (values 'start pos (string-append result (object->string (list '*PP_SINGLE_COMMENT* current)))))
          ((not started?)
           ;; Skip leading whitespace after semicolon
           (if (char-whitespace? (str pos))
               (loop (+ pos 1) current #f)
               (loop (+ pos 1) (string-append current (string (str pos))) #t)))
          (else
           ;; Continue collecting comment content until newline or end
           (loop (+ pos 1) (string-append current (string (str pos))) #t)))))


(define (next-state-from-multi-comment str pos result)
  (let loop ((pos pos) (current "") (lines '()) (line-start pos))
    (cond ((>= pos (string-length str))
           ;; End reached, terminate
           (values 'end pos (object->string (cons '*PP_MULTI_COMMENT* (reverse (cons current lines))))))
          ((and (< (+ pos 1) (string-length str))
                (char=? (str pos) #\|)
                (char=? (str (+ pos 1)) #\#))
           ;; Found |#, add accumulated content to result and terminate
           (values 'multi-comment-end (+ pos 2) 
                   (string-append result (object->string (cons '*PP_MULTI_COMMENT* (reverse (cons current lines)))))))
          ((is-newline? str pos)
           ;; Found newline, add current content to lines and reset current
           (loop (+ pos 1) "" (cons current lines) (+ pos 1)))
          (else
           ;; Continue collecting characters for current line
           (loop (+ pos 1) (string-append current (string (str pos))) lines line-start)))))

(define (next-state-from-multi-comment-end str pos result)
  (if (>= pos (string-length str))
      (values 'end pos result)
      (cond ((is-newline? str pos)
             ;; multi-comment-end -> start | #\newline 
             (let* ((n (count-newline str pos))
                    (next-pos (+ pos n)))
               (values 'start next-pos (string-append result (encode-newlines n)))))
            ((char-whitespace? (str pos))
             ;; multi-comment-end -> normal | #\whitespace
             (values 'normal (+ pos 1) result))
            (else
             ;; multi-comment-end -> normal | char
             (values 'normal pos result)))))

(define (pp-parse str)
  (let loop ((state 'start) (pos 0) (result ""))
    (if (string-null? str)
        ""
        (case state 
          ((start)
           (receive (next-state next-pos next-result)
                    (next-state-from-start str pos result)
             (loop next-state next-pos next-result)))
          ((comment)
           (receive (next-state next-pos next-result)
                    (next-state-from-comment str pos result)
             (loop next-state next-pos next-result)))
          ((multi-comment)
           (receive (next-state next-pos next-result)
                    (next-state-from-multi-comment str pos result)
             (loop next-state next-pos next-result)))
          ((multi-comment-end)
           (receive (next-state next-pos next-result)
                    (next-state-from-multi-comment-end str pos result)
             (loop next-state next-pos next-result)))
          ((end) result)
          (else
           (receive (next-state next-pos next-result)
                    (next-state-from-normal str pos result)
             (loop next-state next-pos next-result)))))))

(define (find-matched-right-paren str pos)
  (let ((len (string-length str)))
    (if (or (< pos 0) (>= pos len) (not (char=? (string-ref str pos) #\()))
        -1  ; 无效起始位置或不是左括号
        (let loop ((p (+ pos 1)) (in-string? #f))
          (cond
            ((>= p len) -1)  ; 到达字符串末尾未找到
            ;; 找到右括号且不在字符串内
            ((and (not in-string?) (char=? (string-ref str p) #\)))
             p)
            ;; 进入/退出字符串
            ((char=? (string-ref str p) #\")
             (loop (+ p 1) (not in-string?)))
            ;; 其他情况继续扫描
            (else
             (loop (+ p 1) in-string?)))))))

; start状态
; 含义：位置在代码的第一行第一列
; 转移：直接进入normal状态即可
(define (next-state-from-start-post str pos result)
  (values 'normal pos result))

(define (next-state-from-newline-post str pos result)
  (values 'normal pos result))

(define (next-state-from-multi-comment-post str pos result)
  (let* ((start-pos (+ pos 20))  ; 跳过 "(*PP_MULTI_COMMENT*"
         (end-pos (find-matched-right-paren str pos))  ; 找到匹配的右括号
         (indent (let loop ((i pos))
                   (if (and (> i 0) (char-whitespace? (string-ref str (- i 1))))
                       (loop (- i 1))
                       (- pos i)))))  ; 计算缩进空格数
    
    (if (>= end-pos 0)
        (let* ((full-expr (substring str pos (+ end-pos 1)))  ; 提取完整表达式
               (expr (with-input-from-string full-expr read))  ; 用 read 解析
               (comment-texts (cdr expr))  ; 获取所有注释内容
               (indent-str (make-string indent #\space))  ; 生成缩进字符串
               (indented-texts (map (lambda (s) 
                                     (string-append indent-str s))
                                   comment-texts)))  ; 为每行添加缩进
          
          (values 'normal 
                  (+ end-pos 1)
                  (string-append result 
                               "#|"
                               (if (> indent 0) "\n" "")  ; 如果有缩进则先换行
                               (string-join indented-texts "\n")
                               "|#\n")))
        (values 'normal (string-length str) result))))

(define (is-pp-newline? str pos)
  (and (< (+ pos 14) (string-length str))  ; "(*PP_NEWLINE*" + " " + 数字 + ")"
       (string=? (substring str pos (+ pos 14)) "(*PP_NEWLINE* ")))

(define (next-state-from-newline-post str pos result)
  (let ((start-pos (+ pos 14))) ; 跳过 "(*PP_NEWLINE* "
    (let ((end-pos (string-index str #\) start-pos)))
      (if end-pos
          (let* ((num-str (substring str start-pos end-pos))
                 (n (string->number num-str)))
            (values 'normal (+ end-pos 1)
                    (string-append result (make-string (- n 2) #\newline))))
          (values 'normal (string-length str) result)))))

(define (is-pp-single-comment? str pos)
  (and (< (+ pos 21) (string-length str)) ; "(*PP_SINGLE_COMMENT*" 长度
       (string=? (substring str pos (+ pos 21)) "(*PP_SINGLE_COMMENT* ")))

(define (next-state-from-single-comment-post str pos result)
  (let* ((start-pos (+ pos 21))  ; 跳过 "(*PP_SINGLE_COMMENT*"
         (end-pos (find-matched-right-paren str pos)))  ; 找到匹配的右括号
    (if (>= end-pos 0)
        (let* ((full-expr (substring str pos (+ end-pos 1)))  ; 提取完整表达式
               (expr (with-input-from-string full-expr read))  ; 用 read 解析
               (comment-text (cadr expr)))  ; 获取注释内容
          (values 'normal (+ end-pos 1) (string-append result "; " comment-text)))
        (values 'normal (string-length str) result))))  ; 未找到右括号，直接结束

(define (is-pp-multi-comment? str pos)
  (and (< (+ pos 20) (string-length str)) ; "(*PP_MULTI_COMMENT*" 长度
       (string=? (substring str pos (+ pos 20)) "(*PP_MULTI_COMMENT* ")))

(define (next-state-from-normal-post str pos result)
  (if (>= pos (string-length str))
      (values 'end pos result)
      (cond ((is-pp-newline? str pos)
             ;; normal -> newline 状态转换：检测到 PP_NEWLINE 表达式
             (values 'newline pos result))
            ((is-pp-single-comment? str pos)
             ;; normal -> single-comment 状态转换
             (values 'single-comment pos result))
            ((is-pp-multi-comment? str pos)
             ;; normal -> multi-comment 状态转换
             (values 'multi-comment pos result))
            (else
             ;; 正常处理字符，直接按原样输出
             (values 'normal (+ pos 1) (string-append result (string (str pos))))))))

(define (pp-post str)
  (let loop ((state 'start) (pos 0) (result ""))
    (if (>= pos (string-length str))
        result
        (case state
          ((start) (receive (next-state next-pos next-result)
                        (next-state-from-start-post str pos result)
                      (loop next-state next-pos next-result)))
          ((newline) (receive (next-state next-pos next-result)
                        (next-state-from-newline-post str pos result)
                      (loop next-state next-pos next-result)))
          ((single-comment) (receive (next-state next-pos next-result)
                        (next-state-from-single-comment-post str pos result)
                      (loop next-state next-pos next-result)))
          ((multi-comment) (receive (next-state next-pos next-result)
                        (next-state-from-multi-comment-post str pos result)
                      (loop next-state next-pos next-result)))
          ((normal) (receive (next-state next-pos next-result)
                        (next-state-from-normal-post str pos result)
                      (loop next-state next-pos next-result)))
          ((end) result)
          (else
           (receive (next-state next-pos next-result)
                               (next-state-from-normal-post str pos result)
                        (loop next-state next-pos next-result)))))))

(define (format-file filename)
  (let* ((content (path-read-text filename))
         (preprocessed (pp-parse content))
         (port (open-input-string preprocessed)))
    (if (not port)
        #f
        (let ((output (open-output-string)))
          (let loop ()
            (let ((expr (read port)))
              (cond 
               ((eof-object? expr) 
                (let ((formatted (get-output-string output)))
                  (pp-post formatted)))
               (else 
                (display (pp expr) output)
                (newline output)
                (loop)))))))))

(define (format-single-file filename)
  (let ((formatted (format-file filename)))
    (if formatted
        (begin
          (display formatted)
          #t)  ; Return success
        (begin
          (display (string-append "Error: Failed to format file: " filename "\n"))
          #f))))  ; Return failure

(define (format-file-in-place filename)
  (let ((formatted (format-file filename)))
    (if formatted
        (begin
          (path-write-text filename formatted)
          #t)  ; Return success
        (begin
          (display (string-append "Error: Failed to format file: " filename "\n"))
          #f))))  ; Return failure

(define (pp-format)
  (let* ((args (argv))
         (argc (length args)))
    (cond 
     ((<= argc 2) 
      (display "Usage: format.scm [-i] <file1> [file2] ...\n")
      (display "  -i  Format files in-place\n")
      #t)  ; Return success for usage display
     (else
      (let ((first-arg (and (> argc 2) (list-ref args 2))))
        (if (and first-arg (string=? first-arg "-i"))
            (if (<= argc 3)
                (begin
                  (display "Error: -i option requires at least one file\n")
                  #f)  ; Return failure for error
                (let ((files (list-tail args 3)))  ; Skip program name and -i option
                  (let ((results (map format-file-in-place files)))
                    (if (every (lambda (x) x) results)
                        #t  ; All succeeded
                        #f)))) ; At least one failed
            (format-single-file first-arg)))))))


) ; end of begin
) ; end of define-library
