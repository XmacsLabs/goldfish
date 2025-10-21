(import (liii check)
        (liii rich-vector)
        (liii lang))

(check-set-mode! 'report-failed)

;;; 测试构造函数
(let ((v (rich-vector #(1 2 3))))
  (check (v :is-instance-of 'rich-vector) => #t)
  (check (= (v :length) 3) => #t))

;;; 测试基本操作
(let ((v (rich-vector #(1 2 3))))
  (check (= (v :fold 0 +) 6) => #t)
  (check (= (v :head) 1) => #t)
  (check (= (v :last) 3) => #t))

;;; 测试元素查找
(let ((v (rich-vector #(1 2 3))))
  (check (= (v :index-of 2) 1) => #t)
  (check (v :contains 2) => #t))

;;; 测试转换
(let ((v (rich-vector #(1 2 3))))
  (check (equal? (v :to-list) '(1 2 3)) => #t))

;;; 测试函数式操作
(let ((v (rich-vector #(1 2 3))))
  (check (equal? ((v :map (lambda (x) (* x 2))) :to-list) '(2 4 6)) => #t)
  (check (equal? ((v :filter (lambda (x) (> x 1))) :to-list) '(2 3)) => #t))

(check-report)