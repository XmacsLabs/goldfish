(import (scheme base)
        (scheme time)
        (only (liii list) fold)) ; 使用 SRFI-1 的 map 和 fold

(define start-time (current-jiffy))

;; 高性能版本，避免不必要的中间列表构造
(define result
  (let loop1 ((i 1) (acc1 '()))
    (if (> i 10)
        (reverse acc1) ; 最终收集结果
        (let ((squares
               (let loop2 ((j 1) (acc2 '()))
                 (if (>= j 1000)
                     (reverse acc2)
                     (loop2 (+ j 1) (cons (* j j) acc2))))))
          (loop1 (+ i 1) (cons (fold + 0 squares) acc1))))))

(display result)
(display (car result))
(newline)

(define end-time (current-jiffy))
(display (- end-time start-time))
