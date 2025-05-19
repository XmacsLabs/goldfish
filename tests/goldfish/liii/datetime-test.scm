(import (liii check)
        (liii datetime))

(let ((now (datetime :now)))
  (check-true (datetime :is-type-of now))
  (check-true (>= (now 'year) 2023))  ; Assuming test is run in 2023 or later
  (check-true (<= 1 (now 'month) 12))
  (check-true (<= 1 (now 'day) 31))
  (check-true (<= 0 (now 'hour) 23))
  (check-true (<= 0 (now 'minute) 59))
  (check-true (<= 0 (now 'second) 59))
  (check-true (<= 0 (now 'micro-second) 999999)))

;; Test microsecond functionality
(let ((dt1 (datetime :now))
      (dt2 (datetime :now)))
  ;; Two close timestamps should have different microsecond values
  (check-true (integer? (dt1 'micro-second)))
  (check-true (integer? (dt2 'micro-second)))
  (check-true (<= 0 (dt1 'micro-second) 999999))
  (check-true (<= 0 (dt2 'micro-second) 999999)))

;; Tests for creating datetime with specific microsecond values
(check ((datetime :year 2025 :month 1 :day 1) :to-string)
  => "2025-01-01 00:00:00")

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 111111) :to-string)
  => "2025-01-01 00:00:00.111111")

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 1) :to-string)
  => "2025-01-01 00:00:00.000001")

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 999999) :to-string)
  => "2025-01-01 00:00:00.999999")

