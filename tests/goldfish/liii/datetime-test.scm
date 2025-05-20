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

(check ((datetime :year 2025 :month 1 :day 1) :to-string)
  => "2025-01-01 00:00:00")

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 111111) :to-string)
  => "2025-01-01 00:00:00.111111")

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 1) :to-string)
  => "2025-01-01 00:00:00.000001")

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 999999) :to-string)
  => "2025-01-01 00:00:00.999999")

;; Test plus-days with positive days
(check ((datetime :year 2024 :month 1 :day 1) :plus-days 10) 
  => (datetime :year 2024 :month 1 :day 11))

(check ((datetime :year 2024 :month 1 :day 31) :plus-days 1) 
  => (datetime :year 2024 :month 2 :day 1))

(check ((datetime :year 2024 :month 1 :day 1) :plus-days 31) 
  => (datetime :year 2024 :month 2 :day 1))

(check ((datetime :year 2024 :month 2 :day 28) :plus-days 1) 
  => (datetime :year 2024 :month 2 :day 29)) ; 2024 is a leap year

(check ((datetime :year 2024 :month 2 :day 29) :plus-days 1) 
  => (datetime :year 2024 :month 3 :day 1))

(check ((datetime :year 2023 :month 2 :day 28) :plus-days 1) 
  => (datetime :year 2023 :month 3 :day 1)) ; 2023 is not a leap year

(check ((datetime :year 2024 :month 12 :day 31) :plus-days 1) 
  => (datetime :year 2025 :month 1 :day 1))

(check ((datetime :year 2024 :month 1 :day 1) :plus-days 366) 
  => (datetime :year 2025 :month 1 :day 1)) ; 2024 is a leap year

;; Test plus-days with negative days
(check ((datetime :year 2024 :month 1 :day 11) :plus-days -10) 
  => (datetime :year 2024 :month 1 :day 1))

(check ((datetime :year 2024 :month 2 :day 1) :plus-days -1) 
  => (datetime :year 2024 :month 1 :day 31))

(check ((datetime :year 2024 :month 3 :day 1) :plus-days -1) 
  => (datetime :year 2024 :month 2 :day 29))  ; 2024 is a leap year

(check ((datetime :year 2023 :month 3 :day 1) :plus-days -1) 
  => (datetime :year 2023 :month 2 :day 28))  ; 2023 is not a leap year

(check ((datetime :year 2025 :month 1 :day 1) :plus-days -1) 
  => (datetime :year 2024 :month 12 :day 31))

(check ((datetime :year 2025 :month 1 :day 1) :plus-days -365) 
  => (datetime :year 2024 :month 1 :day 2)) ; 2024 is a leap year

;; Test plus-days with zero
(check ((datetime :year 2024 :month 1 :day 1) :plus-days 0) 
  => (datetime :year 2024 :month 1 :day 1))

;; Test preserving time components
(let ((dt (datetime :year 2024 :month 1 :day 1 
                   :hour 12 :minute 30 :second 45 :micro-second 123456)))
  (check (dt :plus-days 10) 
    => (datetime :year 2024 :month 1 :day 11 
                :hour 12 :minute 30 :second 45 :micro-second 123456)))

(check-report)

