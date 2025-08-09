;
; Copyright (C) 2025 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii datetime))

(check-set-mode! 'report-failed)

;; Example for type-error
(check-catch 'type-error (years :leap? 2024.1))

(check-true (years :leap? 2024))
(check-true (years :leap? 2000))

(check-false (years :leap? 2025))
(check-false (years :leap? 1000))

(let ((now (datetime :now)))
  (check-true (datetime :is-type-of now)))

(let ((not-date "2025-01-01"))
  (check-false (datetime :is-type-of not-date)))

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

;; Example for type-error
(check-catch 'type-error ((datetime :year 2024 :month 1 :day 31) :plus-days 1.1))

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

;; Example for type-error
(check-catch 'type-error ((datetime :year 2024 :month 1 :day 31) :plus-months 1.1))

;; Test plus-months with positive months
(check ((datetime :year 2024 :month 1 :day 15) :plus-months 1) 
  => (datetime :year 2024 :month 2 :day 15))

(check ((datetime :year 2024 :month 12 :day 15) :plus-months 1) 
  => (datetime :year 2025 :month 1 :day 15))

(check ((datetime :year 2024 :month 1 :day 15) :plus-months 12) 
  => (datetime :year 2025 :month 1 :day 15))

(check ((datetime :year 2024 :month 1 :day 15) :plus-months 24) 
  => (datetime :year 2026 :month 1 :day 15))

;; Test date adjustment for month end dates
(check ((datetime :year 2024 :month 1 :day 31) :plus-months 1) 
  => (datetime :year 2024 :month 2 :day 29)) ; Feb 2024 has 29 days (leap year)

(check ((datetime :year 2023 :month 1 :day 31) :plus-months 1) 
  => (datetime :year 2023 :month 2 :day 28)) ; Feb 2023 has 28 days (non-leap year)

(check ((datetime :year 2024 :month 1 :day 31) :plus-months 2) 
  => (datetime :year 2024 :month 3 :day 31)) ; March has 31 days

(check ((datetime :year 2024 :month 1 :day 31) :plus-months 3) 
  => (datetime :year 2024 :month 4 :day 30)) ; April has 30 days

;; Test plus-months with negative months
(check ((datetime :year 2024 :month 3 :day 15) :plus-months -1) 
  => (datetime :year 2024 :month 2 :day 15))

(check ((datetime :year 2024 :month 1 :day 15) :plus-months -1) 
  => (datetime :year 2023 :month 12 :day 15))

(check ((datetime :year 2024 :month 12 :day 15) :plus-months -12) 
  => (datetime :year 2023 :month 12 :day 15))

;; Test date adjustment for month end dates with negative months
(check ((datetime :year 2024 :month 3 :day 31) :plus-months -1) 
  => (datetime :year 2024 :month 2 :day 29)) ; Feb 2024 has 29 days (leap year)

(check ((datetime :year 2023 :month 3 :day 31) :plus-months -1) 
  => (datetime :year 2023 :month 2 :day 28)) ; Feb 2023 has 28 days (non-leap year)

;; Test plus-months with zero
(check ((datetime :year 2024 :month 1 :day 15) :plus-months 0) 
  => (datetime :year 2024 :month 1 :day 15))

;; Test preserving time components
(let ((dt (datetime :year 2024 :month 1 :day 15 
            :hour 12 :minute 30 :second 45 :micro-second 123456)))
  (check (dt :plus-months 1) 
    => (datetime :year 2024 :month 2 :day 15 
         :hour 12 :minute 30 :second 45 :micro-second 123456)))

;; Test plus-years with positive years
(check ((datetime :year 2024 :month 1 :day 15) :plus-years 1) 
  => (datetime :year 2025 :month 1 :day 15))

(check ((datetime :year 2024 :month 2 :day 29) :plus-years 1) 
  => (datetime :year 2025 :month 2 :day 28))

(check ((datetime :year 2024 :month 2 :day 29) :plus-years 1) 
  => (datetime :year 2025 :month 2 :day 28)) ; Feb 29 -> Feb 28 (non-leap year)

(check ((datetime :year 2024 :month 2 :day 29) :plus-years 4) 
  => (datetime :year 2028 :month 2 :day 29)) ; Feb 29 -> Feb 29 (leap year)

(check ((datetime :year 2024 :month 2 :day 29) :plus-years 100) 
  => (datetime :year 2124 :month 2 :day 29)) ; 2124 is also a leap year

;; Test plus-years with negative years
(check ((datetime :year 2025 :month 1 :day 15) :plus-years -1) 
  => (datetime :year 2024 :month 1 :day 15))

(check ((datetime :year 2025 :month 2 :day 28) :plus-years -1) 
  => (datetime :year 2024 :month 2 :day 28))

(check ((datetime :year 2025 :month 2 :day 28) :plus-years -5) 
  => (datetime :year 2020 :month 2 :day 28)) ; 2020 is a leap year

;; Test plus-years with zero
(check ((datetime :year 2024 :month 1 :day 15) :plus-years 0) 
  => (datetime :year 2024 :month 1 :day 15))

;; Test preserving time components
(let ((dt (datetime :year 2024 :month 1 :day 15 
            :hour 12 :minute 30 :second 45 :micro-second 123456)))
  (check (dt :plus-years 1) 
    => (datetime :year 2025 :month 1 :day 15 
         :hour 12 :minute 30 :second 45 :micro-second 123456)))

(check-true (> ((date :now) 'year) 2023))
(let ((today (date :now)))
  (check-true (date :is-type-of today))
  (check-true (>= (today 'year) 2023))
  (check-true (<= 1 (today 'month) 12))
  (check-true (<= 1 (today 'day) 31)))

(check ((date :year 2025 :month 1 :day 1) :to-string)
  => "2025-01-01")

(check ((date :year 2025 :month 12 :day 1) :to-string)
  => "2025-12-01")

(check ((date :year 2025 :month 3 :day 4) :to-string)
  => "2025-03-04")

(check ((date :year 2025 :month 4 :day 12) :to-string)
  => "2025-04-12")

;; Test weekday functionality
(check ((datetime :year 2024 :month 1 :day 1) :weekday)  => 0)  ; Monday
(check ((datetime :year 2024 :month 1 :day 2) :weekday)  => 1)  ; Tuesday
(check ((datetime :year 2024 :month 1 :day 7) :weekday)  => 6)  ; Sunday
(check ((datetime :year 2024 :month 1 :day 8) :weekday)  => 0)  ; Monday

(check ((date :year 2024 :month 1 :day 1) :weekday)  => 0)  ; Monday
(check ((date :year 2024 :month 1 :day 7) :weekday)  => 6)  ; Sunday
(check ((date :year 2024 :month 2 :day 29) :weekday)  => 2)  ; Thursday (2024 is leap year)

(check-report)

