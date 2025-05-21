(import (liii check)
        (liii date))

(check-true (> ((date :now) 'year) 2023))

(check ((date :year 2025 :month 1 :day 1) :to-string)
  => "2025-01-01")

(check ((date :year 2025 :month 12 :day 1) :to-string)
  => "2025-12-01")

(check ((date :year 2025 :month 3 :day 4) :to-string)
  => "2025-03-04")

(check ((date :year 2025 :month 4 :day 12) :to-string)
  => "2025-04-12")

(check-report)

