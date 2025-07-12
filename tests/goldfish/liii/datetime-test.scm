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

#|
datetime@leap?
判断指定年份是否为闰年。

语法
----
(years :leap? year)

参数
----
year:integer
    待判断的年份。

返回值
-----
boolean
    新的日期时间对象。

错误
----
type-error
    若 year 不是整数，则引发类型错误。

额外信息
----
闰年判定规则
    能被 4 整除但不能被 100 整除 → 闰年（如 2024）
    能被 400 整除 → 闰年（如 2000）
    其他情况 → 非闰年（如 2025, 1000）

|#

;; Example for type-error
(check-catch 'type-error (years :leap? 2024.1))

(check-true (years :leap? 2024))
(check-true (years :leap? 2000))

(check-false (years :leap? 2025))
(check-false (years :leap? 1000))

#|
datetime@now
创建一个表示当前系统时间的日期时间对象。
该对象精确到微秒级别，可用于获取当前时间的各个时间分量。

语法
----
(datetime :now)

参数
----
无参数（使用 :now 关键字创建当前时间对象）

返回值
-----
返回一个表示当前日期时间的对象，该对象支持以下字段查询：
    'year         : 年份 (>= 2023)
    'mouth        : 月份 (1-12)
    'day          : 日期 (1-31)
    'hour         : 小时 (0-23)
    'minute       : 分钟 (0-59)
    'second       : 秒   (0-59)
    'micro-second : 微秒 (0-999999)

错误
----
无特定错误（始终返回有效时间对象）

|#

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

#|
datetime%to-string
将 datetime 对象格式化为标准字符串表示。
    当微秒为0时，返回 "YYYY-MM-DD HH:MM:SS"，
    当微秒非0时，返回 "YYYY-MM-DD HH:MM:SS.MMMMMM" （6位微秒）。

语法
----
(datetime-object :to-string)

参数
----
无参数（直接调用对象方法）。

返回值
-----
返回日期时间字符串：
    日期部分：年-月-日
    时间部分：时:分:秒
    微秒部分：.6位微秒数（不足6位补零）

错误
----
如果调用对象不是有效的 datetime 类型，抛出类型错误。

格式规则
------
|  字段  |   格式化规则   |  示例  |
|--------|----------------|--------|
|   年   |    4位数字     |  2025  |
|  月/日 | 2位数字（补零）|  01,09 |
|时/分/秒| 2位数字（补零）|  00,05 |
|  微秒  | 6位数字（补零）| 000001 |

|#

(check ((datetime :year 2025 :month 1 :day 1) :to-string)
  => "2025-01-01 00:00:00")

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 111111) :to-string)
  => "2025-01-01 00:00:00.111111")

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 1) :to-string)
  => "2025-01-01 00:00:00.000001")

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 999999) :to-string)
  => "2025-01-01 00:00:00.999999")

#|
datetime%plus-days
计算当前日期增加/减少指定天数后的新日期对象。

语法
----
(datetime-object :plus-days days)

参数
----
days:integer
    整数，表示要增加的天数（正数）或减少的天数（负数）。

返回值
-----
datetime
    新的日期时间对象。

错误
----
type-error
    若 days 不是整数，则引发类型错误。

额外信息
----
能自动识别闰年（如 2024）与非闰年（如 2023）
跨月时自动调整月份/年份
跨年时自动递增/递减年份
days=0 时返回原日期副本

|#

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

#|
datetime%plus-months
计算当前日期增加/减少指定月数后的新日期对象，自动处理月末日期调整。

语法
----
(datetime-object :plus-months months)

参数
----
months:integer
    整数，表示要增加的月数（正数）或减少的月数（负数）。

返回值
-----
datetime
	新的日期时间对象。

错误
----
type-error
	若 months 不是整数，则引发类型错误。

额外信息
----
当原始日期是月末时，结果自动调整为目标月份的最后一天
跨年时自动调整年份
二月天数根据目标年份的闰年状态自动确定
months=0 时返回原日期副本

|#

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

#|
datetime%plus-years
计算当前日期增加/减少指定年数后的新日期对象。

语法
----
(datetime-object :plus-years years)

参数
----
years:integer
    整数，表示要增加的年（正数）或减少的年（负数）。

返回值
-----
datetime
    新的日期时间对象。

错误
----
type-error
    若 years 不是整数，则引发类型错误。

额外信息
----
能自动识别闰年（如 2024）与非闰年（如 2023）；
当原始日期为闰年2月29日且目标年份非闰年时，日期将调整为2月28日；
years=0 时返回原日期副本。

|#

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

(check ((date :year 2025 :month 1 :day 1) :to-string)
  => "2025-01-01")

(check ((date :year 2025 :month 12 :day 1) :to-string)
  => "2025-12-01")

(check ((date :year 2025 :month 3 :day 4) :to-string)
  => "2025-03-04")

(check ((date :year 2025 :month 4 :day 12) :to-string)
  => "2025-04-12")

(check-report)

