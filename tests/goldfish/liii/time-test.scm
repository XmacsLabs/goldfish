;
; Copyright (C) 2024 The Goldfish Scheme Authors
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
        (liii time)
        (scheme time)
        (liii base))

(check-set-mode! 'report-failed)


#|
sleep
使当前线程暂停执行指定的秒数。

语法
----
(sleep seconds)

参数
----
seconds : number?
暂停的时间长度，以秒为单位。可以是整数或浮点数，表示精确的时间间隔。

返回值
-----
#<unspecified>
返回未指定的值，主要用于其副作用（暂停执行）。

说明
----
1. 暂停当前线程的执行，让出CPU时间给其他线程或进程
2. 实际暂停时间可能受到系统调度精度的影响
3. 对于短时间间隔（如毫秒级），实际暂停时间可能比指定的稍长
4. 参数必须是数值类型，否则会抛出类型错误

错误处理
--------
type-error
当参数不是数值类型时抛出错误。

|#

; Test sleep function
(let ((t1 (current-second)))
  (sleep 1)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 1) => #t)))

(let ((t1 (current-second)))
  (sleep 0.5)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 0) => #t)))

(check-catch 'type-error (sleep 'not-a-number))


#|
current-second
获取当前时间，以秒为单位。

语法
----
(current-second)

参数
----
无参数

返回值
-----
number?
返回从某个固定时间点（通常是纪元时间）到当前时间的秒数，返回值为浮点数。

说明
----
1. 返回的时间戳通常基于Unix纪元时间（1970-01-01 00:00:00 UTC）
2. 返回值是浮点数类型，支持小数秒精度
3. 精度取决于系统实现，通常为秒级精度
4. 主要用于时间测量和时间戳生成

示例
----
(let ((start (current-second)))
  (sleep 1)
  (let ((end (current-second)))
    (display (format "耗时: ~a 秒" (- end start)))))

|#

(let ((t1 (current-second)))
  (check (number? t1) => #t)
  (check (>= t1 0) => #t))


#|
current-jiffy
获取当前时间的 jiffy 计数。

语法
----
(current-jiffy)

参数
----
无参数

返回值
-----
integer?
返回从某个固定时间点（通常是纪元时间）到当前时间的 jiffy 计数，返回值为整数。

说明
----
1. jiffy 是时间测量单位，1 jiffy = 1/1,000,000 秒（微秒）
2. 返回值是整数类型，提供比秒更精确的时间测量
3. 主要用于高精度时间测量和性能分析
4. 与 current-second 的关系：current-jiffy = (round (* current-second 1000000))

示例
----
(let ((start (current-jiffy)))
  (do-some-work)
  (let ((end (current-jiffy)))
    (display (format "耗时: ~a 微秒" (- end start)))))

|#
(let ((j1 (current-jiffy)))
  (check (integer? j1) => #t)
  (check (>= j1 0) => #t))


#|
jiffies-per-second
获取每秒钟的 jiffy 数量。

语法
----
(jiffies-per-second)

参数
----
无参数

返回值
-----
integer?
返回每秒钟包含的 jiffy 数量，固定值为 1000000。

说明
----
1. 定义 jiffy 与秒之间的换算关系：1 秒 = 1000000 jiffy
2. 返回值是固定的整数，用于时间单位转换
3. 主要用于将 jiffy 时间间隔转换为秒数
4. 与 current-jiffy 配合使用，可以计算精确的时间间隔

示例
----
(let ((start (current-jiffy)))
  (do-some-work)
  (let ((end (current-jiffy)))
    (display (format "耗时: ~a 秒" (/ (- end start) (jiffies-per-second))))))

|#
(check (jiffies-per-second) => 1000000)
(check (integer? (jiffies-per-second)) => #t)
(check (positive? (jiffies-per-second)) => #t)

(check-report)
