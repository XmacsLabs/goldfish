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
        (liii bitwise))

(check-set-mode! 'report-failed)

#|
bitwise-not
计算整数的按位取反（补码表示）。

语法
----
(bitwise-not i)

参数
----
i : integer?
整数，要进行按位取反操作的整数。

返回值
-----
integer?
返回整数 i 的按位取反结果。

说明
----
1. 对整数 i 的每一位进行取反操作（0 变 1，1 变 0）
2. 在补码表示中，bitwise-not 等价于 (- i 1)
3. 对于任意整数 i，(bitwise-not (bitwise-not i)) = i
4. 对于 0，bitwise-not 返回 -1
5. 对于 -1，bitwise-not 返回 0

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 基本功能测试：按位取反操作
(check (bitwise-not 0) => -1)
(check (bitwise-not 1) => -2)
(check (bitwise-not #b1000) => -9)
(check (bitwise-not -1) => 0)

;;; 边界值测试
(check (bitwise-not 2) => -3)     ; #b10 => #b11111101
(check (bitwise-not -2) => 1)     ; #b11111110 => #b1
(check (bitwise-not 255) => -256) ; #b11111111 => #b11111111111111111111111100000000
(check (bitwise-not -256) => 255) ; #b11111111111111111111111100000000 => #b11111111

;;; 二进制表示测试
(check (bitwise-not #b1010) => -11)   ; #b1010 => #b11110101
(check (bitwise-not #b0101) => -6)    ; #b0101 => #b11111010
(check (bitwise-not #b1111) => -16)   ; #b1111 => #b11110000

;;; 特殊值测试
(check (bitwise-not 2147483647) => -2147483648)  ; 最大32位有符号整数
(check (bitwise-not -2147483648) => 2147483647)  ; 最小32位有符号整数

#|
bitwise-and
计算多个整数的按位与操作。

语法
----
(bitwise-and i1 i2 ...)

参数
----
i1, i2, ... : integer?
一个或多个整数，参与按位与操作。

返回值
-----
integer?
返回所有整数按位与操作的结果。

说明
----
1. 对所有整数的每一位进行与操作（都为1时结果为1，否则为0）
2. 按位与操作常用于提取特定位或掩码操作
3. 对于任意整数 i，(bitwise-and i i) = i
4. 对于任意整数 i，(bitwise-and i 0) = 0
5. 对于任意整数 i，(bitwise-and i -1) = i
6. 按位与操作满足交换律：(bitwise-and i1 i2) = (bitwise-and i2 i1)
7. 按位与操作满足结合律：(bitwise-and i1 (bitwise-and i2 i3)) = (bitwise-and (bitwise-and i1 i2) i3)
8. 支持两个或多个参数，按从左到右的顺序依次进行按位与操作

实现说明
--------
- bitwise-and 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 如果考虑性能优化，可以使用 S7 Scheme 内置的 logand 函数
- logand 是 S7 的原生函数，通常比 bitwise-and 有更好的性能
- bitwise-and支持三个或更多参数，logand仅支持两个参数

错误
----
type-error
当参数不是整数时抛出错误。
|#

;;; 基本功能测试：按位与操作
(check (bitwise-and 5 3) => 1)  ; 5 (101) AND 3 (011) = 1 (001)
(check (bitwise-and 8 4) => 0)  ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b101 #b011) => 1)  ; 5 (101) AND 3 (011) = 1 (001)
(check (bitwise-and #b1000 #b0100) => 0) ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b1100 #b1010) => 8)

;;; 边界值测试
(check (bitwise-and 0 0) => 0)          ; 0 AND 0 = 0
(check (bitwise-and 0 1) => 0)          ; 0 AND 1 = 0
(check (bitwise-and 1 0) => 0)          ; 1 AND 0 = 0
(check (bitwise-and 1 1) => 1)          ; 1 AND 1 = 1
(check (bitwise-and -1 -1) => -1)       ; -1 AND -1 = -1
(check (bitwise-and -1 0) => 0)         ; -1 AND 0 = 0
(check (bitwise-and 0 -1) => 0)         ; 0 AND -1 = 0

;;; 数学性质测试
(check (bitwise-and 15 15) => 15)       ; 自反性
(check (bitwise-and 7 3) => (bitwise-and 3 7)) ; 交换律
(check (bitwise-and 15 (bitwise-and 7 3)) => (bitwise-and (bitwise-and 15 7) 3)) ; 结合律
(check (bitwise-and 255 0) => 0)        ; 与0相与得0
(check (bitwise-and 255 -1) => 255)     ; 与-1相与得原数

;;; 二进制表示测试
(check (bitwise-and #b10101010 #b01010101) => 0)  ; 交替位模式
(check (bitwise-and #b11110000 #b11001100) => #b11000000) ; 部分重叠
(check (bitwise-and #b11111111 #b00001111) => #b00001111) ; 掩码提取低4位
(check (bitwise-and #b11111111 #b11110000) => #b11110000) ; 掩码提取高4位

;;; 特殊值测试
(check (bitwise-and 2147483647 2147483647) => 2147483647) ; 最大32位有符号整数
(check (bitwise-and -2147483648 -2147483648) => -2147483648) ; 最小32位有符号整数
(check (bitwise-and 2147483647 -2147483648) => 0) ; 最大和最小整数相与

;;; 三个参数测试
(check (bitwise-and 1 2 3) => 0)          ; 001 & 010 & 011 = 000
(check (bitwise-and 7 3 5) => 1)          ; 111 & 011 & 101 = 001
(check (bitwise-and 15 7 3) => 3)         ; 1111 & 0111 & 0011 = 0011
(check (bitwise-and #b101 #b011 #b111) => 1) ; 101 & 011 & 111 = 001
(check (bitwise-and #b1100 #b1010 #b0110) => 0) ; 1100 & 1010 & 0110 = 0000
(check (bitwise-and 255 127 63) => 63)    ; 11111111 & 01111111 & 00111111 = 00111111
(check (bitwise-and -1 -1 -1) => -1)      ; 全1相与
(check (bitwise-and 0 1 2) => 0)          ; 包含0的相与
(check (bitwise-and 1 1 1) => 1)          ; 全1相与
(check (bitwise-and 2 2 2) => 2)          ; 相同数相与

;;; 错误处理测试 - type-error
(check-catch 'type-error
             (bitwise-and "string" 1))  ; 字符串参数
(check-catch 'type-error
             (bitwise-and 1 'symbol))   ; 符号参数
(check-catch 'type-error
             (bitwise-and 3.14 2))      ; 浮点数参数
(check-catch 'type-error
             (bitwise-and #\a 1))       ; 字符参数
(check-catch 'type-error
             (bitwise-and '(1 2) 3))    ; 列表参数

;;; 多参数错误处理测试
(check-catch 'type-error
             (bitwise-and 1 2 3 "four"))  ; 第四个参数不是整数
(check-catch 'type-error
             (bitwise-and 1 2 "three" 4))  ; 第三个参数不是整数

#|
bitwise-ior
计算多个整数的按位或操作。

语法
----
(bitwise-ior i1 i2 ...)

参数
----
i1, i2, ... : integer?
一个或多个整数，参与按位或操作。

返回值
-----
integer?
返回所有整数按位或操作的结果。

说明
----
1. 对所有整数的每一位进行或操作（任意一个为1时结果为1，否则为0）
2. 按位或操作常用于设置特定位或合并位掩码
3. 对于任意整数 i，(bitwise-ior i i) = i
4. 对于任意整数 i，(bitwise-ior i 0) = i
5. 对于任意整数 i，(bitwise-ior i -1) = -1
6. 按位或操作满足交换律：(bitwise-ior i1 i2) = (bitwise-ior i2 i1)
7. 按位或操作满足结合律：(bitwise-ior i1 (bitwise-ior i2 i3)) = (bitwise-ior (bitwise-ior i1 i2) i3)
8. 支持两个或多个参数，按从左到右的顺序依次进行按位或操作

实现说明
--------
- bitwise-ior 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 如果考虑性能优化，可以使用 S7 Scheme 内置的 logior 函数
- logior 是 S7 的原生函数，通常比 bitwise-ior 有更好的性能

错误
----
type-error
当参数不是整数时抛出错误。
|#

;;; 基本功能测试：按位或操作
(check (bitwise-ior 5 3) => 7)  ; 5 (101) OR 3 (011) = 7 (111)
(check (bitwise-or 5 3) => 7)
(check (bitwise-ior 8 4) => 12) ; 8 (1000) OR 4 (0100) = 12 (1100)
(check (bitwise-ior #b101 #b011) => 7)  ; 5 (101) OR 3 (011) = 7 (111)
(check (bitwise-ior #b1000 #b0100) => 12) ; 8 (1000) OR 4 (0100) = 12 (1100)
(check (bitwise-ior #b1100 #b0001) => 13) ; 12 (1100) OR 1 (0001) = 13 (1101)

;;; 边界值测试
(check (bitwise-ior 0 0) => 0)          ; 0 OR 0 = 0
(check (bitwise-ior 0 1) => 1)          ; 0 OR 1 = 1
(check (bitwise-ior 1 0) => 1)          ; 1 OR 0 = 1
(check (bitwise-ior 1 1) => 1)          ; 1 OR 1 = 1
(check (bitwise-ior -1 -1) => -1)       ; -1 OR -1 = -1
(check (bitwise-ior -1 0) => -1)        ; -1 OR 0 = -1
(check (bitwise-ior 0 -1) => -1)        ; 0 OR -1 = -1

;;; 数学性质测试
(check (bitwise-ior 15 15) => 15)       ; 自反性
(check (bitwise-ior 7 3) => (bitwise-ior 3 7)) ; 交换律
(check (bitwise-ior 15 (bitwise-ior 7 3)) => (bitwise-ior (bitwise-ior 15 7) 3)) ; 结合律
(check (bitwise-ior 255 0) => 255)      ; 与0相或得原数
(check (bitwise-ior 255 -1) => -1)      ; 与-1相或得-1

;;; 二进制表示测试
(check (bitwise-ior #b10101010 #b01010101) => #b11111111)  ; 交替位模式
(check (bitwise-ior #b11110000 #b11001100) => #b11111100) ; 部分重叠
(check (bitwise-ior #b00001111 #b11110000) => #b11111111) ; 互补位模式

;;; 三个参数测试
(check (bitwise-ior 1 2 4) => 7)          ; 001 | 010 | 100 = 111
(check (bitwise-ior 1 1 1) => 1)          ; 全1相或
(check (bitwise-ior 0 1 2) => 3)          ; 包含0的相或
(check (bitwise-ior #b101 #b011 #b111) => 7) ; 101 | 011 | 111 = 111

;;; 错误处理测试 - type-error
;; (check-catch 'type-error
;;              (bitwise-ior "string" 1))  ; 字符串参数
;; (check-catch 'type-error
;;              (bitwise-ior 1 'symbol))   ; 符号参数

#|
bitwise-xor
计算多个整数的按位异或操作。

语法
----
(bitwise-xor i1 i2 ...)

参数
----
i1, i2, ... : integer?
一个或多个整数，参与按位异或操作。

返回值
-----
integer?
返回所有整数按位异或操作的结果。

说明
----
1. 对所有整数的每一位进行异或操作（相同为0，不同为1）
2. 按位异或操作常用于比较位差异或实现简单的加密
3. 对于任意整数 i，(bitwise-xor i i) = 0
4. 对于任意整数 i，(bitwise-xor i 0) = i
5. 对于任意整数 i，(bitwise-xor i -1) = (bitwise-not i)
6. 按位异或操作满足交换律：(bitwise-xor i1 i2) = (bitwise-xor i2 i1)
7. 按位异或操作满足结合律：(bitwise-xor i1 (bitwise-xor i2 i3)) = (bitwise-xor (bitwise-xor i1 i2) i3)
8. 支持两个或多个参数，按从左到右的顺序依次进行按位异或操作

实现说明
--------
- bitwise-xor 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 如果考虑性能优化，可以使用 S7 Scheme 内置的 logxor 函数
- logxor 是 S7 的原生函数，通常比 bitwise-xor 有更好的性能

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 基本功能测试：按位异或操作
(check (bitwise-xor 1 1) => 0)
(check (bitwise-xor #b10 #b11) => #b01) ; 2 xor 3 = 1
(check (bitwise-xor #b101010 #b110100) => #b011110) ; 42 xor 20 = 34
(check (bitwise-xor #b0 #b0) => #b0) ; 0 xor 0 = 0
(check (bitwise-xor #b1 #b1) => #b0) ; 1 xor 1 = 0
(check (bitwise-xor #b101 #b111) => #b010) ; 5 xor 7 = 2
(check (bitwise-xor #b1000 #b1001) => #b0001) ; 8 xor 9 = 1
(check (bitwise-xor #b10010101 #b01111001) => #b11101100)

;;; 边界值测试
(check (bitwise-xor 0 0) => 0)          ; 0 XOR 0 = 0
(check (bitwise-xor 0 1) => 1)          ; 0 XOR 1 = 1
(check (bitwise-xor 1 0) => 1)          ; 1 XOR 0 = 1
(check (bitwise-xor 1 1) => 0)          ; 1 XOR 1 = 0
(check (bitwise-xor -1 -1) => 0)        ; -1 XOR -1 = 0
(check (bitwise-xor -1 0) => -1)        ; -1 XOR 0 = -1
(check (bitwise-xor 0 -1) => -1)        ; 0 XOR -1 = -1

;;; 数学性质测试
(check (bitwise-xor 15 15) => 0)        ; 自反性
(check (bitwise-xor 7 3) => (bitwise-xor 3 7)) ; 交换律
(check (bitwise-xor 15 (bitwise-xor 7 3)) => (bitwise-xor (bitwise-xor 15 7) 3)) ; 结合律
(check (bitwise-xor 255 0) => 255)      ; 与0相异或得原数
(check (bitwise-xor 255 -1) => -256)    ; 与-1相异或得按位取反

;;; 二进制表示测试
(check (bitwise-xor #b10101010 #b01010101) => #b11111111)  ; 交替位模式
(check (bitwise-xor #b11110000 #b11001100) => #b00111100) ; 部分重叠
(check (bitwise-xor #b00001111 #b11110000) => #b11111111) ; 互补位模式

;;; 三个参数测试
(check (bitwise-xor 1 2 4) => 7)          ; 001 XOR 010 XOR 100 = 111
(check (bitwise-xor 1 1 1) => 1)          ; 001 XOR 001 XOR 001 = 001
(check (bitwise-xor 0 1 2) => 3)          ; 000 XOR 001 XOR 010 = 011
(check (bitwise-xor #b101 #b011 #b111) => 1) ; 101 XOR 011 XOR 111 = 001

;;; 特殊值测试
(check (bitwise-xor 2147483647 2147483647) => 0) ; 最大32位有符号整数
(check (bitwise-xor -2147483648 -2147483648) => 0) ; 最小32位有符号整数
(check (bitwise-xor 2147483647 -2147483648) => -1) ; 最大和最小整数相异或

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (bitwise-xor "string" 1))  ; 字符串参数
(check-catch 'wrong-type-arg
             (bitwise-xor 1 'symbol))   ; 符号参数
(check-catch 'wrong-type-arg
             (bitwise-xor 3.14 2))      ; 浮点数参数
(check-catch 'wrong-type-arg
             (bitwise-xor #\a 1))       ; 字符参数
(check-catch 'wrong-type-arg
             (bitwise-xor '(1 2) 3))    ; 列表参数

;;; 多参数错误处理测试
(check-catch 'wrong-type-arg
             (bitwise-xor 1 2 3 "four"))  ; 第四个参数不是整数
(check-catch 'wrong-type-arg
             (bitwise-xor 1 2 "three" 4))  ; 第三个参数不是整数

#|
bitwise-eqv
计算两个整数的按位等价操作。

语法
----
(bitwise-eqv i1 i2)

参数
----
i1, i2 : integer?
两个整数，参与按位等价操作。

返回值
-----
boolean?
返回两个整数按位等价操作的结果。

说明
----
1. 对两个整数的每一位进行等价操作（相同为1，不同为0）
2. 按位等价操作检查两个整数的所有位是否完全相同
3. 对于任意整数 i，(bitwise-eqv i i) = #t
4. 对于任意整数 i1 i2，(bitwise-eqv i1 i2) = (bitwise-eqv i2 i1)
5. 按位等价操作满足交换律：(bitwise-eqv i1 i2) = (bitwise-eqv i2 i1)
6. 按位等价操作常用于位模式的比较和验证
7. 与 bitwise-xor 的关系：当且仅当 (bitwise-xor i1 i2) = 0 时，(bitwise-eqv i1 i2) = #t

实现说明
--------
- bitwise-eqv 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 按位等价操作可以理解为 "位相等" 比较
- 在逻辑上，bitwise-eqv 等价于对 bitwise-xor 的结果取反

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 基本功能测试：按位等价操作
(check (bitwise-eqv 1 1) => #t)
(check (bitwise-eqv 1 2) => #f)
(check (bitwise-eqv -1 -1) => #t)
(check (bitwise-eqv -1 -2) => #f)
(check (bitwise-eqv 1 0) => #f)
(check (bitwise-eqv -1 0) => #f)
(check (bitwise-eqv #b1010 #b1010) => #t) ; 10 eqv 10 = #t
(check (bitwise-eqv #b1010 #b0101) => #f) ; 10 eqv 5 = #f

;;; 边界值测试
(check (bitwise-eqv 0 0) => #t)          ; 0 eqv 0 = #t
(check (bitwise-eqv 0 1) => #f)          ; 0 eqv 1 = #f
(check (bitwise-eqv 1 0) => #f)          ; 1 eqv 0 = #f
(check (bitwise-eqv 1 1) => #t)          ; 1 eqv 1 = #t
(check (bitwise-eqv -1 -1) => #t)        ; -1 eqv -1 = #t
(check (bitwise-eqv -1 0) => #f)         ; -1 eqv 0 = #f
(check (bitwise-eqv 0 -1) => #f)         ; 0 eqv -1 = #f

;;; 数学性质测试
(check (bitwise-eqv 15 15) => #t)        ; 自反性
(check (bitwise-eqv 7 3) => (bitwise-eqv 3 7)) ; 交换律
(check (bitwise-eqv 255 255) => #t)      ; 相同数等价
(check (bitwise-eqv 255 0) => #f)        ; 不同数不等价

;;; 二进制表示测试
(check (bitwise-eqv #b10101010 #b10101010) => #t)  ; 相同位模式
(check (bitwise-eqv #b10101010 #b01010101) => #f)  ; 相反位模式
(check (bitwise-eqv #b11110000 #b11110000) => #t)  ; 相同高4位
(check (bitwise-eqv #b11110000 #b00001111) => #f)  ; 互补位模式

;;; 与 bitwise-xor 的关系测试
(check (bitwise-eqv 5 3) => (zero? (bitwise-xor 5 3))) ; eqv 检查是否所有位都相同
(check (bitwise-eqv 10 10) => (zero? (bitwise-xor 10 10))) ; 相同数
(check (bitwise-eqv 7 2) => (zero? (bitwise-xor 7 2))) ; 不同数

;;; 特殊值测试
(check (bitwise-eqv 2147483647 2147483647) => #t) ; 最大32位有符号整数
(check (bitwise-eqv -2147483648 -2147483648) => #t) ; 最小32位有符号整数
(check (bitwise-eqv 2147483647 -2147483648) => #f) ; 最大和最小整数

;;; 错误处理测试 - wrong-type-arg
(check-catch 'wrong-type-arg
             (bitwise-eqv "string" 1))  ; 字符串参数
(check-catch 'wrong-type-arg
             (bitwise-eqv 1 'symbol))   ; 符号参数
(check-catch 'wrong-type-arg
             (bitwise-eqv 3.14 2))      ; 浮点数参数
(check-catch 'wrong-type-arg
             (bitwise-eqv #\a 1))       ; 字符参数
(check-catch 'wrong-type-arg
             (bitwise-eqv '(1 2) 3))    ; 列表参数

#|
bitwise-or
计算多个整数的按位或操作。

语法
----
(bitwise-or i1 i2 ...)

参数
----
i1, i2, ... : integer?
一个或多个整数，参与按位或操作。

返回值
-----
integer?
返回所有整数按位或操作的结果。

说明
----
1. 对所有整数的每一位进行或操作（任意一个为1时结果为1，否则为0）
2. bitwise-or 是 bitwise-ior 的别名，两者功能完全相同
3. 支持两个或多个参数，按从左到右的顺序依次进行按位或操作

实现说明
--------
- bitwise-or 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 作为 bitwise-ior 的别名，提供更简洁的函数名

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 精简测试：bitwise-or 作为 bitwise-ior 的别名
(check (bitwise-or 5 3) => 7)  ; 5 (101) OR 3 (011) = 7 (111)
(check (bitwise-or 8 4) => 12) ; 8 (1000) OR 4 (0100) = 12 (1100)
(check (bitwise-or 1 2 4) => 7) ; 001 | 010 | 100 = 111

;;; 验证 bitwise-or 与 bitwise-ior 功能相同
(check (bitwise-or 5 3) => (bitwise-ior 5 3))
(check (bitwise-or 8 4) => (bitwise-ior 8 4))
(check (bitwise-or 1 2 4) => (bitwise-ior 1 2 4))

#|
bitwise-nor
计算两个整数的按位或非操作。

语法
----
(bitwise-nor i1 i2)

参数
----
i1, i2 : integer?
两个整数，参与按位或非操作。

返回值
-----
integer?
返回两个整数按位或非操作的结果。

说明
----
1. 对两个整数的每一位进行或非操作（或操作后取反）
2. 按位或非操作等价于 (bitwise-not (bitwise-ior i1 i2))
3. 对于任意整数 i1 i2，(bitwise-nor i1 i2) = (bitwise-nor i2 i1)
4. 按位或非操作常用于逻辑电路设计和位掩码操作

实现说明
--------
- bitwise-nor 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 按位或非操作是或操作和取反操作的组合

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 精简测试：bitwise-nor 按位或非操作
(check (bitwise-nor 2 4) => -7)  ; 2 (010) NOR 4 (100) = -7 (11111001)
(check (bitwise-nor 3 1) => -4)  ; 3 (011) NOR 1 (001) = -4 (11111100)
(check (bitwise-nor #b111 #b011) => -8)  ; 7 (111) NOR 3 (011) = -8 (11111000)

#|
bitwise-nand
计算两个整数的按位与非操作。

语法
----
(bitwise-nand i1 i2)

参数
----
i1, i2 : integer?
两个整数，参与按位与非操作。

返回值
-----
integer?
返回两个整数按位与非操作的结果。

说明
----
1. 对两个整数的每一位进行与非操作（与操作后取反）
2. 按位与非操作等价于 (bitwise-not (bitwise-and i1 i2))
3. 对于任意整数 i1 i2，(bitwise-nand i1 i2) = (bitwise-nand i2 i1)
4. 按位与非操作常用于逻辑电路设计和位掩码操作

实现说明
--------
- bitwise-nand 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 按位与非操作是与操作和取反操作的组合

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 精简测试：bitwise-nand 按位与非操作
(check (bitwise-nand 1 1) => -2)  ; 1 (001) NAND 1 (001) = -2 (11111110)
(check (bitwise-nand 3 1) => -2)  ; 3 (011) NAND 1 (001) = -2 (11111110)
(check (bitwise-nand #b110 #b001) => -1)    ; 6 (110) NAND 1 (001) = -1 (11111111)

#|
bit-count
计算整数中值为1的位数。

语法
----
(bit-count i)

参数
----
i : integer?
整数，要计算值为1的位数的整数。

返回值
-----
integer?
返回整数 i 中值为1的位数。

说明
----
1. 计算整数二进制表示中值为1的位数
2. 对于非负整数，返回值为1的位数
3. 对于负整数，返回值为0的位数
4. 常用于计算汉明权重或位密度

实现说明
--------
- bit-count 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 对于非负整数，计算值为1的位数
- 对于负整数，计算值为0的位数

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 精简测试：bit-count 位数计算
(check (bit-count 0) =>  0)
(check (bit-count -1) =>  0)
(check (bit-count 7) =>  3)
(check (bit-count  13) =>  3)
(check (bit-count -13) =>  2)

#|
bitwise-orc1
计算两个整数的按位或非操作（第一个参数取反）。

语法
----
(bitwise-orc1 i1 i2)

参数
----
i1, i2 : integer?
两个整数，参与按位或非操作。

返回值
-----
integer?
返回两个整数按位或非操作的结果。

说明
----
1. 对两个整数的每一位进行或非操作（第一个参数取反后与第二个参数进行或操作）
2. 按位或非操作等价于 (bitwise-ior (bitwise-not i1) i2)
3. 对于任意整数 i1 i2，(bitwise-orc1 i1 i2) = (bitwise-orc1 i2 i1)
4. 按位或非操作常用于逻辑电路设计和位掩码操作

实现说明
--------
- bitwise-orc1 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 按位或非操作是取反操作和或操作的组合

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 精简测试：bitwise-orc1 按位或非操作
(check (bitwise-orc1 1 1) => -1)
(check (bitwise-orc1 3 1) => -3)
(check (bitwise-orc1 11 26) => -2)
(check (bitwise-orc1 #b110 #b001) => -7)

#|
bitwise-orc2
计算两个整数的按位或非操作（第二个参数取反）。

语法
----
(bitwise-orc2 i1 i2)

参数
----
i1, i2 : integer?
两个整数，参与按位或非操作。

返回值
-----
integer?
返回两个整数按位或非操作的结果。

说明
----
1. 对两个整数的每一位进行或非操作（第一个参数与第二个参数取反后进行或操作）
2. 按位或非操作等价于 (bitwise-ior i1 (bitwise-not i2))
3. 对于任意整数 i1 i2，(bitwise-orc2 i1 i2) = (bitwise-orc2 i2 i1)
4. 按位或非操作常用于逻辑电路设计和位掩码操作

实现说明
--------
- bitwise-orc2 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 按位或非操作是或操作和取反操作的组合

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 精简测试：bitwise-orc2 按位或非操作
(check (bitwise-orc2 11 26) => -17)
(check (bitwise-orc2 3 1) => -1)
(check (bitwise-orc2 #b110 #b001) => -2)
(check (bitwise-orc2 #b1001 #b0111) => -7)

#|
bitwise-andc2
计算两个整数的按位与非操作（第二个参数取反）。

语法
----
(bitwise-andc2 i1 i2)

参数
----
i1, i2 : integer?
两个整数，参与按位与非操作。

返回值
-----
integer?
返回两个整数按位与非操作的结果。

说明
----
1. 对两个整数的每一位进行与非操作（第一个参数与第二个参数取反后进行与操作）
2. 按位与非操作等价于 (bitwise-and i1 (bitwise-not i2))
3. 按位与非操作常用于逻辑电路设计和位掩码操作

实现说明
--------
- bitwise-andc2 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 按位与非操作是与操作和取反操作的组合

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

#|
arithmetic-shift
对整数进行算术移位操作。

语法
----
(arithmetic-shift i count)

参数
----
i : integer?
要进行移位操作的整数。
count : integer?
移位位数，正数表示左移，负数表示右移。

返回值
-----
integer?
返回整数 i 算术移位 count 位后的结果。

说明
----
1. 对整数 i 进行算术移位操作
2. 当 count > 0 时，向左移位（相当于乘以 2^count）
3. 当 count < 0 时，向右移位（相当于除以 2^|count|，保留符号位）
4. 算术移位会保留整数的符号位

实现说明
--------
- arithmetic-shift 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 算术移位操作保持整数的符号位不变

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

#|
bitwise-andc1
计算两个整数的按位与非操作（第一个参数取反）。

语法
----
(bitwise-andc1 i1 i2)

参数
----
i1, i2 : integer?
两个整数，参与按位与非操作。

返回值
-----
integer?
返回两个整数按位与非操作的结果。

说明
----
1. 对两个整数的每一位进行与非操作（第一个参数取反后与第二个参数进行与操作）
2. 按位与非操作等价于 (bitwise-and (bitwise-not i1) i2)
3. 对于任意整数 i1 i2，(bitwise-andc1 i1 i2) = (bitwise-andc1 i2 i1)
4. 按位与非操作常用于逻辑电路设计和位掩码操作

实现说明
--------
- bitwise-andc1 是 SRFI 151 标准定义的函数，提供标准化的位运算接口
- 按位与非操作是取反操作和与操作的组合

错误
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

;;; 精简测试：bitwise-andc1 按位与非操作
(check (bitwise-andc1 11 26) => 16)
(check (bitwise-andc1 5 3) => 2)
(check (bitwise-andc1 #b1100 #b1010) => 2)
(check (bitwise-andc1 0 15) => 15)

(check (bitwise-andc2 11 26) => 1)
(check (bitwise-andc2 5 3) => 4)
(check (bitwise-andc2 #b1100 #b1010) => 4)
(check (bitwise-andc2 0 15) => 0)
(check (bitwise-andc2 15 0) => 15)
(check (bitwise-andc2 7 1) => 6)

(check (arithmetic-shift #b10 -1) => #b1) ; 2 >> 1 = 1
(check (arithmetic-shift #b10 1) => #b100) ; 2 << 1 = 4
(check (arithmetic-shift #b1000 -2) => #b10) ; 8 >> 2 = 2
(check (arithmetic-shift #b1000 2) => #b100000)
(check (arithmetic-shift #b10000000000000000 -3) => #b10000000000000)
(check (arithmetic-shift #b1000000000000000 3) => #b1000000000000000000)

(check (integer-length 0) => 0)
(check (integer-length 1) => 1)     ; 1
(check (integer-length 3) => 2)     ; 11
(check (integer-length 4) => 3)     ; 100
(check (integer-length -5) => 3)    ; -101 (长度为3)
(check (integer-length #xFFFF) => 16) ; 16位二进制

(check (bitwise-if 3 1 8) => 9)  ; #b011 #001 #100 => #101
(check (bitwise-if 3 8 1) => 0)  ; #011 #100 #001 => #000
(check (bitwise-if 1 1 2) => 3)  ; #001 #001 #010 => #011
(check (bitwise-if #b00111100 #b11110000 #b00001111) => #b00110011)  ; 60 240 15 => 51

(check (bit-set? 1 1) => #f)        ; Binary of 1 is #b0001, bit 1 is 0
(check (bit-set? 0 1) => #t)        ; Binary of 1 is #b0001, bit 0 is 1
(check (bit-set? 3 10) => #t)       ; Binary of 10 is #b1010, bit 3 is 1
(check (bit-set? 2 6) => #t)        ; Binary of 6 is #b0110, bit 2 is 1
(check (bit-set? 0 6) => #f)        ; Binary of 6 is #b0110, bit 0 is 0
(check (bit-set? 63 -1) => #t)
(check (bit-set? 63 1) => #f)
(check-catch 'out-of-range
             (bit-set? -1 1))       ; index cannot be negative
(check-catch 'out-of-range
             (bit-set? 64 1))       ; index cannot exceed 63

(check (copy-bit 0 0 #t) => #b1)         ; Set bit 0 of 0 to 1, result is #b1
(check (copy-bit 2 0 #t) => #b100)       ; Set bit 2 of #000 to 1, result is #b100
(check (copy-bit 2 #b1111 #f) => #b1011) ; Set bit 2 of #b1111 to 0, result is #b1011
(check (copy-bit 62 0 #t) => #x4000000000000000)
(check (copy-bit 63 1 #t) => #x8000000000000001)
(check (copy-bit 63 -1 #f) => #x7FFFFFFFFFFFFFFF)
(check-catch 'out-of-range
             (copy-bit 64 -1 #f))        ; index cannot exceed 63
(check-catch 'out-of-range
             (copy-bit 10000 -1 #f))     ; index cannot exceed 63
(check-catch 'out-of-range
             (copy-bit -1 1 #t))         ; index cannot be negative

(check (bit-swap 0 2 4) => #b1)
(check (bit-swap 3 0 5) => #b1100)
(check (bit-swap 63 0 1) => #x8000000000000000)
(check-catch 'out-of-range
             (bit-swap 64 0 1))          ; index cannot exceed 63
(check-catch 'out-of-range
             (bit-swap -1 1 3))          ; index cannot be negative  

(check (any-bit-set? 3 6) => #t)
(check (any-bit-set? 3 12) => #f)

(check (every-bit-set? 4 6) => #t)
(check (every-bit-set? 7 6) => #f)

(check (first-set-bit 1) => 0)
(check (first-set-bit 2) => 1)
(check (first-set-bit 0) => -1)
(check (first-set-bit 40) => 3)
(check (first-set-bit -28) => 2)
(check (first-set-bit (expt  2 62)) => 62)
(check (first-set-bit (expt -2 62)) => 62)

(check (bit-field #b1101101010 0 4) => #b1010 )
(check (bit-field #b1101101010 3 9) => #b101101 )
(check (bit-field #b1101101010 4 9) => #b10110 )
(check (bit-field #b1101101010 4 10) => #b110110 )
(check (bit-field 6 0 1) => 0 )    ; #110 => #0
(check (bit-field 6 1 3) => 3 )    ; #110 => #11
(check (bit-field 6 2 999) => 1 )  ; 超出整数长度的部分截断
(check-catch 'out-of-range
             (bit-field #x100000000000000000000000000000000 128 129))       ; start 超过64位整数范围

(check (bit-field-any? #b1001001 1 6) => #t)
(check (bit-field-any? #b1000001 1 6) => #f)

(check (bit-field-every? #b1011110 1 5) => #t)
(check (bit-field-every? #b1011010 1 5) => #f)

(check (bit-field-clear #b101010 1 4) => #b100000)

(check (bit-field-set #b101010 1 4) => #b101110)

(check-report)

