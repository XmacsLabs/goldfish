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
        (scheme base)
        (liii rich-string)
        (liii lang)
        (liii error))

(check-set-mode! 'report-failed)

#|
rich-string%length
获取rich-string对象中Unicode字符的数量。

语法
----
(rich-string-instance :length)
($ "string-content" :length)
($ "string-content" :method1 arg1_1 arg1_2 :method2 arg2 :length)

参数
----
无参数。

返回值
-----
以integer形式返回rich-string对象中Unicode字符的数量。

说明
----
该方法返回rich-string对象中Unicode字符的数量，而不是字节长度。
对于空字符串返回0，对于包含Unicode字符的字符串返回实际的字符数量。

边界条件
--------
- 空字符串：返回0
- ASCII字符串：字符数量等于字符串长度
- Unicode字符串：返回实际的Unicode字符数量（可能小于字节长度）
- 混合字符：正确计算所有Unicode字符的数量

性能特征
--------
- 时间复杂度：O(1)，长度在对象创建时已计算并缓存
- 空间复杂度：O(1)，直接返回缓存的长度值

兼容性
------
- 与所有rich-string实例兼容
- 返回标准整数，可与任何数值操作配合使用
|#

;; 基本功能测试
;; 空字符串
(check ((rich-string :empty) :length) => 0)

;; 单字符字符串
(check ($ "a" :length) => 1)

;; 多字符ASCII字符串
(check ($ "hello" :length) => 5)

;; Unicode字符测试
(check ($ "测试" :length) => 2)
(check ($ "🎉🎊" :length) => 2)

;; 混合字符
(check ($ "hello 世界 🎉" :length) => 10)

;; 链式操作后长度验证
(check ($ "hello" :+ " world" :length) => 11)

;; 长字符串测试
(check ($ (make-string 1000 #\a) :length) => 1000)

(check-report)
