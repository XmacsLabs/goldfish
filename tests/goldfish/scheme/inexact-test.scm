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
        (scheme inexact))

(check-set-mode! 'report-failed)
(check (nan? +nan.0) => #t)
(check (nan? +nan.0+5.0i) => #t)
       
(check (nan? 32) => #f)
(check (nan? 3.14) => #f)
(check (nan? 1+2i) => #f)
(check (nan? +inf.0) => #f)
(check (nan? -inf.0) => #f)
       
(check (sqrt 9) => 3)              
(check (sqrt 25.0) => 5.0)
(check (sqrt 9/4) => 3/2) 
(check (< (abs (- (sqrt 2.0) 1.4142135623730951)) 1e-10) => #t)
       
(check (sqrt -1.0) => 0.0+1.0i)
(check (sqrt -1) => 0.0+1.0i)
       
(check (sqrt 0) => 0)
(check (sqrt 0.0) => 0.0)
       
(check (exact? (sqrt 2.0)) => #f) 
(check (exact? (sqrt -1)) => #f) 
(check (exact? (sqrt -1.0)) => #f) 

#|
infinite?

判断一个数值是否无限。

语法
----
(infinite? obj)

参数
----
obj : number?
要判断的数值。支持整数、浮点数、有理数、复数。

返回值
-----
boolean?
- 若 obj 是数值，且其实部或虚部中存在 +inf.0 或 -inf.0，返回 #t。
- 否则返回 #f。

错误
----
无错误情况，非数值将返回 #f。
|#
(check (infinite? 0) => #f)
(check (infinite? 0.0) => #f)
(check (infinite? 1/2) => #f)
(check (infinite? 1/2+i) => #f)
(check (infinite? 1+1/2i) => #f)
(check (infinite? 1+2i) => #f)
(check (infinite? 1.0+2.0i) => #f)
(check (infinite? +inf.0) => #t)
(check (infinite? -inf.0) => #t)
(check (infinite? +inf.0+2.0i) => #t)
(check (infinite? +inf.0+2i) => #t)
(check (infinite? +inf.0+1/2i) => #t)
(check (infinite? 2.0-inf.0i) => #t)
(check (infinite? 2-inf.0i) => #t)
(check (infinite? 1/2-inf.0i) => #t)
(check (infinite? +inf.0-inf.0i) => #t)
(check (infinite? -inf.0+inf.0i) => #t)
(check (infinite? +nan.0) => #f)
(check (infinite? -nan.0) => #f)
(check (infinite? (* +nan.0 2.0)) => #f)
(check (infinite? (* 0.0 +nan.0)) => #f)
(check (infinite? +nan.0+5.0i) => #f)
(check (infinite? 5.0+nan.0i) => #f)
(check (infinite? +nan.0+5i) => #f)
(check (infinite? 5+nan.0i) => #f)
(check (infinite? +nan.0+2/5i) => #f)
(check (infinite? 2/5+nan.0i) => #f)
(check (infinite? #t) => #f)
(check (infinite? "hello") => #f)
(check (infinite? 'symbol) => #f)
(check (infinite? '(+inf.0)) => #f)
(check (infinite? '#(+inf.0)) => #f)

#|
finite?

判断一个数值是否有限。

语法
----
(finite? obj)

参数
----
obj : any
任意类型的对象。

返回值
-----
boolean?
- 若 obj 是数值，且实部与虚部都为有限数，返回 #t。
- 若是非数值、包含 inf.0、nan.0 的实部或虚部，返回 #f。

错误
----
无错误情况，非数值将返回 #f。
|#
(check (finite? 0) => #t)
(check (finite? 0.0) => #t)
(check (finite? 1/2) => #t)
(check (finite? 1/2+i) => #t)
(check (finite? 1+1/2i) => #t)
(check (finite? 1+2i) => #t)
(check (finite? 1.0+2.0i) => #t)
(check (finite? +inf.0) => #f)
(check (finite? -inf.0) => #f)
(check (finite? +inf.0+2.0i) => #f)
(check (finite? +inf.0+2i) => #f)
(check (finite? +inf.0+1/2i) => #f)
(check (finite? 2.0-inf.0i) => #f)
(check (finite? 2-inf.0i) => #f)
(check (finite? 1/2-inf.0i) => #f)
(check (finite? +inf.0-inf.0i) => #f)
(check (finite? -inf.0+inf.0i) => #f)
(check (finite? +nan.0) => #f)
(check (finite? -nan.0) => #f)
(check (finite? (* +nan.0 2.0)) => #f)
(check (finite? (* 0.0 +nan.0)) => #f)
(check (finite? +nan.0+5.0i) => #f)
(check (finite? 5.0+nan.0i) => #f)
(check (finite? +nan.0+5i) => #f)
(check (finite? 5+nan.0i) => #f)
(check (finite? +nan.0+2/5i) => #f)
(check (finite? 2/5+nan.0i) => #f)
(check (finite? #t) => #f)
(check (finite? "hello") => #f)
(check (finite? 'symbol) => #f)
(check (finite? '(+inf.0)) => #f)
(check (finite? '#(+inf.0)) => #f)

(check-catch 'wrong-type-arg  (sqrt "hello"))
(check-catch 'wrong-type-arg  (sqrt 'symbol))
       
(check-report)
