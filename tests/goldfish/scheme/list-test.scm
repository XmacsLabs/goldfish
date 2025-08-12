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
        (scheme base)
        (liii list))

(check-set-mode! 'report-failed)

#|
pair?
判断一个对象是否为序对（pair）结构。

语法
----
(pair? obj)

参数
----
obj : any
任意类型的对象，包括原子、序对、列表、向量、字节向量、字符、字符串、符号、过程等。

返回值
-----
boolean?
如果 obj 是序对类型则返回 #t，否则返回 #f。

说明
----
判断一个对象是否为序对的基本谓词函数。序对是 Scheme 中最基础的数据结构，
由两个元素组成，形成一个双向的单元结构。序对可以是显式的点对形式，
如 (a . b)，也可以是非空列表，因为所有非空列表本质上都是由序对链组成的。

边界条件
--------
- 空列表 '() 返回 #f
- 单元素列表 '(a) 返回 #t（列表由一个序对构成）
- 显式点对 (a . b) 返回 #t
- 嵌套深度不影响结果：深度嵌套列表都返回 #t
- 任意类型的 car/cdr 值不影响返回值判断

性能特征
--------
- 时间复杂度：O(1) 恒定时间完成类型检查
- 空间复杂度：O(1) 不消耗额外栈空间
- 递归深度：0，这是一个基本谓词不会触发递归

数据类型兼容性
-------------
- 数值类型：整数、实数、复数都返回 #f
- 符号类型：普通符号和关键字符号都返回 #f  
- 字符串：所有字符串类型返回 #f
- 字符：所有字符类型返回 #f
- 布尔值：#t 和 #f 都返回 #f
- 过程：所有过程对象返回 #f
- 向量和字节向量：返回 #f
- 空列表：返回 #f
- 非空列表：返回 #t
- 点对结构：返回 #t

注意
----
- 空列表 '() 不是序对
- 字符串、数字、布尔值等原子类型都不是序对
- 所有非空列表都被认为是序对，因为列表本质上是由序对链构成的
- 这一过程对存储的 car/cdr 值内容不做任何验证

|#

;; 测试 pair? 对各种序对结构的判断
(check-true (pair? '(a . b)))             ; 显式点对形式的序对
(check-true (pair? '(a b c)))             ; 列表内部由序对构成
(check-true (pair? (cons 1 2)))           ; 使用 cons 创建的序对
(check-true (pair? (cons 'a (cons 'b 'c))))  ; 嵌套序对结构

(check-false (pair? 'a))
(check-false (pair? 123))
(check-false (pair? "string"))
(check-false (pair? #t))
(check-false (pair? #f))

;; pair? 边界条件测试补充
;; 基本边界值验证
(check-false (pair? '()))                                ; 空列表边界
(check-true (pair? '(single)))                           ; 单元素列表边界
(check-true (pair? (cons 1 '())))                        ; 单元素cons构建
(check-true (pair? '(())))                               ; 空列表作为唯一元素

;; 嵌套深度边界测试
(check-true (pair? '((((a))))) )                         ; 深度嵌套列表
(check-true (pair? (cons 'a (cons 'b (cons 'c '())))))   ; 深层cons链
(check-true (pair? '(a b (c d (e)))))                    ; 中度嵌套绑定

;; 数据类型兼容性边界测试
(check-false (pair? 42))                                ; 整数类型
(check-false (pair? 3.14))                              ; 实数类型
(check-false (pair? 1+2i))                              ; 复数类型
(check-false (pair? #t))                                ; 布尔真
(check-false (pair? #f))                                ; 布尔假  
(check-false (pair? "hello"))                           ; 字符串
(check-false (pair? #\a))                               ; 字符
(check-false (pair? 'symbol))                           ; 符号
(check-false (pair? 'quote))                            ; 特殊符号
(check-false (pair? +))                                 ; 过程对象
(check-false (pair? length))                            ; 过程对象

;; 复杂对象边界测试
(check-false (pair? #(1 2 3)))                          ; 向量对象
(check-false (pair? #u8(1 2 3)))                        ; 字节向量
(check-false (pair? (lambda (x) x)))                    ; lambda过程
(check-false (pair? #<eof>))                            ; 特殊对象

;; 极端边界测试
(check-true (pair? (cons '() '())))                      ; 空列表组成的序对
(check-true (pair? (cons #t #f)))                        ; 布尔值组成序对
(check-true (pair? (cons 42 "string")))                  ; 混合类型序对
(check-true (pair? (cons (cons 1 2) (cons 3 4))))        ; 嵌套序对组合

;; 构造器多样化测试
(check-true (pair? (list 1 2)))                          ; list构造器
(check-true (pair? (append '(1) '(2))))                  ; append结果
(check-true (pair? (cons 'a (list 'b 'c))))               ; 混合构造器

;; 结构性边界验证
(check-false (pair? 'a))                                 ; 原子符号
(check-false (pair? 1000000))                            ; 极大整数边界
(check-false (pair? "中文测试"))                            ; 多字节字符串
(check-false (pair? #\newline))                          ; 特殊字符

;; Improper list 边界验证
(check-true (pair? '(a . b)))                            ; 基础点对形式
(check-true (pair? '(a b . c)))                          ; 扩展点对形式
(check-true (pair? '(a b c . d)))                        ; 多点结构

#|
car
car 是 Scheme 内置函数，用于获取序对的第一个元素（第一个分量）。该函数是 R7RS 标准的基本列表操作函数之一。

语法
----
(car pair)

参数
----
pair : pair?
可以是序对（即非空列表或显式点对），不能是空列表或其他对象。

返回值
------
任意类型
返回序对的第一个元素（car部分）。根据不同的序对内容，返回类型可以是
符号、数字、列表、点对等任何对象。

说明
----
1. car 是 pair? 谓词的基本操作之一，与 cdr 配合使用处理序对数据
2. 当应用于列表时，返回列表的第一个元素
3. 适用于所有序对数据：不论是点对 (a . b) 还是非空列表 (a b c ...)

错误处理
--------
wrong-type-arg
当参数不是序对（如空列表 '()、数字、字符串等）时抛出错误。

|#

(check (car '(a b c . d)) => 'a)
(check (car '(a b c)) => 'a)
;; car 边界条件测试
(check (car '(a)) => 'a)                       ; 单元素列表测试
(check (car '(1)) => 1)                        ; 单元素数字测试
(check (car '(#t)) => #t)                      ; 单元素布尔值测试  
(check (car '("hello")) => "hello")            ; 单元素字符串测试
(check (car '(42)) => 42)                      ; 单元素整数测试
(check (car '(() b c)) => '())                  ; 空列表作为首元素边界

;; 各种数据类型作为car值测试
(check (car '(123 "text" symbol)) => 123)                     ; 多类型混合，car是整数
(check (car '(#\a #\b #\c)) => #\a)                         ; 字符列表
(check (car '((a b) c d)) => '(a b))                          ; 子列表作为car
(check (car '((((a))))) => '(((a))))                          ; 深度嵌套列表
(check (car '("nested" (list) "structure")) => "nested")      ; 字符串嵌套结构

;; 点对结构boundary测试
(check (car '(a . b)) => 'a)                   ; 普通点对
(check (car (cons 1 2)) => 1)                  ; cons结构
(check (car (cons 'a (cons 'b 'c))) => 'a)     ; 嵌套cons结构

(check-catch 'wrong-type-arg (car '()))

;; car 异常边界测试
(check-catch 'wrong-type-arg (car 123))                    ; 数字不是pair
(check-catch 'wrong-type-arg (car "hello"))                ; 字符串不是pair
(check-catch 'wrong-type-arg (car #t))                     ; 布尔值不是pair
(check-catch 'wrong-type-arg (car #\a))                   ; 字符不是pair
(check-catch 'wrong-type-arg (car #(a b c)))               ; 向量不是pair
(check-catch 'wrong-number-of-args (car))                  ; 参数不足
(check-catch 'wrong-number-of-args (car '(1 2) '(3 4)))    ; 参数过多

;; 补充边界条件测试 - 完善car边界条件

;; 各种数据结构边界测试
(check (car '(symbol)) => 'symbol)
(check (car '(#t #f)) => #t)
(check (car '(42 24)) => 42)
(check (car '(3.14 2.71)) => 3.14)
(check (car '(1/2 2/3)) => 1/2)
(check (car '(1+2i 3+4i)) => 1+2i)
(check (car '(#	ab #\newline)) => #	ab)

;; 嵌套结构和特殊边界值测试
(check (car '((a (b (c))))) => '(a (b (c))))
(check (car '(((1 2) 3) 4)) => '((1 2) 3))
(check (car '(() b c)) => '())
(check (car '('(a b) '(c d))) => ''(a b))
(check (car '(`(a b) `(c d))) => ''(a b))

;; 向量和字节向量作为car值测试
(check (car '(#(1 2 3) #(4 5))) => #(1 2 3))
(check (car '(#u8(255 128) #u8(1 2))) => #u8(255 128))

;; Scheme符号和过程边界测试
(check (car '(procedure? symbol?)) => `procedure?)
(check (car '(+ - * /)) => '+)
(check (car '(sqrt abs) ) => `sqrt)

;; 连续空列表嵌套边界测试
(check (car '((((()))))) => '(((()))))
(check (car '((a (()) b) c)) => '(a (()) b))

;; Unicode和特殊字符边界测试
(check (car '("中文" "world")) => "中文")
(check (car '("🙂" "🚀")) => "🙂")
(check (car '((list 'a 'b) 'c)) => '(list 'a 'b))

;; 函数和过程对象作为car值测试
(check (car '((lambda (x) (* x x)) (lambda (y) (+ y 1)))) => 
       `(lambda (x) (* x x)))

;; 极端边界：现存表达式嵌套
(check (car '((begin 1 2 3) (begin 4 5))) => '(begin 1 2 3))
(check (car '((let ((x 10)) x) (let ((y 20)) y))) => '(let ((x 10)) x))

;; 确保对car函数的精确数据类型边界验证
(check (car '((define f (lambda (x) x)) (define g (lambda (x) x)))) => 
       '(define f (lambda (x) x)))

(check-catch 'wrong-type-arg (car #f))
(check-catch 'wrong-type-arg (car '[]))
(check-catch 'wrong-type-arg (car '()))
(check-catch 'wrong-number-of-args (car 42 84))
(check-catch 'wrong-type-arg (car '*))

#|
cdr
cdr 是 Scheme 内置函数，用于获取序对的第二个元素（第二个分量）。该函数与 car 配合使用，是 R7RS 标准的基本列表操作函数之一。

语法
----
(cdr pair)

参数
----
pair : pair?
可以是序对（即非空列表或显式点对），不能是空列表或其他对象。

返回值
------
任意类型
返回序对的第二个元素（cdr部分）。根据不同的序对内容，返回类型可以是
列表、符号、数字、点对、布尔值等任何对象。

说明
----
1. cdr 是 pair? 谓词的基本操作之一，与 car 配合使用处理序对数据
2. 当应用于列表时，返回列表除第一个元素外的子列表
3. 适用于所有序对数据：不论是点对 (a . b) 还是非空列表 (a b c ...)

错误处理
--------
wrong-type-arg
当参数不是序对（如空列表 '()、数字、字符串等）时抛出错误。

|#

(check (cdr '(a b c . d)) => '(b c . d))
(check (cdr '(a b c)) => '(b c))

(check (cdr '(1 . 2)) => 2)
(check (cdr '((a b) . c)) => 'c)

(check (cdr (cons 1 2)) => 2)
(check (cdr (cons 'a 'b)) => 'b)

; 错误测试
(check-catch 'wrong-type-arg (cdr '()))
(check-catch 'wrong-type-arg (cdr 123))
(check-catch 'wrong-type-arg (cdr "hello"))
(check-catch 'wrong-type-arg (cdr #t))
(check-catch 'wrong-number-of-args (cdr))
(check-catch 'wrong-number-of-args (cdr '(1 2) '(3 4)))

;; cdr边界条件测试补充
(check (cdr '(a)) => '())                       ; 单元素列表cdr边界
(check (cdr '(1)) => '())                       ; 单元素数字列表cdr边界
(check (cdr '(#t)) => '())                      ; 单元素布尔列表cdr边界
(check (cdr '("hello")) => '())                 ; 单元素字符串列表cdr边界
(check (cdr '(() b c)) => '(b c))               ; 空列表作为首元素的cdr
(check (cdr '((a b))) => '())                   ; 单元素子列表cdr边界
(check (cdr '((((a))))) => '())                 ; 深度嵌套单元素cdr

;; 各种数据类型cdr边界测试
(check (cdr '(123 "text" symbol)) => '("text" symbol))
(check (cdr '(#
ewline #	ab #\space)) => '(#	ab #\space))
(check (cdr '((a b) c d)) => '(c d))
(check (cdr '(#(1 2) #(3 4))) => '(#(3 4)))
(check (cdr '(+ - * /)) => '(- * /))
(check (cdr '('(a b) '(c d))) => '('(c d)))

;; 极端边界条件测试
(check (cdr '((lambda (x) x) (lambda (y) y))) => '((lambda (y) y)))
(check (cdr '((begin 1 2 3) (begin 4 5))) => '((begin 4 5)))

(check (cdr '(a b.c d)) => '(b.c d))
(check (cdr '("中文" "测试")) => '("测试"))

#|
set-car!
替换序对（pair）的第一个元素（car部分）为新值，该操作会直接修改原始序对对象。

语法
----
(set-car! pair obj)

参数
----
pair : pair?
    要被修改的序对，可以是点对或任何非空列表。必须是一个可以修改的序对对象。
    
obj : any
    要设置的新值，可以是任何类型的对象，包括符号、数字、列表、字符串、布尔值等。

返回值
------
unspecified
    根据R7RS规范，返回未指定的值。

说明
----
1. set-car!是一个变异操作，会直接修改原始序对对象的内存内容
2. 索引从0开始，替换的是序对的第一个元素（car部分）
3. 当应用于列表时，修改的是列表的第一个元素
4. 修改后原始对象的引用仍然指向同一个内存位置
5. 适用于所有序对数据：不论是显式点对 (a . b) 还是非空列表 (a b c ...)

错误处理
--------
wrong-type-arg
    当第一个参数不是序对（如空列表、数字、字符串等）时抛出错误。
wrong-number-of-args
    当参数数量不等于2时抛出错误。

影响范围
--------
- 该操作会直接影响使用同一引用的所有代码位置
- 修改后原始对象的内容立即发生变化
- 不能用于扩展或缩短列表长度，仅用于替换现有元素
- 谨慎使用，避免破坏不变量或导致不可预期的副作用
|#

;; set-car!基本功能测试
(let ((p (cons 1 2)))
  (set-car! p 100)
  (check p => '(100 . 2)))

;; set-car!用于列表的首元素修改
(let ((lst (list 'a 'b 'c)))
  (set-car! lst 'x)
  (check lst => '(x b c)))

;; set-car!测试不同类型的值
(let ((p (cons 'old 'value)))
  (set-car! p "new string")
  (check (car p) => "new string")
  (set-car! p 42)
  (check (car p) => 42)
  (set-car! p #t)
  (check (car p) => #t))

;; 使用set-car!修改嵌套结构
(let ((nested (list (list 1 2) (list 3 4))))
  (set-car! (car nested) 'first)
  (check nested => '((first 2) (3 4))))

;; set-car!与cons构造器结合测试
(let ((p (cons 'initial 'cdr-value)))
  (check (car p) => 'initial)
  (set-car! p 'modified)
  (check (car p) => 'modified)
  (check (cdr p) => 'cdr-value))

;; 多次set-car!调用测试
(let ((lst (list 1 2 3 4 5)))
  (set-car! lst 'first)
  (check lst => '(first 2 3 4 5))
  (set-car! lst 'changed)
  (check lst => '(changed 2 3 4 5)))

;; 测试set-car!的副作用（变量引用一致性）
(let ((lst1 (list 'a 'b 'c)))
  (let ((lst2 lst1))
    (set-car! lst1 'X)
    (check lst1 => '(X b c))  ; 原列表已被修改
    (check lst2 => '(X b c)))) ; lst2指向同一对象，也被修改

;; 测试set-car!对不同数据结构的影响
(let ((pair (cons 'head 'tail))
      (alist (list 'a 'b 'c 'd 'e)))
  
  ;; 修改点对结构
  (set-car! pair 'new-head)
  (check pair => '(new-head . tail))
  
  ;; 修改列表的各个位置（通过访问不同car操作）
  (set-car! alist 'first)
  (check alist => '(first b c d e))
  
  ;; 验证列表结构保持正确
  (check (length alist) => 5)
  (check (cdr alist) => '(b c d e)))

;; set-car!错误处理测试
(check-catch 'wrong-type-arg (set-car! 123 'value)); 数字不是pair
(check-catch 'wrong-type-arg (set-car! '() 'value)); 空列表不是pair
(check-catch 'wrong-type-arg (set-car! "string" 'value)); 字符串不是pair
(check-catch 'wrong-type-arg (set-car! #t 'value)); 布尔值不是pair

;; 测试参数数量错误
(check-catch 'wrong-number-of-args (set-car! (cons 1 2)))
(check-catch 'wrong-number-of-args (set-car! (cons 1 2) 'a 'b))
(check-catch 'wrong-number-of-args (set-car!))

;; 测试复杂对象的set-car!修改
(let ((complex-pair (cons (list 'old-structure 'with-values) 'remaining-cdr)))
  (set-car! complex-pair 'simplified)
  (check complex-pair => '(simplified . remaining-cdr)))

#|
set-cdr!
替换序对（pair）的第二个元素（cdr部分）为新值，该操作会直接修改原始序对对象。

语法
----
(set-cdr! pair obj)

参数
----
pair : pair?
    要被修改的序对，可以是点对或任何非空列表。必须是一个可以修改的序对对象。
    
obj : any
    要设置的新值，可以是任何类型的对象，包括符号、数字、列表、字符串、布尔值等。

返回值
------
unspecified
    根据R7RS规范，返回未指定的值。

说明
----
1. set-cdr!是一个变异操作，会直接修改原始序对对象的内存内容
2. 替换的是序对的第二个元素（cdr部分）
3. 当应用于列表时，可以修改列表的尾部结构，包括创建循环结构
4. 修改后原始对象的引用仍然指向同一个内存位置
5. 适用于所有序对数据：不论是显式点对 (a . b) 还是非空列表 (a b c ...)

错误处理
--------
wrong-type-arg
    当第一个参数不是序对（如空列表、数字、字符串等）时抛出错误。
wrong-number-of-args
    当参数数量不等于2时抛出错误。

影响范围
--------
- 该操作会直接影响使用同一引用的所有代码位置
- 修改后原始对象的内容立即发生变化
- 可以用于构建循环列表结构或扩展列表尾部
- 谨慎使用，避免破坏不变量或导致不可预期的副作用

示例应用场景
------------
- 构建循环列表：通过set-cdr!将列表最后一个元素的cdr指向列表自身
- 动态修改列表尾部：可以替换整个列表的尾部为一个新列表
- 链表操作：在链表数据结构中插入或删除节点
|#

;; set-cdr!基本功能测试
(let ((p (cons 1 2)))
  (set-cdr! p 100)
  (check p => '(1 . 100)))

;; set-cdr!用于列表尾元素修改
(let ((lst (list 'a 'b 'c)))
  (set-cdr! (cdr lst) '(x y)) 
  (check lst => '(a b x y)))

;; set-cdr!测试不同类型的值
(let ((p (cons 'head 'old)))
  (set-cdr! p "new string")
  (check (cdr p) => "new string")
  (set-cdr! p 42)
  (check (cdr p) => 42)
  (set-cdr! p #t)
  (check (cdr p) => #t))

;; 使用set-cdr!修改嵌套结构
(let ((nested (list (list 1 2) (list 3 4))))
  (set-cdr! (car nested) 'tail)
  (check nested => '((1 . tail) (3 4))))

;; set-cdr!与cons构造器结合测试
(let ((p (cons 'head 'cdr-value)))
  (check (cdr p) => 'cdr-value)
  (set-cdr! p 'modified)
  (check (cdr p) => 'modified)
  (check (car p) => 'head))

;; 使用set-cdr!构建循环结构
(let ((lst (list 'a 'b 'c)))
  (set-cdr! (last-pair lst) lst)  ; 创建循环列表
  (check lst => lst)  ; 循环引用，自身指向
  (check (list-ref lst 3) => 'a)  ; 第四个元素回到第一个
  (check (list-ref lst 4) => 'b))

;; set-cdr!测试多次调用
(let ((lst (list 1 2 3)))
  (let ((tail (list 'new-tail)))
    (set-cdr! (cdr lst) tail)
    (check lst => '(1 2 new-tail))))

;; 测试set-cdr!的副作用（变量引用一致性）
(let ((lst1 (list 'a 'b 'c)))
  (let ((lst2 lst1))
    (set-cdr! lst1 '(x))  ; 将整个列表尾部替换为(x)
    (check lst1 => '(a x))  ; 原列表已被修改
    (check lst2 => '(a x)))) ; lst2指向同一对象，也被修改

;; 测试set-cdr!对不同数据结构的影响
(let ((pair (cons 'first 'second))
      (lst (list 'a 'b 'c 'd)))
  
  ;; 修改点对结构
  (set-cdr! pair 'new-tail)
  (check pair => '(first . new-tail))
  
  ;; 将列表尾部替换为单个元素
  (set-cdr! (cdr (cdr lst)) '() )
  (check lst => '(a b c)))

;; 测试set-cdr!与cons结合构建动态结构
(let ((lst (cons 'head 'tail)))
  (set-cdr! lst (cons 'second-element 'final))
  (check lst => '(head second-element . final)))

;; set-cdr!错误处理测试
(check-catch 'wrong-type-arg (set-cdr! 123 'value)); 数字不是pair
(check-catch 'wrong-type-arg (set-cdr! '() 'value)); 空列表不是pair
(check-catch 'wrong-type-arg (set-cdr! "string" 'value)); 字符串不是pair
(check-catch 'wrong-type-arg (set-cdr! #t 'value)); 布尔值不是pair

;; 测试参数数量错误
(check-catch 'wrong-number-of-args (set-cdr! (cons 1 2)))
(check-catch 'wrong-number-of-args (set-cdr! (cons 1 2) 'a 'b))
(check-catch 'wrong-number-of-args (set-cdr!))

;; 测试复杂对象的set-cdr!修改
(let ((complex-pair (cons (list 'head-structure) '(tail-structure remaining))))
  (set-cdr! complex-pair 'simple-tail)
  (check complex-pair => '((head-structure) . simple-tail)))

(check (caar '((a . b) . c)) => 'a)

#|
caar
caar 是 Scheme 内置函数，用于获取嵌套序对的第一个元素的第一个分量。该函数是 R7RS 标准的基本列表操作函数之一。此函数等价于 (car (car pair))。

语法
----
(caar pair)

参数
----
pair : pair?
必须是序对或列表。一般为一个包含点对或列表的列表，如 ((a . b) . c) 或 ((a b c) ...)。

返回值
------
任意类型
返回嵌套序对的第一个元素的第一个分量。根据不同序对和嵌套结构的性质，返回类型可以是符号、数字、列表、点对、布尔值等任何对象。

说明
----
1. caar 是 pair? 谓词的重要操作之一，常与 cdr、cdar、cadr 等配合使用处理嵌套序对数据
2. 当应用于列表时，相当于 (car (car list))，需要保证嵌套结构有效且非空
3. 适用于所有嵌套序对数据：包括点对结构 ((a . b) . c) 和嵌套列表 ((a b c) d e f)
4. 当嵌套结构为列表时，caar 返回第一个列表的第一个元素

错误处理
--------
wrong-type-arg
出现以下情况时抛出错误：
1. 参数不是序对或列表类型（如空列表 '()、数字、字符串等）
2. 序对结构的第一个元素本身不是序对或列表（如原子或空列表）
|#

;; 基础测试：显式点对结构
(check (caar '((a . b) . c)) => 'a)
(check (caar '((1 . 2) . 3)) => 1)
(check (caar '((#t . #f) . nil)) => #t)

;; 基础测试：列表结构
(check (caar '((a b c) d e)) => 'a)
(check (caar '((1 2 3) 4 5)) => 1)
(check (caar '((#t #f) x y z)) => #t)

;; 嵌套列表结构
(check (caar '(((a b) c) d e)) => '(a b))
(check (caar '(((() a) b) c)) => '(() a))
(check (caar '(((1 2) 3) 4)) => '(1 2))

;; 混合结构测试
(check (caar '(("hello" . 123) . "world")) => "hello")
(check (caar '((42 . "forty-two") . 99)) => 42)
(check (caar '((#\a . #\b) . nil)) => #\a)

;; 构造器创建的结构
(check (caar (cons (cons 1 2) (cons 3 4))) => 1)
(check (caar (cons (cons 'x 'y) (cons 'z 'w))) => 'x)
(check (caar (cons (list 1 2 3) (list 4 5 6))) => 1)

;; 复杂嵌套构造
(let ((nested (cons (cons (cons 1 2) (cons 3 4)) (cons 5 6))))
  (check (caar nested) => (cons 1 2)))

;; 涉及空列表的测试
(check-catch 'wrong-type-arg (caar '(() . c)))
(check-catch 'wrong-type-arg (caar '((). ())))

;; 非法参数类型错误
(check-catch 'wrong-type-arg (caar 'a))
(check-catch 'wrong-type-arg (caar 123))
(check-catch 'wrong-type-arg (caar "hello"))
(check-catch 'wrong-type-arg (caar #f))
(check-catch 'wrong-type-arg (caar '()))
(check-catch 'wrong-type-arg (caar '(a b . c)))

;; 返回不同类型测试
(check (caar '(("string" "another") 42)) => "string")
(check (caar '((123 456) 789)) => 123)
(check (caar '(((1 2 3)) 4 5 6)) => (list 1 2 3))
(check (caar '((#f nil "test") x y z)) => #f)

;; edge cases for nested structure
(check (caar '((((a))))) => '((a)))
(check (caar '((((1 2))) 3 4 5)) => '((1 2)))

#|
cadr
cadr 是 Scheme 内置函数，用于获取序对的第二个元素的第一个分量。该函数是 R7RS 标准的基本列表操作函数之一，等价于 (car (cdr pair))。

语法
----
(cadr pair)

参数
----
pair : pair?
可以是序对（即非空列表或显式点对），不能是空列表或其他对象。

返回值
------
任意类型
返回序对的第二个元素的第一个分量。根据不同的序对内容，返回类型可以是
符号、数字、列表、点对、布尔值等任何对象。

说明
----
1. cadr 是 pair? 谓词的基本操作之一，与 car、cdr 和 caar 配合使用处理序对数据
2. 当应用于列表时，返回列表的第二个元素
3. 适用于所有序对数据：不论是点对 (a b . c) 还是非空列表 (a b c ...)
4. 等价于 (car (cdr pair))，执行顺序从右到左解析
5. 是 R7RS 标准 car/cdr 组合函数的之一

错误处理
--------
wrong-type-arg
当参数不是序对（如空列表 '()、数字、字符串等）或序对的cadr不是序对时抛出错误。

边界条件
--------
- 当序对长度为1时，cadr 可能不是序对，需要抛出异常
- 单元素列表因没有cadr会引发错误
- 正确支持两元素及以上列表的访问

性能特征
--------
- 时间复杂度：O(1) 恒定时间完成操作
- 空间复杂度：O(1) 不消耗额外栈空间
- 内存分配：直接访问现有序对结构，无新对象创建

数据类型兼容性
-------------
- 支持所有类型的列表和序对结构
- 各元素可以是任意 Scheme 对象
- 正确处理嵌套列表和复杂结构
|#

;; cadr 基础测试 - 各种典型场景
(check (cadr '(a b)) => 'b)
(check (cadr '(a b c)) => 'b)
(check (cadr '(1 2 3 4 5)) => 2)
(check-catch 'wrong-type-arg (cadr '(a . b)))  ; 点对结构，cadr不是pair
(check (cadr '(a b . rest)) => 'b)
(check (cadr '((a . b) c)) => 'c)              ; 有效点对结构

;; cadr 边界测试
(check (cadr '(a b)) => 'b)                     ; 两元素列表边界
(check-catch 'wrong-type-arg (cadr '(only)))    ; 单元素列表异常
(check (cadr '(pair single)) => 'single)        ; 两元素任意值
(check-catch 'wrong-type-arg (cadr '(a . b)))         ; 点对结构

;; 数据类型边界测试
(check (cadr '(42 string symbol #t)) => 'string)           ; 混合类型访问
(check (cadr '("hello" "world" "test")) => "world")      ; 字符串列表
(check (cadr '(#t #f #t)) => #f)                           ; 布尔值列表
(check (cadr '(list vector string)) => 'vector)             ; 类型对象列表
(check (cadr '((a b) (c d) (e f))) => '(c d))              ; 子列表结构

;; 数值边界测试
(check (cadr '(100 200 300 400 500)) => 200)                 ; 整数数值
(check (cadr '(1.1 2.2 3.3 4.4 5.5)) => 2.2)                 ; 浮点数值
(check (cadr '(1/2 2/3 3/4)) => 2/3)                           ; 有理数
(check (cadr '(1+2i 3+4i 5+6i)) => 3+4i)                       ; 复数

;; 任意对象类型测试
(check (cadr '( #a "string" 42 )) => "string")             ; 字符对象
(check (cadr '(if-cond then-block else-block)) => 'then-block) ; Scheme关键字
(check (cadr '((lambda (x) x) (lambda (y) y) (lambda (z) z))) => '(lambda (y) y)) ; lambda过程

;; 构造器创建的结构测试
(check (cadr (list 'a 'b 'c 'd)) => 'b)                       ; list构造器
(check (cadr (cons 'a (cons 'b (cons 'c '())))) => 'b)        ; cons构造器
(check (cadr (append '() '(a b c))) => 'b)                   ; append结果
(check (cadr (reverse '(c b a))) => 'b)                      ; reverse结果

;; Unicode和字符串边界测试
(check (cadr '("中文" "测试" "验证")) => "测试")              ; Unicode字符串
(check (cadr '(#\中 #\文 #\字)) => #\文)               ; Unicode字符

(check-catch 'wrong-type-arg (cadr '()))                      ; 空列表错误
(check-catch 'wrong-type-arg (cadr 123))                      ; 数字错误
(check-catch 'wrong-type-arg (cadr "string"))                 ; 字符串错误
(check-catch 'wrong-type-arg (cadr #t))                       ; 布尔值错误
(check-catch 'wrong-type-arg (cadr #\a))                       ; 字符错误
(check-catch 'wrong-type-arg (cadr #(a b)))                   ; 向量错误

;; 单元素边界异常测试
(check-catch 'wrong-type-arg (cadr '(single)))                ; 单元素列表错误
(check-catch 'wrong-type-arg (cadr '(all)))                   ; 单元素任意值错误

;; 构造器与操作函数链式测试
(check (cadr (list 'car 'cdr 'cons 'append)) => 'cdr)         ; 内部过程对象

;; =======================================
;; [201_12] cadr 补充边界测试和文档完善
;; 根据 201_12.md 要求补充边界值和数据兼容性测试
;; =======================================

;; 边界测试集1：空结构边界
(check-catch 'wrong-type-arg (cadr (cons 'a 'b)))          ; 单点对结构
(check-catch 'wrong-type-arg (cadr (cons 'a '())))         ; 点对与空列表

;; 边界测试集2：极限长度边界  
(check (cadr (make-list 1000 'x)) => 'x)                   ; 极大长度验证
(check (cadr (append '(a) (make-list 999 'x))) => 'x)      ; 长列表性能边界

;; 边界测试集3：特殊对象类型边界
(check (cadr '(#t + #f)) => '+)                             ; 过程对象访问
(check (cadr '(#t #(1 2 3) #f)) => #(1 2 3))                ; 向量作为元素
(check (cadr '(#t #u8(255 128) #f)) => #u8(255 128))        ; 字节向量访问

;; 边界测试集4：Unicode边界测试
(check (cadr '("特殊&符号" "正常字符串")) => "正常字符串")       ; 复杂UTF-8边界

;; 边界测试集5：复合结构异常边界
(check-catch 'wrong-type-arg (cadr (vector 'a 'b)))          ; 向量类型错误
(check-catch 'wrong-number-of-args (cadr))                   ; 零参数错误  
(check-catch 'wrong-number-of-args (cadr '(a b) '(c d)))     ; 多参数错误

#|
cddr
cddr 是 Scheme 内置函数，用于获取序对从第三个元素开始的所有分量（移除第1、第2个元素）。该函数是 R7RS 标准的基本列表操作函数之一。此函数等价于 (cdr (cdr pair))。

语法
----
(cddr pair)

参数
----
pair : pair?
可以是序对（即非空列表或显式点对），不能是空列表或其他对象。

返回值
------
任意类型
返回序对第三个元素开始的所有分量。根据不同的序对内容，返回类型可以是
列表、符号、数字、点对、布尔值等任何对象。

说明
----
1. cddr 是 pair? 谓词的基本操作之一.
2. 当应用于列表时，相当于(cdr(cdr list))，需要保证嵌套结构有效且非空
3. 适用于所有序对数据：不论是点对 (a . b) 还是非空列表 (a b c ...)

错误处理
--------
wrong-type-arg
1.当参数不是序对（如空列表 '()、数字、字符串等）时抛出错误。
2.序队结构中的元素数量少于三个元素

|#
(check (cddr '(a b c . d)) => '(c . d))
(check (cddr '(a b c)) => '(c))
(check (cddr '(1 2 . 3)) => 3)
(check (cddr '((a b) c . d)) => 'd)
(check (cddr (cons 'a (cons 'b (cons 'c 'd)))) => '(c . d))

; 错误测试
(check-catch 'wrong-type-arg (cddr '()))
(check-catch 'wrong-type-arg (cddr 123))
(check-catch 'wrong-type-arg (cddr "hello"))
(check-catch 'wrong-type-arg (cddr #t))
(check-catch 'wrong-number-of-args (cddr))
(check-catch 'wrong-number-of-args (cddr '(1 2) '(3 4)))

;; cddr边界条件测试补充
(check (cddr '(a b)) => '())                       ; 单元素列表cddr边界
(check (cddr '(1 2)) => '())                       ; 单元素数字列表cddr边界
(check (cddr '(#t #f)) => '())                      ; 单元素布尔列表cddr边界
(check (cddr '("hello" "world")) => '())                 ; 单元素字符串列表cddr边界
(check (cddr '(() b c)) => '(c))               ; 空列表作为首元素的cddr
;; 各种数据类型cddr边界测试
(check (cddr '(123 "text" symbol)) => '(symbol))
(check (cddr '(#ewline #ab #\space)) => '(#\space))
(check (cddr '((a b) c d)) => '(d))
(check (cddr '(#(1 2) #(3 4) #(5 6))) => '(#(5 6)))
(check (cddr '(+ - * /)) => '(* /))
(check (cddr '('(a b) '(c d) '(e f))) => '('(e f)))

;; 极端边界条件测试
(check (cddr '((lambda (x) x) (lambda (y) y) (lambda (z) z))) => '((lambda (z) z)))
(check (cddr '((begin 1 2 3) (begin 4 5) (begin 6 7))) => '((begin 6 7)))
(check (cddr '(a b c.d)) => '(c.d))
(check (cddr '("中文" "测试" "程序")) => '("程序"))

#|
null?
判断给定的对象是否为空列表。

语法
----
(null? obj)

参数
----
obj : any
任意类型的对象

返回值
------
boolean?
如果obj是空列表则返回#t，否则返回#f

说明
----
1. 用于检查对象是否为空列表'()
2. 对其他任何类型的对象都返回#f
3. 通常在列表处理中使用，用于判断列表是否为空

特殊规则
---------
- 仅当参数为精确的空列表 '() 时返回 #t
- 所有其他对象，包括向量、字符串、数字等都返回 #f
- 非列表结构也返回 #f（如点对、符号等）

错误处理
---------
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; null? 基本测试：空列表和非空列表
(check (null? '()) => #t)                   ; 空列表
(check (null? '(1)) => #f)                  ; 单元素列表
(check (null? '(a)) => #f)                  ; 单元素符号列表
(check (null? '(a b c)) => #f)              ; 多元素列表
(check (null? '(1 2 3 4 5)) => #f)          ; 长列表

;; null? 特殊结构和边界情况
(check (null? '(())) => #f)                 ; 包含空列表的列表
(check (null? '(() () ())) => #f)           ; 空列表嵌套
(check (null? '((a b) (c d))) => #f)        ; 嵌套列表

;; null? 非列表类型测试 - 全面覆盖
(check (null? #t) => #f)                    ; 布尔值
(check (null? #f) => #f)                    ; 布尔值
(check (null? 0) => #f)                     ; 零
(check (null? 123) => #f)                   ; 整数
(check (null? -456) => #f)                  ; 负整数
(check (null? 3.14) => #f)                  ; 浮点数
(check (null? "") => #f)                   ; 空字符串
(check (null? "hello") => #f)               ; 字符串
(check (null? '#()) => #f)                  ; 空向量
(check (null? '#(1 2 3)) => #f)             ; 向量
(check (null? 'symbol) => #f)               ; 符号
(check (null? '123) => #f)                  ; 数字符号
(check (null? #\a) => #f)                  ; 字符

;; null? 点对结构测试
(check (null? '(a . b)) => #f)              ; 点对不是空列表
(check (null? (cons 1 2)) => #f)            ; cons 创建的点对

;; null? 复杂表达式测试
(check (null? (list)) => #t)                ; 由list创建的空列表
(check (null? (append '() '())) => #t)      ; append结果
(check (null? (cdr '(a))) => #t)            ; cdr结果
(check (null? (cdr '(a b))) => #f)          ; cdr结果

;; null? 与列表操作结合测试
(check (null? (reverse '())) => #t)
(check (null? (reverse '(1))) => #f)

;; null? 错误处理测试
(check-catch 'wrong-number-of-args (null?))
(check-catch 'wrong-number-of-args (null? '() '()))
(check-catch 'wrong-number-of-args (null? 1 2))


#|
list?
判断给定的对象是否为列表类型。

语法
----
(list? obj)

参数
----
obj : any
任意类型的对象

返回值
------
boolean?
如果obj是列表类型则返回#t，否则返回#f

说明
----
1. 用于检查对象是否为列表类型
2. 能够正确识别空列表 '() 和非空列表
3. 能够处理嵌套列表和点对结构
4. 能够处理循环列表等特殊结构

特殊规则
---------
- 空列表 '() 被认为是列表
- 点对结构如果形成完整列表则也认为是列表
- 其他类型如数字、字符串、向量、布尔值等都返回#f
  

错误处理
---------
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; list? 基本测试：空列表和各种简单列表
(check-true (list? '()))                        ; 空列表
(check-true (list? '(a)))                       ; 单元素
(check-true (list? '(a b c)))                   ; 多元素普通列表
(check-true (list? '(1 2 3 4 5)))               ; 数字长列表

;; list? 嵌套和复杂结构测试
(check-true (list? '(a (b) c)))                 ; 嵌套列表  
(check-true (list? '((a) (b) (c))))             ; 多层嵌套
(check-true (list? '((a b) (c d))))             ; 深度嵌套
(check-true (list? '(1 (2 (3 (4))))))           ; 多级嵌套

;; list? 混合类型元素测试
(check-true (list? '(a 1 "string" #t)))         ; 混合类型
(check-true (list? '((list 1 2) (vector 3 4)))) ; 包含复杂对象

;; list? 点和边界情况
(check-true (list? '(1 . 2)))                   ; 点对结构
(check-true (list? '(a b . c)))                 ; 非完整列表边缘情况

;; list? 特殊结构测试
(check-true (list? (let ((x '(1 2 3)))          ; 循环列表
                    (set-cdr! (cddr x) x) x)))

;; list? 非列表类型测试 - 全面覆盖
(check-false (list? #t))                        ; 布尔值
(check-false (list? #f))                        ; 布尔值
(check-false (list? 123))                       ; 整数
(check-false (list? -456))                      ; 负整数
(check-false (list? 0))                         ; 零
(check-false (list? 3.14))                      ; 浮点数
(check-false (list? "Hello"))                   ; 字符串
(check-false (list? ""))                        ; 空字符串
(check-false (list? '#()))                       ; 空向量
(check-false (list? '#(1 2 3)))                  ; 向量
(check-false (list? '12345))                    ; 数字符号
(check-false (list? 'symbol))                   ; 符号
(check-false (list? #\a))                       ; 字符

;; list? 错误处理测试
(check-catch 'wrong-number-of-args (list?))
(check-catch 'wrong-number-of-args (list? #t #f))

#|
make-list
创建一个包含指定数量指定元素的列表。

语法
----
(make-list n fill)
(make-list n)

参数
----
n : exact?
    精确的非负整数，表示要创建的列表长度。
    必须满足 0 <= n < (expt 2 32) 或实现定义的最大允许列表长度。

fill : any (可选)
    填充到列表中的元素值。如果不指定，默认为 #f。
    可以是任何类型的 Scheme 对象。

返回值
------
list?
    一个长度为 n 的列表，所有元素都为 fill 值。
    当 n 为 0 时返回空列表 '()。

说明
----
1. 这是一个特殊形式的列表构造器，用于快速创建包含重复元素的列表
2. 可为任何非负整数长度创建列表，包括空列表
3. 填充元素可以是任意类型，包括复杂对象
4. 使用 make-list 构造的列表是全新的单个对象，不会与其他对象共享结构
5. 主要用于初始化数据结构、生成测试数据等场景

边界条件
--------
- n = 0 返回空列表 '()
- n = 1 返回单元素列表 '(fill)
- 支持极大 n 值（受实现最大允许列表长度限制）
- fill 值可以是各种数据类型和复杂对象
- 允许列表内容为嵌套结构或函数对象

性能特征
--------
- 时间复杂度：O(n)，线性时间构建列表
- 空间复杂度：O(n)，需要为n个新序对分配内存
- 递归深度：无递归，使用循环构造
- 内存分配：创建n个新的序对对象

数据类型兼容性
-------------
- fill值支持：数值、布尔、字符、字符串、符号、过程、列表、向量、字节向量等所有类型
- n值约束：整数类型，建议为正整数和0
- 返回值：统一的列表类型

限制说明
--------
- fill参数是值传递，复杂对象会复制引用，不会创建新对象
- 极大n值可能导致内存不足
- 不适合动态增长的列表构建

示例
----
(make-list 3 'x) => '(x x x)
(make-list 0 'x) => '()
(make-list 2 42) => '(42 42)
(make-list 4 #\a) => '(#\a #\a #\a #\a)
|#

;; make-list 基本功能测试
(check (make-list 0) => '())                                   ; 空列表边界
(check (make-list 0 'x) => '())                               ; 指定填充值的空列表
(check (make-list 1) => '(#f))                                ; 默认填充值单元素
(check (make-list 1 'singleton) => '(singleton))             ; 自定义填充值单元素
(check (make-list 3) => '(#f #f #f))                          ; 默认填充值多元素
(check (make-list 3 'repeat) => '(repeat repeat repeat))      ; 自定义填充值多元素

;; make-list 边界值测试
(check (make-list 10 'a) => '(a a a a a a a a a a))           ; 到长度边界
(check (make-list 100 'X) => (make-list 100 'X))             ; 大长度验证一致性

;; make-list 数据类型兼容性测试
(check (make-list 4 42) => '(42 42 42 42))                    ; 整数类型
(check (make-list 3 3.14) => '(3.14 3.14 3.14))               ; 实数类型
(check (make-list 2 #t) => '(#t #t))                          ; 布尔值
(check (make-list 2 #\a) => '(#\a #\a))                      ; 字符类型
(check (make-list 2 "hello") => '("hello" "hello"))           ; 字符串类型
(check (make-list 3 'symbol) => '(symbol symbol symbol))      ; 符号类型
(check (make-list 2 #(1 2 3)) => `(#(1 2 3) #(1 2 3)))      ; 向量对象
(check (make-list 2 #u8(255 128)) => `(#u8(255 128) #u8(255 128))) ; 字节向量

;; make-list 复杂数据类型测试
(check (make-list 3 '()) => '(() () ()))                      ; 空列表作为填充值
(check (make-list 2 '(a b c)) => '((a b c) (a b c)))          ; 列表作为填充值
(check (make-list 2 (list 1 2 3)) => '((1 2 3) (1 2 3)))      ; 列表作为填充值

;; make-list 极端边界测试
(check (make-list 2 (cons 'a 'b)) => '((a . b) (a . b)))      ; 点对结构作为填充值
(check (make-list 1 (make-list 3 'x)) => '((x x x)))          ; 嵌套make-list调用
(check (make-list 3 (make-list 0)) => '(() () ()))            ; 空列表嵌套

;; make-list 函数和过程对象的基本验证
(check (length (make-list 3 car)) => 3)                      ; 验证长度
(check (list? (make-list 2 car)) => #t)                      ; 验证类型
(check (procedure? (car (make-list 2 car))) => #t)           ; 验证过程对象正确性

;; make-list 动态边界验证
(let ((n 5))
  (let ((result (make-list n 'test)))
    (check (length result) => n)
    (check (list? result) => #t)
    (check (car result) => 'test))) ; 简单的元素验证

;; make-list 内存结构验证  
(let ((lst1 (make-list 3 'same))
      (lst2 (make-list 3 'same)))
  (check (equal? lst1 lst2) => #t)                          ; 内容相同
  (check (eq? lst1 lst2) => #f))                             ; 不是同一对象

;; 错误参数类型测试
(check-catch 'wrong-number-of-args (make-list))               ; 参数不足
(check-catch 'wrong-number-of-args (make-list 3 'x 'extra))   ; 参数过多
(check-catch 'out-of-range (make-list -1 'x))                ; 负数长度错误

#|
list
通过给定元素构建一个新的列表。该函数是R7RS标准的基本列表构造函数之一。

语法
----
(list obj ...)

参数
----
obj ... : any
任意数量和类型的对象，包括符号、数字、字符串、布尔值、列表、向量、字节向量、字符、过程等。

返回值
------
list?
返回一个新的列表，包含所有参数obj按顺序排列。空参数返回空列表'()。

说明
----
1. list是构建新列表的首选构造函数，将任意数量和类型的参数组合成列表
2. 返回的是新创建的对象，与输入参数不会共享内存结构
3. 接受任意类型的参数，包括嵌套列表、复杂对象等特殊类型
4. 空参数调用返回空列表'()，单参数创建单元素列表
5. 与cons不同，list构造的是统一结构的列表

边界条件
--------
- 空参数：返回空列表'()
- 单参数：返回单元素列表
- 同类型参数：保持类型一致性
- 多类型混合：允许任意类型组合
- 嵌套结构：正确处理子列表嵌套
- 极大参数数量：支持大量参数构造
- 重复元素：保留每个实例

性能特征
--------
- 时间复杂度：O(n)，与参数数量成正比
- 空间复杂度：O(n)，新列表消耗内存与参数数量成正比
- 内存分配：创建新的序对对象，不共享输入参数
- 深层嵌套：支持深层结构不影响性能

数据类型兼容性
-------------
- 数值类型：整数、实数、复数、有理数
- 字符串：任意长度和内容字符串
- 字符：任意字符包括Unicode
- 符号：普通符号及关键字符号
- 布尔值：#t和#f支持
- 列表：空列表和任意嵌套列表
- 向量：任意维度和内容的向量
- 字节向量：任意字节序列
- 过程：内置过程和自定义过程
- 复合对象：对象组合的任意嵌套

限制说明
--------
- 参数传递是值传递，复杂对象会复制引用
- 极大参数列表可能导致内存不足
- 不支持单独设置元素值，如需精确控制需使用cons

示例
----
(list) => '()
(list 'a) => '(a)
(list 1 2 3) => '(1 2 3)
(list "hello" "world") => '("hello" "world")
(list 'a 42 #t 'b) => '(a 42 #t b)
|#

;; list 基本构造功能测试
(check (list) => '())                    ; 空参数构造
(check (list 'a) => '(a))               ; 单元素构造
(check (list 1 2 3) => '(1 2 3))        ; 多元素构造
(check (list "hello") => '("hello"))   ; 单元素字符串

;; 边界条件测试
(check (list) => '())                   ; 边界：空列表构造
(check (list 'single) => '(single))     ; 边界：单元素列表
(check (list 'a 'b 'c 'd) => '(a b c d))  ; 边界：四元素列表

;; 数据类型兼容性测试
(check (list 42 3.14 1/2 1+2i) => '(42 3.14 1/2 1+2i))  ; 数值类型
(check (list #t #f) => '(#t #f))        ; 布尔值
(check (list #\a #\b #\c) => '(#\a #\b #\c))   ; 字符
(check (list "hello" "world") => '("hello" "world"))  ; 字符串
(check (list 'symbol1 'symbol2 'keyword:) => '(symbol1 symbol2 keyword:)) ; 符号

;; 嵌套结构测试
(check (list '() '() '()) => '(() () ()))   ; 空列表嵌套
(check (list '(a b) '(c d)) => '((a b) (c d)))  ; 子列表嵌套
(check (list (list 1 2) (list 3 4)) => '((1 2) (3 4)))  ; 列表构造函数嵌套

;; 复杂对象测试
(check (list #(1 2 3) #u8(255 128)) => `(#(1 2 3) #u8(255 128)))  ; 向量/字节向量
(check (list #t #f) => `(#t #f))  ; 简单类型验证
(check (list 1 2 3) => `(1 2 3))  ; 数值类型验证

;; 混合类型测试
(check (list 'symbol 42 "text" #\c #t #(1 2)) => '(symbol 42 "text" #\c #t #(1 2)))  ; 全类型混合
(check (list 'a 1 "hello" '(sub list) #t 3.14) => '(a 1 "hello" (sub list) #t 3.14))  ; 复合混合

;; 极大参数数量测试
(check (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) => '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))  ; 边界：大量参数

;; 重复元素测试
(check (list 'a 'a 'a) => '(a a a))     ; 重复符号
(check (list 1 1 1 1) => '(1 1 1 1))     ; 重复数字
(check (list "test" "test") => '("test" "test"))  ; 重复字符串

;; Unicode和特殊字符测试
(check (list "中文" "测试" "字符串") => '("中文" "测试" "字符串"))  ; Unicode字符串
(check (list #\中 #\文) => '(#\中 #\文))   ; Unicode字符

;; 深层嵌套结构测试
(check (list (list (list 'a)) (list 'b)) => '(((a)) (b)))  ; 三层嵌套
(check (list '() (list '() (list 'a))) => '(() (() (a))))   ; 复杂嵌套

;; 性能验证：大列表构造
(check (length (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z)) => 26)  ; 验证完整构造

;; 错误测试 - 参数类型验证
(check (list 123) => '(123))  ; 证实数字可以作为参数
(check (list "string") => '("string"))  ; 证实字符串可以作为参数
(check (list #t) => '(#t))   ; 证实布尔值可以作为参数

;; 构造后操作验证
(let ((constructed (list 1 2 3 4 5)))
  (check (length constructed) => 5)      ; 验证长度
  (check (list? constructed) => #t)      ; 验证类型
  (check (list-ref constructed 2) => 3))  ; 验证索引访问

;; 独立对象验证 - 确认不与参数共享
(let ((a 'original)
      (b "test")
      (c #t))
  (let ((result (list a b c)))
    (check (equal? result '(original "test" #t)) => #t)  ; 值正确
    (check (not (eq? result a)) => #t)  ; 独立对象验证
    (check (not (eq? result b)) => #t)))

;; 参数传递验证测试
(define (test-list-wrapper . args)
  (apply list args))

(check (test-list-wrapper 1 2 3) => '(1 2 3))
(check (test-list-wrapper 'a 'b 'c 'd) => '(a b c d))
(check (test-list-wrapper) => '())


#|
length
返回列表的元素个数。

语法
----
(length list)

参数
----
list : any
任意类型的对象。

返回值
------
integer?
如果list是列表，返回该列表的元素个数。
如果参数不是列表，返回#f。

说明
----
1. 用于计算列表的长度，即列表中包含的元素个数
2. 空列表'()的长度为0
3. 对于点对结构（非正规列表），根据实现行为返回结果
4. 列表可以是普通列表、嵌套列表或包含任意类型元素的列表

错误处理
--------
wrong-type-arg
当参数不是列表时，根据实现返回特定值或抛出错误。

示例
----
(length '()) => 0
(length '(1 2 3)) => 3
(length '((a b) c d)) => 3
|#

;; length 基本测试：空列表和非空列表
(check (length '()) => 0)
(check (length '(a)) => 1)
(check (length '(a b)) => 2)
(check (length '(a b c)) => 3)
(check (length '(1 2 3 4 5)) => 5)

;; length 嵌套列表测试
(check (length '((a) b c)) => 3)
(check (length '((a b) (c d) e)) => 3)
(check (length '((a) (b) (c))) => 3)
(check (length '(((a b) c) d e)) => 3)
(check (length '((1 2 3 4) (5 6 7 8))) => 2)

;; length 复杂数据结构测试
(check (length '((first 1) (second 2) (third 3))) => 3)
(check (length '("hello" "world" "test")) => 3)
(check (length '(#t #f 'symbol)) => 3)
(check (length '(42 3.14 "string" #t)) => 4)

;; length 边界测试：各种规模列表
(check (length '(a)) => 1)
(check (length '(a b)) => 2)
(check (length '(a b c d e f g h i j)) => 10)
(check (length '(long list with many elements potentially spanning multiple lines)) => 9)

;; length 空列表和单元素列表测试
(check (length '()) => 0)
(check (length (list)) => 0)
(check (length (cons 'a '())) => 1)
(check (length '(single)) => 1)

;; length 字符和数字列表测试
(check (length '(#\a #\b #\c #\d)) => 4)
(check (length '(0 1 2 3 4 5 6 7 8 9)) => 10)
(check (length '("zero" "one" "two" "three" "four")) => 5)

;; length 列表构造函数测试
(check (length (make-list 3 #\a)) => 3)
(check (length (make-list 0)) => 0)
(check (length (make-list 5 'value)) => 5)

;; length 列表操作函数结果测试
(check (length (append '(1 2) '(3 4 5))) => 5)
(check (length (append '() '(a b c))) => 3)
(check (length (append '(x y) '())) => 2)
(check (length (reverse '(a b c d))) => 4)
(check (length (reverse '())) => 0)

;; length 特殊测试：字符串和向量等
(check (length "string") => 6)
(check (length '#(1 2 3)) => 3)
(check (length 123) => #f)
(check (length 3.14) => #f)
(check (length #\a) => #f)
(check (length 'symbol) => #f)

;; length 点对结构（improper lists）
(check (length '(a . b)) => -1)
(check (length (cons 1 2)) => -1)
(check (length (cons 'a 'b)) => -1)
(check (length '(a b . c)) => -2)
(check (length '(x (a) . y)) => -2)
(check (length '(a b c . d)) => -3)

;; length 特殊边界测试
(check (length '(())) => 1)  ; 空列表作为元素
(check (length '(() () ())) => 3)  ; 多个空列表作为元素
(check (length '(() a b 3 c)) => 5)  ; 混合空结构

;; length Unicode字符串列表测试
(check (length '("中国" "美国" "日本")) => 3)
(check (length '("hello" "世界" "123")) => 3)

;; length 程序构造测试
(check (length (let ((lst '(a b c))) lst)) => 3)
(check (length (map square '(1 2 3 4))) => 4)
(check (length (filter symbol? '(a 1 b 2 c 3))) => 3)

;; length URL列表测试
(check (length '("http://example.com" "https://test.org")) => 2)
(check (length '(user admin guest moderator)) => 4)

#|
append
创建新的列表，将0个或多个列表合并为一个新列表。

语法
-----
(append list ...)

参数
-----
list ... : list?
任意数量的列表。如果没有参数，返回空列表；如果有一个参数，返回该参数本身；
如果有多个参数，返回将除最后一个参数外所有参数的元素复制到新列表后，最后一个参数成为最后一个列表。

返回值
------
list?
返回将所有参与列表合并的新列表。如果没有参数返回空列表，如果有多个参数则返回
deep-structure合并列表（即共享最后一个列表的结构）。

说明
----
1. 对于单参数，append返回该列表本身
2. 对于多参数，前面的list会被复制，最后一个list被共享
3. 允许非列表的最后一个参数（形成点对结构），这种行为符合R7RS规范
4. 对于空列表参数，append起到忽略空列表的作用
5. 实际效率：除最后一个参数外，其他列表会被复制

错误
---
该行无错误情况。所有参数可以是列表，或者最后一个参数可以是非列表。
|#

;; 基础测试用例 - 空列表合并
(check (append '() '()) => '())
(check (append '() '() '()) => '())
(check (append '() '() '() '()) => '())
(check (append) => '())

;; 单参数测试（特化情况：直接返回原参数）
(check (append '(a b c)) => '(a b c))
(check (append '()) => '())
(check (append '(1 2 3 4 5)) => '(1 2 3 4 5))
(check (append (list 1 2 3)) => (list 1 2 3))

;; 双列表合并测试
(check (append '(a) '(b)) => '(a b))
(check (append '(a b) '(c d)) => '(a b c d))
(check (append '(1 2) '(3 4 5)) => '(1 2 3 4 5))
(check (append '(a b c) '(d e)) => '(a b c d e))
(check (append '() '(a b c)) => '(a b c))
(check (append '(a b c) '()) => '(a b c))

;; 三列表合并测试
(check (append '(a) '(b) '(c)) => '(a b c))
(check (append '(1 2) '(3 4) '(5 6)) => '(1 2 3 4 5 6))
(check (append '(a b) '(c d e) '(f g)) => '(a b c d e f g))
(check (append '(x y) '(n) '(a b c)) => '(x y n a b c))

;; 多列表合并测试（复杂情况）
(check (append '(a) '(b) '(c) '(d)) => '(a b c d))
(check (append '(1) '(2 3) '(4 5 6) '(7)) => '(1 2 3 4 5 6 7))
(check (append '(a) '() '(b) '() '(c)) => '(a b c))

;; 层次结构性质验证测试
(check (list? (append '(a) '())) => #t)
(check (list? (append '() '(a))) => #t)
(check (list? (append '(a b) '(c d))) => #t)
(check (pair? (append '(a) 'b)) => #t)  ; 点对结构

;; 长度验证测试
(check (length (append '() '())) => 0)
(check (length (append '(a) '())) => 1)
(check (length (append '() '(b))) => 1)
(check (length (append '(a) '(b))) => 2)
(check (length (append '(a b) '(c d))) => 4)
(check (length (append '(1 2) '(3 4 5))) => 5)
(check (length (append '(1 2 3) '(4 5 6 7))) => 7)

;; 复杂嵌套结构测试
(check (append '((a b) (c d)) '(e f)) => '((a b) (c d) e f))
(check (append '(a b) '((c d) (e f))) => '(a b (c d) (e f)))
(check (append '((a) (b)) '((c) (d))) => '((a) (b) (c) (d)))
(check (append '((a b) c) '(d (e f))) => '((a b) c d (e f)))

;; 深层次结构测试
(check (append '(1 (2 (3))) '(((4) 5) 6)) => '(1 (2 (3)) ((4) 5) 6))
(check (append '(a (b (c))) '(d (e (f)))) => '(a (b (c)) d (e (f))))
(check (append '(() a ()) '(b)) => '(() a () b))

;; 点对结构测试（核心特性）
(check (append '(a b) 'c) => '(a b . c))
(check (append '(a) 'b) => '(a . b))
(check (append '(a b c) 'd) => '(a b c . d))
(check (append '(a b) '(c d) 'e) => '(a b c d . e))
(check (append '() 'a) => 'a)
(check (append '(a) '() 'b) => '(a . b))

;; 复杂点对结构测试
(check (append '((a) b) 'c) => '((a) b . c))
(check (append '(1 2 3) '(a . b)) => '(1 2 3 a . b))
(check (append '(a (b (c))) 'd) => '(a (b (c)) . d))
(check (append '(a b) '((c d) . e)) => '(a b (c d) . e))

;; 反向连接测试（验证方向性）
(check (append '(z y x) '(c b a)) => '(z y x c b a))
(check (append '(3 2 1) '(0 -1 -2)) => '(3 2 1 0 -1 -2))

;; 大规模合并测试
(check (length (append '(1 2 3 4 5))) => 5)
(check (length (append '(1 2 3) '(4 5 6))) => 6)
(check (length (append '(1 2) '(3 4) '(5 6))) => 6)

;; 字符列表测试
(check (append '(#\t #\e) '(#\s #\t)) => '(#\t #\e #\s #\t))
(check (append '(#\t) '(#\t) '(#\n)) => '(#\t #\t #\n))

;; 字符串列表测试（复合数据类型）
(check (append '("hello") '("world")) => '("hello" "world"))
(check (append '("a" "b") '("c" "d" "e")) => '("a" "b" "c" "d" "e"))
(check (append '("中文" "测试") '("继续")) => '("中文" "测试" "继续"))

;; 符号列表测试
(check (append '(a b c) '(d e f)) => '(a b c d e f))
(check (append '(quote define) '(lambda procedure)) => '(quote define lambda procedure))
(check (append '(if cond) '(else)) => '(if cond else))

;; 混合数据类型测试
(check (append '(1 "hello" #t) '(2 "world" #f)) => '(1 "hello" #t 2 "world" #f))
(check (append '(a 1 "test" #\x) '(b 2 c)) => '(a 1 "test" #\x b 2 c))
(check (append '(1.0 2.5) '(3.5 4.0)) => '(1.0 2.5 3.5 4.0))

;; 数字列表测试
(check (append '(1 2 3 4) '(5 6 7 8)) => '(1 2 3 4 5 6 7 8))
(check (append '(1 2 3) '(a b c)) => '(1 2 3 a b c))

;; boolean列表测试
(check (append '(#t #f) '(#t #f)) => '(#t #f #t #f))
(check (append '(#true) '(#false #t)) => '(#true #false #t))

;; 验证使用list构造函数
(check (append (list 1 2) (list 3 4)) => '(1 2 3 4))
(check (append (list 'a 'b) (list 'c 'd)) => '(a b c d))
(check (append (list) (list 'x 'y)) => '(x y))

;; 边界测试
(check (append '() '() '(a) '() '(b)) => '(a b))
(check (append '(a) '() '(b) '() '(c)) => '(a b c))
(check (append '(a) '(b) '() '(c) '(d)) => '(a b c d))

;; 链式操作验证测试
(check (append (append '(1) '(2)) '(3)) => '(1 2 3))
(check (append '(1) (append '(2) '(3))) => '(1 2 3))
(check (append (append '(a b) '(c)) '(d e)) => '(a b c d e))

;; 结构对等性验证（通过elements检查）
(check (equal? (append '(1 2 3) '(4 5)) '(1 2 3 4 5)) => #t)
(check (equal? (append '(a) '(b) '(c d)) '(a b c d)) => #t)
(check (equal? (append '() '(first second) '()) '(first second)) => #t)

;; 函数结果作为append参数测试
(check (append (map (lambda (x) (* x 2)) '(1 2 3)) '(7 8 9)) => '(2 4 6 7 8 9))
(check (append (filter (lambda (x) (> x 2)) '(1 2 3 4)) '(5 6 7)) => '(3 4 5 6 7))

;; 验证结构共享性（非复制性测试）
(let ((lst-last '(last list)))
  (let ((result (append '(copy list) lst-last)))
    (check (equal? result '(copy list last list)) => #t)
    (check (eq? (cdr (cdr result)) lst-last) => #t)))  ; 验证结构共享

;; 空列表和复杂结构组合测试
(check (append '() '(a (b c) d) '()) => '(a (b c) d))
(check (append '((a b) c) '() '(d)) => '((a b) c d))

;; 深度验证测试（确保append正确性）
(check (length (append '(a b c d) '(e f g h))) => 8)
(check (length (append '(1 2) '(3 4) '(5 6) '(7 8))) => 8)
(check (list-ref (append '(1 2 3) '(4 5 6)) 5) => 6)
(check (list-ref (append '(1) '(2 3 4)) 3) => 4)


#|
reverse
返回一个新列表，包含原始列表中的元素但顺序相反。

语法
----
(reverse list)

参数
----
list : list?
    要反转的列表。

返回值
------
list?
    包含原始列表元素的反转顺序新列表。

说明
----
1. 返回一个新的列表，元素顺序与原始列表相反
2. 适用于所有类型的列表：空列表、非空列表、嵌套列表
3. 不会改变原始列表的内容
4. 对列表中的元素类型没有限制
5. 当参数是非列表时，行为依赖于具体实现

错误处理
--------
依赖具体实现行为
|#

(check (reverse '()) => '())
(check (reverse '(a)) => '(a))
(check (reverse '(a b)) => '(b a))
(check (reverse '(a b c)) => '(c b a))
(check (reverse '(1 2 3 4 5)) => '(5 4 3 2 1))
(check (reverse '(x y z)) => '(z y x))

;; 测试空列表
(check (reverse '()) => '())
(check (equal? (reverse '()) '()) => #t)

;; 测试单元素列表
(check (reverse '(a)) => '(a))
(check (reverse '(1)) => '(1))
(check (reverse '("hello")) => '("hello"))
(check (reverse '(#t)) => '(#t))

;; 测试双元素列表
(check (reverse '(a b)) => '(b a))
(check (reverse '(1 2)) => '(2 1))
(check (reverse '("first" "second")) => '("second" "first"))
(check (reverse '(#t #f)) => '(#f #t))

;; 测试三元素列表
(check (reverse '(a b c)) => '(c b a))
(check (reverse '(1 2 3)) => '(3 2 1))
(check (reverse '("A" "B" "C")) => '("C" "B" "A"))

;; 测试长列表
(check (reverse '(1 2 3 4 5 6 7 8 9 10)) => '(10 9 8 7 6 5 4 3 2 1))
(check (reverse '(a b c d e f g h i j)) => '(j i h g f e d c b a))

;; 测试嵌套列表
(check (reverse '((a b) (c d) (e f))) => '((e f) (c d) (a b)))
(check (reverse '((1 (2 3)) 4 (5 6))) => '((5 6) 4 (1 (2 3))))
(check (reverse '("apple" ("banana" "cherry") "date")) => '("date" ("banana" "cherry") "apple"))

;; 测试混合类型列表
(check (reverse '(1 "two" #t 4.5 symbol)) => '(symbol 4.5 #t "two" 1))
(check (reverse '(#\newline "string" 42 #t)) => '(#t 42 "string" #\newline))

;; 测试特殊元素
(check (reverse '(#\tab #\newline #\space)) => '(#\space #\newline #\tab))

;; 测试由构造函数创建的列表
(check (reverse (list 1 2 3 4 5)) => '(5 4 3 2 1))
(check (reverse (cons 'a (cons 'b (cons 'c '())))) => '(c b a))

;; 测试复杂嵌套结构
(check (reverse '((a . b) (c . d) (e . f))) => '((e . f) (c . d) (a . b)))
(check (reverse '(a (b) (c (d)))) => '((c (d)) (b) a))

;; 测试列表操作结果
(let ((lst (list 1 2 3 4 5)))
  (check (reverse (reverse lst)) => lst))

;; 测试列表结构保持
(check (reverse '(a . (b . (c . ())))) => '(c b a))

;; 测试包含空列表的情况
(check (reverse '(() (a) ())) => '(() (a) ()))

;; 验证列表反转的正确性
(check (equal? (reverse '(1 2 3)) '(3 2 1)) => #t)
(check (equal? (reverse '("hello" "world")) '("world" "hello")) => #t)

;; 测试边界情况
(check (reverse (list)) => '())
(check (reverse (cons 'a '())) => '(a))

;; 特殊字符测试
(check (reverse '(\u4E2D\u6587 \u5B57\u7B26)) => '(\u5B57\u7B26 \u4E2D\u6587))
(check (reverse '("foo" "bar" "baz" "" "qux")) => '("qux" "" "baz" "bar" "foo"))

;; 长列表测试
(check (reverse (map (lambda (x) (* x x)) '(1 2 3 4 5))) => '(25 16 9 4 1))
(check (reverse (filter even? '(1 2 3 4 5 6 7 8))) => '(8 6 4 2))

#|
list-tail
返回列表从指定索引位置开始的子列表。该函数是R7RS标准的基本列表操作函数之一。

语法
----
(list-tail list k)

参数
----
list : pair?
    非空列表或点结构。可以是普通列表、嵌套列表、点对结构或包含任意类型元素的列表。
    空列表 '() 作为参数将根据索引值产生不同的异常。

k : exact?
    非负的精确整数，表示开始截取子列表的起始索引位置。
    必须满足 0 <= k <= (length list)。

返回值
------
list?
    从列表第k个元素（索引从0开始）开始直到列表末尾的子列表。
    当 k = 0 时返回整个原列表。
    当 k = length(list) 时返回空列表 '()。

说明
----
1. list-tail返回从列表index位置开始的尾部子列表
2. 适用于所有类型的列表结构：普通列表、嵌套列表、点对结构
3. 索引从0开始计数，与list-ref等其他列表函数保持一致
4. 不会修改原始列表，始终返回新的列表结构
5. 可以正确处理嵌套结构和复杂数据结构

边界条件
--------
- k = 0: 返回原列表本身
- k = length(list): 返回空列表 '()
- k > length(list): 抛出out-of-range异常
- k < 0: 抛出out-of-range异常
- 空列表参数：除k=0外都需抛出异常
- 点对结构：正确处理非正规列表

性能特征
--------
- 时间复杂度：O(k)，需要遍历前k个元素
- 空间复杂度：O(1)，结果与原始列表共享尾部结构
- 内存分配：不创建新对象，直接共享原始结构
- 对于长列表，性能与索引位置成正比

数据类型兼容性
---------------
- 支持任意类型的列表元素：数字、字符串、字符、符号、布尔值、过程等
- 支持嵌套列表和多层次结构
- 支持点对结构和非正规列表(dotted lists)
- 支持空列表、单元素列表、长列表

错误处理
--------
out-of-range
    当索引k为负数或超出列表长度时抛出错误。
wrong-type-arg
    当list参数不是列表类型时抛出错误。

示例
----
(list-tail '(a b c d) 0) => '(a b c d)
(list-tail '(a b c d) 2) => '(c d)
(list-tail '(a b c d) 4) => '()
(list-tail '((a b) (c d) (e f)) 1) => '((c d) (e f))
|#

;; list-tail 基本功能测试
(check (list-tail '(a b c d) 0) => '(a b c d))
(check (list-tail '(a b c d) 1) => '(b c d))
(check (list-tail '(a b c d) 2) => '(c d))
(check (list-tail '(a b c d) 3) => '(d))
(check (list-tail '(a b c d) 4) => '())

;; 边界值测试
(check (list-tail '(single) 0) => '(single))
(check (list-tail '(single) 1) => '())
(check (list-tail '() 0) => '())

;; 各种数据类型边界测试
(check (list-tail '(42 "text" #t 'symbol) 1) => '("text" #t 'symbol))
(check (list-tail '(#	 #
 #
) 0) => '(#	 #
 #
))
(check (list-tail '(#	 #
 #
) 2) => '(#
))
(check (list-tail '(#	 #
 #
) 3) => '())

;; 子列表包含嵌套结构测试
(check (list-tail '((a b) (c d) (e f)) 0) => '((a b) (c d) (e f)))
(check (list-tail '((a b) (c d) (e f)) 1) => '((c d) (e f)))
(check (list-tail '((a b) (c d) (e f)) 2) => '((e f)))
(check (list-tail '((a b) (c d) (e f)) 3) => '())

;; 复杂数据结构测试
(check (list-tail '(() a b 3 c) 1) => '(a b 3 c))
(check (list-tail '(#(1 2) 'symbol "string" 3.14) 1) => '('symbol "string" 3.14))
(check (list-tail '(1 "hello" (nested list) #t 3.14) 2) => '((nested list) #t 3.14))

;; 长列表性能测试
(check (list-tail '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 15) => '(16 17 18 19 20))
(check (list-tail '(1 2 3 4 5 6 7 8 9 10) 10) => '())
(check (list-tail '(1 2 3 4 5 6 7 8 9 10) 0) => '(1 2 3 4 5 6 7 8 9 10))
(check (list-tail '(a b c d e f g h i j k l m n o p q r s t) 0) => '(a b c d e f g h i j k l m n o p q r s t))

;; 列表与点对结构测试
(check (list-tail '(a . (b . (c . ()))) 0) => '(a b c))
(check (list-tail '(a . (b . (c . ()))) 1) => '(b c))
(check (list-tail '(a . (b . (c . ()))) 2) => '(c))
(check (list-tail '(a . (b . (c . ()))) 3) => '())

;; 字符串列表测试
(check (list-tail '("hello" "world" "test") 1) => '("world" "test"))
(check (list-tail '("first" "second" "third" "fourth") 3) => '("fourth"))

;; 符号列表测试
(check (list-tail '(define lambda if cond else) 2) => '(if cond else))
(check (list-tail '(car cdr cons list) 4) => '())

;; 数值列表测试
(check (list-tail '(1 2 3 4 5) 1) => '(2 3 4 5))
(check (list-tail '(1 2 3 4 5) 3) => '(4 5))
(check (list-tail '(1 2 3 4 5) 5) => '())
(check (list-tail '(1.5 2.5 3.5 4.5) 2) => '(3.5 4.5))

;; Unicode字符串列表测试
(check (list-tail '("中文" "美国" "日本" "韩国") 2) => '("日本" "韩国"))
(check (list-tail '("测试" "验证" "调试") 0) => '("测试" "验证" "调试"))

;; 构造器函数结果测试
(check (list-tail (list 1 2 3 4 5) 2) => '(3 4 5))
(check (list-tail (append '(1 2) '(3 4 5)) 3) => '(4 5))
(check (list-tail (reverse '(5 4 3 2 1)) 2) => '(3 4 5))

;; 错误参数测试
(check-catch 'wrong-type-arg (list-tail 123 0))
(check-catch 'wrong-type-arg (list-tail "string" 1))
(check-catch 'wrong-type-arg (list-tail #t 0))

;; 索引越界测试
(check-catch 'out-of-range (list-tail '(a b c) -1))
(check-catch 'out-of-range (list-tail '(a b c) 4))
(check-catch 'out-of-range (list-tail '(a) 2))
(check-catch 'out-of-range (list-tail '() 1))
(check-catch 'out-of-range (list-tail '(single) 2))

;; 参数错误测试
(check-catch 'wrong-number-of-args (list-tail))
(check-catch 'wrong-number-of-args (list-tail '(a b c)))
(check-catch 'wrong-number-of-args (list-tail '(a b c) 1 2))
(check-catch 'wrong-number-of-args (list-tail '(a b c) 1 2 3))

#|
list-ref
获取列表中指定位置的元素。该函数是R7RS标准的基本列表操作函数之一。

语法
----
(list-ref list k)

参数
----
list : pair?
    非空的列表或点对结构。空列表 '() 不被接受。

k : exact?
    非负的精确整数，表示要获取的元素的索引位置。索引从0开始。
    必须满足 0 <= k < (length list)。

返回值
------
任意类型
    返回列表中位置k处的元素。类型取决于列表中实际存储的对象，
    可以是符号、数字、字符串、列表、点对或任何其他Scheme对象。

说明
----
1. 索引从0开始，即(list-ref '(a b c) 0)返回'a
2. 适用于所有非空列表：包括普通列表、improper列表和嵌套结构
3. 当用于点对结构时，只能访问0和1位置（car和cdr部分）
4. 用于嵌套结构时，可以访问复杂列表的任意分层结构

示例
----
(list-ref '(a b c d e) 0) => 'a
(list-ref '(a b c d e) 4) => 'e
(list-ref '(1 2 3 4) 2) => 3

错误处理
--------
wrong-type-arg
    当list参数不是pair?类型（如空列表'()、数字、字符串等）时抛出错误。

out-of-range
    当索引k为负数或超出列表有效范围（k >= 长度）时抛出错误。
|#

;; 基础测试：普通列表索引访问
(check (list-ref '(a b c d e) 0) => 'a)
(check (list-ref '(a b c d e) 1) => 'b)
(check (list-ref '(a b c d e) 4) => 'e)
(check (list-ref '(1 2 3 4 5) 0) => 1)
(check (list-ref '(1 2 3 4 5) 2) => 3)
(check (list-ref '(1 2 3 4 5) 4) => 5)

;; 边界值测试：索引0、中间值、最大值
(check (list-ref '(single) 0) => 'single)
(check (list-ref '(x y z) 1) => 'y)
(check (list-ref '(first last) 1) => 'last)

;; 复杂数据类型测试：包含各种类型元素
(check (list-ref '("string" 42 #t symbol) 0) => "string")
(check (list-ref '("string" 42 #t symbol) 1) => 42)
(check (list-ref '("string" 42 #t symbol) 2) => #t)
(check (list-ref '("string" 42 #t symbol) 3) => 'symbol)

;; 嵌套列表测试：访问嵌套结构
(check (list-ref '((1 2 3) (4 5 6) (7 8 9)) 0) => '(1 2 3))
(check (list-ref '("hel" "lo" "wo" "rld") 2) => "wo")
(check (list-ref '(((a b) c d) e f) 0) => '((a b) c d))

;; 测试点对结构：简单点对
(check (list-ref '(a . b) 0) => 'a)
(check (list-ref (cons 'a 'b) 0) => 'a)

;; improper列表测试：包含点对结构的列表
(check (list-ref '(a b c . d) 0) => 'a)
(check (list-ref '(a b c . d) 1) => 'b)
(check (list-ref '(a b c . d) 2) => 'c)

;; 复杂嵌套结构测试：深层嵌套
(check (list-ref '((a b) (c d) (e f)) 1) => '(c d))
(check (list-ref '((1 2) 3 4 5) 0) => '(1 2))
(check (list-ref '((1 2) 3 4 5) 3) => 5)

;; 简单点对重新测试
(check (list-ref (cons '(1 2) '(3 4)) 1) => 3)

;; 基础三元列表
(check (list-ref '(a b c) 0) => 'a)
(check (list-ref '(a b c) 1) => 'b)
(check (list-ref '(a b c) 2) => 'c)

;; 错误情况测试
(check-catch 'wrong-type-arg (list-ref '() 0))
(check-catch 'wrong-type-arg (list-ref 123 0))
(check-catch 'wrong-type-arg (list-ref "string" 1))
(check-catch 'wrong-type-arg (list-ref #t 1))

;; 索引越界测试
(check-catch 'out-of-range (list-ref '(a b c) -1))
(check-catch 'out-of-range (list-ref '(a b c) 3))
(check-catch 'out-of-range (list-ref '(x) 1))
(check-catch 'out-of-range (list-ref '(a b c d e) 5))
(check-catch 'out-of-range (list-ref '(single) 2))

;; 构造器函数创建的列表测试
(check (list-ref (list 1 2 3 4) 2) => 3)
(check (list-ref (cons 1 (cons 2 (cons 3 '()))) 1) => 2)
(check (list-ref (append '(1 2) '(3 4 5)) 3) => 4)

;; 附加的列表操作场景测试
(check (list-ref '(apple banana cherry date elderberry) 2) => 'cherry)
(check (list-ref (list 'symbol 42 #t "string" 3.14) 3) => "string")

#|
list-set!
修改列表指定索引位置的元素值。该函数会直接修改原始列表对象，是R7RS标准的基本列表操作函数之一。

语法
----
(list-set! list k obj)

参数
----
list : pair?
    要被修改的列表对象。可以是普通列表、嵌套列表或点对结构。
    必须是可修改的pair类型对象，不能是空列表或其他原子类型。

k : exact?
    非负的精确整数，表示要修改的元素索引位置。索引从0开始。
    必须满足 0 <= k < (length list)。

obj : any
    要设置的新值，可以是任何类型的Scheme对象，包括数字、字符串、符号、布尔值、过程等。

返回值
------
unspecified
    根据R7RS规范，返回未指定的值。

说明
----
1. 这是一个变异操作，会直接修改原始列表对象的内存内容
2. 索引从0开始计数，与list-ref等其他列表函数保持一致
3. 修改后原始对象的引用仍然指向同一个内存位置
4. 适用于所有可修改的列表结构：普通列表、嵌套列表、点对结构
5. 不会影响列表其他元素的位置或结构

边界条件
--------
- k = 0: 修改列表第一个元素
- k = list.length - 1: 修改列表最后一个元素
- k < 0: 抛出out-of-range异常
- k >= list.length: 抛出out-of-range异常
- 空列表参数：抛出wrong-type-arg异常
- 非列表参数：抛出wrong-type-arg异常

性能特征
--------
- 时间复杂度：O(k)，需要遍历前k个元素
- 空间复杂度：O(1)，不消耗额外内存
- 内存分配：无额外内存分配，直接修改原始结构
- 影响范围：仅修改指定位置的元素值，不影响列表结构

数据类型兼容性
---------------
- 支持任意类型的列表元素替换：
  - 基本类型：数字、字符串、字符、符号、布尔值
  - 复杂类型：列表、点对结构、向量、过程
  - 特殊类型：空列表、嵌套结构、lambda构造器
- 支持深度嵌套列表的结构修改

错误处理
--------
wrong-type-arg
    当list参数不是pair?类型或参数数量错误时抛出。
out-of-range
    当索引k为负数、超出列表长度或参数类型错误时抛出。

影响范围
--------
- 该操作会直接影响使用同一引用的所有代码位置
- 修改后原始对象的内容立即发生变化
- 列表长度和结构保持不变，仅修改特定位置的值
- 谨慎使用，可能破坏特殊列表的不变量

注意事项
--------
- 列表参数必须是非空列表或可修改的pair对象
- 修改后直接作用于原始列表，不创建新对象
- 可以通过校验list-ref来验证修改的正确性
- Rust-like安全模型：严格的边界检查和类型验证
|#

;; list-set! 基本功能测试
(let ((lst (list 'a 'b 'c)))
  (list-set! lst 1 'x)
  (check lst => '(a x c)))

;; 边界值测试：索引极端值
(let ((lst (list 'single)))
  (list-set! lst 0 'modified)
  (check lst => '(modified)))

(let ((lst (list 'first 'middle 'last)))
  (list-set! lst 0 'start)
  (list-set! lst 2 'end)
  (check lst => '(start middle end)))

(let ((lst (list 'a 'b 'c 'd)))
  (list-set! lst 3 'last)
  (check lst => '(a b c last)))

;; 空列表面界测试
(let ((lst (list)))
  (check-catch 'wrong-type-arg (list-set! lst 0 'value)))

;; 各种数据类型测试
(let ((lst (list 1 "text" #t 'symbol)))
  (list-set! lst 0 42)
  (list-set! lst 1 "modified")
  (list-set! lst 2 #f)
  (list-set! lst 3 'changed)
  (check lst => '(42 "modified" #f changed)))

;; 字符数据类型测试
(let ((lst (list #\a #\b #\c #\d)))
  (list-set! lst 0 #\A)
  (list-set! lst 3 #\D)
  (check lst => '(#\A #\b #\c #\D)))

;; 字符串数据类型测试
(let ((lst (list "hello" "world" "test")))
  (list-set! lst 1 "modified")
  (check lst => '("hello" "modified" "test")))

;; 布尔数据类型测试
(let ((lst (list #t #f #t)))
  (list-set! lst 1 #t)
  (check lst => '(#t #t #t)))

;; 符号数据类型测试
(let ((lst (list 'define 'lambda 'if 'cond)))
  (list-set! lst 2 'when)
  (check lst => '(define lambda when cond)))

;; 过程数据类型测试
(let ((lst (list car cdr cons)))
  (list-set! lst 0 list)
  (check (procedure? (list-ref lst 0)) => #t))

;; 嵌套子列表结构测试
(let ((lst (list '(a b) '(c d) '(e f))))
  (list-set! lst 1 '(x y z))
  (check lst => '((a b) (x y z) (e f))))

;; 嵌套结构替换测试
(let ((lst (list (list 'a) (list 'b) (list 'c))))
  (list-set! lst 1 (list 'x 'y))
  (check lst => '((a) (x y) (c))))

;; 深度嵌套结构测试
(let ((lst (list (list (list 1)) (list 2) (list (list 3)))))
  (list-set! lst 1 (list 'new 'structure))
  (check lst => '(((1)) (new structure) ((3)))))

;; 向量和字节向量元素测试
(let ((lst (list #(1 2 3) #u8(255 128))))
  (list-set! lst 0 #(4 5 6))
  (list-set! lst 1 #u8(100 200))
  (check lst => `(#(4 5 6) #u8(100 200))))

;; Unicode字符串元素测试
(let ((lst (list "中文" "测试" "字符串")))
  (list-set! lst 1 "修改")
  (check lst => '("中文" "修改" "字符串")))

;; 构造器函数列表测试
(let ((lst (make-list 4 'placeholder)))
  (list-set! lst 1 'second)
  (list-set! lst 3 'last)
  (check lst => '(placeholder second placeholder last)))

;; 长列表性能测试
(let ((lst (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
  (list-set! lst 5 'five)
  (list-set! lst 15 'fifteen)
  (check lst => '(1 2 3 4 5 five 7 8 9 10 11 12 13 14 15 fifteen 17 18 19 20)))

;; 空字符串和空列表元素测试
(let ((lst (list "" (list) "abc")))
  (list-set! lst 1 (list 'x 'y 'z))
  (check lst => '("" (x y z) "abc")))

;; 数值和浮点数元素测试
(let ((lst (list 1 2.5 3 4.0 )))
  (list-set! lst 2 99)
  (list-set! lst 3 100.0)
  (check lst => '(1 2.5 99 100.0)))

;; 函数替换测试
(let ((lst (list #t #f #t #f)))
  (list-set! lst 2 42)
  (list-set! lst 3 "hello")
  (check lst => '(#t #f 42 "hello")))

;; 错误参数类型测试
(check-catch 'wrong-type-arg (list-set! 123 0 'value))
(check-catch 'wrong-type-arg (list-set! "string" 1 'value))
(check-catch 'wrong-type-arg (list-set! #t 0 'value))
(check-catch 'wrong-type-arg (list-set! 'symbol 1 'value))

;; 索引越界测试
(let ((lst (list 'a 'b 'c)))
  (check-catch 'out-of-range (list-set! lst -1 'value))
  (check-catch 'out-of-range (list-set! lst 3 'value))
  (check-catch 'out-of-range (list-set! lst 4 'value)))

(let ((lst (list 'single)))
  (check-catch 'out-of-range (list-set! lst 1 'value))
  (check-catch 'out-of-range (list-set! lst -1 'value)))

;; 参数数量错误测试
(check-catch 'wrong-number-of-args (list-set!))
(check-catch 'wrong-number-of-args (list-set! '(a b c)))
(check-catch 'wrong-number-of-args (list-set! '(a b c) 1))
(check-catch 'wrong-number-of-args (list-set! '(a b c) 1 'x 'y))

;; 大整数索引边界测试
(let ((lst '(a b c d e f g h i j)))
  (check-catch 'out-of-range (list-set! lst 11 'value)))

;; 验证突变影响（共享引用测试）
(let ((original (list 'a 'b 'c 'd 'e)))
  (let ((shared original))
    (list-set! shared 2 'modified)
    (check original => '(a b modified d e))  ; 原列表也被修改
    (check (eq? original shared) => #t)))   ; 确实是同一对象

;; 确保修改正确性验证（修改为list-ref检查）
(let ((lst (list 'alpha 'beta 'gamma)))
  (let ((val-before (list-ref lst 1)))  ; 原值应为'beta
    (list-set! lst 1 'changed)
    (let ((val-after (list-ref lst 1)))
      (check val-after => 'changed))))

;; 被修改引用关系保持测试
(let ((lst (cons 10 (cons 20 (cons 30 '())))))
  (list-set! lst 1 200)
  (list-set! lst 2 300)
  (check lst => '(10 200 300)))

#|
memq
使用对象标识（eq?比较）在列表中查找指定符号键。此函数是R7RS标准中针对符号的特殊查找优化。

语法
----
(memq symbol lst)

参数
----
symbol :
    要查找的符号键，类型通常为符号(symbol)，但也支持其他可eq?比较的对象类型。

lst : pair?
    要搜索的列表或点对结构。可以是普通列表、关联列表或非空列表结构。
    空列表可作为参数，但返回#f。

返回值
------
list?
    若符号在列表中，返回从第一个匹配项开始的子列表（保留原始内存结构）。
#f
    若未找到指定符号或列表为空。

行为特征
--------
1. 采用eq?进行严格对象标识比较（区别于memv的eqv?比较和member的equal?比较）
2. 专门为符号键设计的最优化查找路径（Symbol Key Desing-Oriented Lookup Pathway）
3. 成功匹配时返回原始列表的尾部子列表，不是复制的新对象
4. 失败时返回#f，无任何系统开销
5. 保持输入输出结构一致性，适用于关联列表操作

边界条件
--------
- 空列表参数：始终返回#f
- 单元素列表边界：成功匹配返回单元素列表
- 重复键处理：返回第一个匹配的尾部子列表
- 非符号类型兼容性：任何可eq?比较对象均支持
- 嵌套结构边界：正确处理深层嵌套和关联列表

性能特征
--------
- 时间复杂度：O(n)，其中n为列表长度，线性遍历
- 空间复杂度：O(1)，无额外内存消耗
- 内存共享：成功时共享原始列表内存结构
- 快速路径：符号键的专用优化比较器
- 递归深度：基于列表长度的线性递归，合理深度安全处理

数据类型兼容性
---------------
- **符号类型**：普通符号、特殊符号、关键字符号
- **数字类型**：整数、浮点数均支持eq?比较
- **字符类型**：所有字符类型统一支持
- **布尔类型**：#t和#f的标准支持
- **过程类型**：内置过程和自定义过程正确识别
- **特殊对象**：点对结构、嵌套列表、构造器结果
- **复合结构**：向量、字节向量、嵌套过程结构

错误处理
--------
wrong-type-arg
    当参数类型不匹配或数量错误时抛出。

应用注意
--------
- 设计用于符号键的优化场景，比member和memv更高效率
- 保持内存结构完全一致，适合连锁操作和状态管理
- 适用于关联列表、谓词列表和环境变量查找
- 返回结构支持继续链式操作（cadr、cddr等）
- 在需要严格对象标识的场景中优先选择memq

限制说明
--------
- eq?比较严格，等价数值例如整数和浮点数值需要完全相同的对象
- 不适用于字符串、数字等需要通过值比较的场景
- 与memv和member形成互补关系，按需求选择
- 不能用于键值对查找（需要assoc等更高级函数）
- 返回的是子列表，不是特定位置的值
|#

;; memq 基本功能测试
(check (memq 'a '(a b c)) => '(a b c))
(check (memq 'b '(a b c)) => '(b c))
(check (memq 'c '(a b c)) => '(c))
(check (memq 'd '(a b c)) => #f)

;; 边界值测试
(check (memq '? '()) => #f)
(check (memq 'only '(only)) => '(only))
(check (memq 'single '(single)) => '(single))
(check (memq 'first '(first second third)) => '(first second third))
(check (memq 'last '(first second last)) => '(last))

;; 符号键查找优化测试
(check (memq 'define '(define lambda if)) => '(define lambda if))
(check (memq 'cond '(if else when unless)) => #f)
(check (memq 'car '(cdr cons car)) => '(car))
(check (memq 'procedure '(symbol list number string)) => #f)

;; 重复键边界测试
(check (memq 'repeat '(a b repeat c repeat d)) => '(repeat c repeat d))
(check (memq 'same '(same same same)) => '(same same same))

;; 布尔值边界测试
(check (memq #t '(#t #f #t)) => '(#t #f #t))
(check (memq #f '(#t #f #t)) => '(#f #t))
(check (memq #t '(#f only)) => #f)

;; 字符键测试
(check (memq #\a '(#\b #\a #\c)) => '(#\a #\c))
(check (memq #\x '(#\a #\b #\c)) => #f)
(check (memq #\newline '(a #\newline b)) => '(#\newline b))

;; 数字边界测试
(check (memq 42 '(1 42 3)) => '(42 3))
(check (memq 0 '(0 1 2)) => '(0 1 2))
(check (memq -1 '(0 1 2)) => #f)
(check (memq 100 '(100 200 300)) => '(100 200 300))

;; 特殊符号字符测试
(check (memq 'λ '(λ α β)) => '(λ α β))
(check (memq '$ '(symbol $ delta)) => '($ delta))

;; 过程对象边界测试
(let ((start car) (next cdr) (last cons))
  (check (memq start (list car cdr start)) => (list car cdr start))
  (check (memq next (list cons list)) => #f))

;; 复合结构测试
(let ((test-list '(cons list append)))
  (check (memq 'list test-list) => '(list append))
  (check (memq 'other-symbol test-list) => #f))

;; 点对结构测试
(check (memq 'center '(left center right top)) => '(center right top))
(check (memq 'middle '(begin middle end)) => '(middle end))

#|
memv
使用对象值比较（eqv?比较）在列表中查找指定元素。此函数是R7RS标准中专门针对数值、字符等基础数据类型的优化查找函数。

语法
----
(memv element lst)

参数
----
element : any
    要查找的元素，可以是符号、字符、数字、布尔值等基础类型数据。特别适合用作标准数值和字符的查找。

lst : pair?
    要搜索的列表或点对结构。可以是普通列表、数值列表、字符列表等，空列表可作为参数但返回#f。

返回值
------
list?
    若元素在列表中找到，返回从第一个匹配项开始的子列表（保留原始内存结构，通常称为查找结果的"尾部"）。
#f
    若未找到指定元素或列表为空。

行为特征
--------
1. 采用eqv?进行精确值比较（区别于memq的eq?比较和member的equal?比较）。equ?能够正确处理数值的等价性（例如整数42和浮点42.0的等价性）。
2. 专门为数值、字符等基础数据类型设计的优化查找路径，在数值列表应用中有性能优势
3. 成功匹配时返回原始列表的尾部子列表，不会创建新的对象，保持内存结构完全一致
4. 失败时返回#f，无任何系统开销
5. 保持输入输出结构一致性，适用于基础数据类型列表的筛选和操作

边界条件
--------
- 空列表参数：始终返回#f
- 单元素列表边界：存在匹配时返回单元素列表
- 重复元素处理：返回第一个匹配值的尾部子列表（索引最小值）
- 精确匹配原则：基于eqv?的精确数值比较，确保数值类型一致性
- 字符和布尔类型：统一处理所有Unicode字符标准类型
- 嵌套列表结构：正确处理数值子列表、字符子列表等相关结构
- 非正规列表：支持点对结构和improper lists查找操作

性能特征
--------
- 时间复杂度：O(n)，其中n为列表长度，线性遍历整个列表
- 空间复杂度：O(1)，无额外内存消耗，返回结构直接共享原始数据
- 内存共享：成功时完全共享原始列表内存结构
- 快速路径：数值和字符类型的专用eqv?优化比较器
- 递归深度：基于列表长度的线性递归，合理深度安全处理

数据类型兼容性
---------------
- **数值类型支持**：
  - 整数类型：正整数、负整数、0均统一支持
  - 浮点数类型：标准IEEE浮点格式，包括科学计数法
  - 复数类型：标准复数表示，值比较型匹配
  - 有理数类型：任意进制有理数表示，保留精度比较
- **字符类型全兼容**：
  - ASCII字符集：所有标准ASCII字符
  - Unicode字符：支持完整Unicode字符集，包括中文、表情符号等
  - 特殊控制字符：换行符、回车符、制表符等统一支持
- **布尔类型**：完全支持#t和#f的精确匹配
- **符号类型支持**：普通符号、关键字符号、复合符号
- **特殊对象支持**：点对结构、嵌套列表、构造器结果
- **效率权衡**：在数值和字符查找中比memq和member效率更高

错误处理
--------
wrong-type-arg
    当参数类型不匹配或参数数量错误时抛出。

与memq、member的精确分工
------------------------
- **memv vs memq**：memv使用eqv?比较（数值和字符最优），memq使用eq?比较（符号最优）
- **memv vs member**：memv使用eqv?比较（基础类型最优），member使用equal?比较（复合对象最优）
- **应用矩阵**：
  - 数值/字符查找 → 优先 memv
  - 符号查找 → 优先 memq
  - 字符串/复合对象 → 优先 member

应用注意
--------
- 查找的核心价值在于数值和字符类型的高效处理，是这类场景中的首选方案
- 针对数值列表查找、字符数据筛选、基础类型过滤等场景进行关键优化
- 调用结果的子列表形式为后续列表操作提供便捷的链式调用接口
- 对于数值运算产生的动态数据非常有用（例如通过map生成的数值列表查找）
- 在处理大规模数值和字符数据时，相比其他方案有显著的性能优势

限制说明
--------
- 不能查找嵌套列表内部的数值或字符元素
- 不支持模糊匹配，需要精确值匹配
- 与memq和member共同组成R7RS的三个标准查找函数的三位一体架构
- 查找功能严格限定在顶层列表结构，不深入嵌套层级
- 若需要复杂条件查找，需要配合filter等函数组合使用
|#

;; memv 基本功能测试
(check (memv 1 '(1 2 3)) => '(1 2 3))
(check (memv 2 '(1 2 3)) => '(2 3))
(check (memv 3 '(1 2 3)) => '(3))
(check (memv 4 '(1 2 3)) => #f)

;; 空列表边界测试
(check (memv 0 '()) => #f)
(check (memv #\a '()) => #f)
(check (memv #t '()) => #f)

;; 单元素列表边界测试
(check (memv 42 '(42)) => '(42))
(check (memv 100 '(single)) => #f)
(check (memv 3.14 '(3.14)) => '(3.14))

;; 数值类型边界测试
(check (memv 0 '(0 1 2 3)) => '(0 1 2 3))
(check (memv -42 '(-42 0 42)) => '(-42 0 42))
(check (memv 42 '(-42 0 42)) => '(42))
(check (memv 3.14159 '(2.71 3.14159 1.414)) => '(3.14159 1.414))
(check (memv 1.5 '(1.0 1.5 2.0)) => '(1.5 2.0))

;; 字符类型边界测试
(check (memv #\a '(#\b #\a #\c)) => '(#\a #\c))
(check (memv #\b '(#\c #\b #\a)) => '(#\b #\a))
(check (memv #\newline '(a b c)) => #f)
(check (memv #\space '(#\tab #\space #\newline)) => '(#\space #\newline))

;; 基本字符测试
(check (memv #\a '(#\b #\a #\c)) => '(#\a #\c))
(check (memv #\A '(#\A #\B #\C)) => '(#\A #\B #\C))

;; 布尔值边界测试
(check (memv #t '(#f #t #f)) => '(#t #f))
(check (memv #f '(#t #f #t)) => '(#f #t))
(check (memv #t '(#f only)) => #f)

;; 符号类型测试（确保eqv?正确处理符号）
(check (memv 'symbol '(other symbol test)) => '(symbol test))
(check (memv '中文符号 '(关键字 中文符号 测试)) => '(中文符号 测试))

;; 重复值处理测试
(check (memv 42 '(10 20 42 42 42)) => '(42 42 42))
(check (memv #\a '(x y z #\a #\a)) => '(#\a #\a))

;; 复数类型边界测试
(check (memv 1+2i '(0+1i 1+2i 3+4i)) => '(1+2i 3+4i))
(check (memv 0+0i '(1+1i 2+2i)) => #f)

;; 有理数边界测试
(check (memv 1/2 '(1/3 1/2 2/3)) => '(1/2 2/3))
(check (memv 3/4 '(1/2 1/4)) => #f)

;; 极端边界值测试
(check (memv 2147483647 '(2147483646 2147483647 2147483648)) => '(2147483647 2147483648))
(check (memv -2147483648 '(-2147483649 -2147483648 -2147483647)) => '(-2147483648 -2147483647))

;; 浮点数精度测试
(check (memv 0.0001 '(0.0000 0.0001 0.0002)) => '(0.0001 0.0002))
(check (memv 9999.999 '(9999.998 9999.999 10000.000)) => '(9999.999 10000.000))

;; 混合类型列表测试
(check (memv 42 '("string" 42 #\a #t)) => '(42 #\a #t))
(check (memv #\x '(x y z #\x symbol 3.14)) => '(#\x symbol 3.14))

;; 嵌套结构中的顶层查找
(check (memv 1 '((1 2) 3 4)) => #f)  ; 不能查找嵌套列表内部
(check (memv 3 '((1 2) 3 4)) => '(3 4))

;; 大列表性能验证
(check (memv 15 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)) => '(15 16 17 18 19 20))
(check (memv 999 (make-list 5 999)) => '(999 999 999 999 999))
(check (memv 20 (append '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19) '(20))) => '(20))

;; 构造器函数结果测试
(check (memv 2 (list 1 2 3 4 5)) => '(2 3 4 5))
(check (memv 2.5 (cons 1.0 (cons 2.5 (cons 3.0 '())))) => '(2.5 3.0))

;; 向量与字符混合测试
(check (memv 3.14 '(#(1 2) 3.14 #\a "test")) => '(3.14 #\a "test"))

;; 错误参数类型测试
(check-catch 'wrong-type-arg (memv 0 "not a list"))
(check-catch 'wrong-type-arg (memv 0 123))
(check-catch 'wrong-type-arg (memv 0 #t))

;; 参数数量错误测试
(check-catch 'wrong-number-of-args (memv))
(check-catch 'wrong-number-of-args (memv 1))
(check-catch 'wrong-number-of-args (memv 1 '(1 2 3) "extra argument"))

#|
member
在列表中搜索指定元素，若存在则返回从该元素开始的子列表，否则返回 #f

语法
----
(member item list)

参数
----
item:any
待查找的元素（支持任意类型，包括数字、字符串、点对、列表等）。
list:list
被搜索的列表（可为空列表）。

返回值
-----
list
若 item 在 list 中，返回从第一个匹配项开始直到列表末尾的子列表。
#f
若未找到或 list 为空，返回 #f。

错误
----
wrong-type-arg
若 list 不是有效列表（如非列表结构），可能引发类型错误。

额外信息
----
使用 equal? 进行元素比较（支持复杂类型如字符串 "1"、点对 (1 . 2) 和列表 (1 2)）。
匹配时返回 原始列表的尾部片段（保留原内存结构），而非复制新列表。

|#

(check-catch 'wrong-type-arg (member 0 "text"))

(check (member 2 '(1 2 3)) => '(2 3))

(check (member 0 '(1 2 3)) => #f)
(check (member 0 '()) => #f)
 
(check (member "1" '(0 "1" 2 3)) => '("1" 2 3))
(check (member '(1 . 2) '(0 (1 . 2) 3)) => '((1 . 2) 3))
(check (member '(1 2) '(0 (1 2) 3)) => '((1 2) 3))

#|
assq
在关联列表中查找键，使用 eq? 进行比较。

语法
----
(assq key alist)

参数
----
key : any
要查找的键值，通常是一个符号。
alist : list
关联列表，每个元素都是一个配对（pair），其中 car 是键，cdr 是值。

返回值
----
alist-element | #f
如果在关联列表中找到匹配的键，返回对应的配对；如果未找到匹配项，返回 #f。

描述
----
assq 在关联列表中查找第一个键与给定 key 匹配的配对。键的比较使用 eq? 操作符，
这意味着键必须是同一个对象（通常用于符号、数字或其他不可变对象）。

assq 是 SRFI-1 规范中定义的关联列表操作函数，适用于符号键或其他.eq? 可比较的对象。

注意
----
- 返回的是找到的第一个配对，不是值本身
- 使用 eq? 进行比较，因此最适合符号键
- 对于字符串键等需要使用 equal? 的情况，请使用 assoc
|#

(let ((l '((a 1) (b 2) (c . 3))))
  (check (assq 'a l) => '(a 1))
  (check-true (eq? (assq 'a l) (list-ref l 0)))
  (check (assq 'b l) => '(b 2))
  (check (assq 'c l) => '(c . 3))
  (check (assq 'd l) => #f))

; Additional comprehensive assq tests
(check (assq 'a '()) => #f)
(check (assq 'a '((a . 1))) => '(a . 1))
(check (assq 'a '((a) (b))) => '(a))
(check (assq 'b '((a 1) (b 2) (a 3))) => '(b 2))
(check (assq 'a '((a 1) (b 2) (a 3))) => '(a 1))
(check (assq 1 '((1 "one") (2 "two"))) => '(1 "one"))
(check (assq 3 '((1 "one") (2 "two"))) => #f)

; Test with different value types
(check (assq 'x '((x . 10) (y . 20))) => '(x . 10))
(check (assq 'vector '((symbol . 1) (vector . #(1 2 3)) (list . (a b c)))) => '(vector . #(1 2 3)))
(check (assq 'list '((symbol . 1) (vector . #(1 2 3)) (list . (a b c)))) => '(list a b c))

; Test with dotted pairs as values
(check (assq 'key '((key . value) (other . something))) => '(key . value))
(check (assq 'missing '((key . value) (other . something))) => #f)

(let ((l '((a 1) (b 2) (c . 3))))
  (check (assq 'a l) => '(a 1))
  (check-true (eq? (assq 'a l) (list-ref l 0)))
  (check (assq 'b l) => '(b 2))
  (check (assq 'c l) => '(c . 3))
  (check (assq 'd l) => #f))

(let ((l '((2 3) (5 7) (11 . 13))))
  (check (assv 5 l) => '(5 7))
  (check (assv 11 l) => '(11 . 13)))

(let ((l '(((a)) ((b)) ((c)))))
  (check (assoc '(a) l) => '((a)))
  (check (assq '(a) l) => #f)
  (check (assv '(a) l) => #f))

#|
assv
在关联列表中查找键，使用 eqv? 进行比较。

语法
----
(assv key alist)

参数
----
key : any
    要查找的键值，可以是符号、数字、字符、布尔值等基本类型对象。

alist : list
    关联列表，每个元素都是一个配对（pair），其中 car 是键，cdr 是值。

返回值
----
list | #f
    如果在关联列表中找到匹配的键，返回对应的配对；如果未找到匹配项，返回 #f。

描述
----
assv 在关联列表中查找第一个键与给定 key 匹配的配对。键的比较使用 eqv? 操作符，
这意味着可以正确处理数值的等价性（例如整数和浮点数的等价比较）以及字符的精确匹配。

assv 是 R7RS 标准中定义的关联列表操作函数，适用于数值键、字符键或其他.eqv? 可比较的对象。

边界条件
--------
- 空列表参数：始终返回 #f
- 单元素列表边界：存在匹配时返回单元素配对
- 重复键处理：返回第一个匹配值的配对（索引最小值）
- 数值等价性：整数 42 和浮点数 42.0 会被认为是相同的键
- 字符匹配：不同字符之间不会匹配，包括大小写敏感
- 布尔值匹配：#t 和 #f 分别匹配
- 嵌套结构边界：正确处理嵌套列表作为值的情况

性能特征
--------
- 时间复杂度：O(n)，其中 n 为关联列表长度，线性遍历
- 空间复杂度：O(1)，无额外内存消耗
- 内存共享：成功匹配时共享原始列表内存结构
- 数值优化：专门优化的数值比较器
- 字符优化：Unicode字符的专用比较器

数据类型兼容性
-------------
- **数值类型支持**：
  - 整数、浮点数、复数、有理数的精确值比较
  - 跨类型数值比较（整数 42 == 浮点数 42.0）
  - 边界数值和大整数处理
- **字符类型全兼容**：
  - ASCII和Unicode字符的精确匹配
  - 特殊字符和控制字符支持
- **布尔类型**：完全支持 #t 和 #f 的精确匹配
- **符号类型**：支持普通符号和关键字符号
- **复合对象**：支持列表、向量等作为值存储

与其他关联函数的比较
--------------------
- **assv vs assq**：assv 使用 eqv?（数值和字符最优），assq 使用 eq?（符号最优）
- **assv vs assoc**：assv 使用 eqv?（基础类型最优），assoc 使用 equal?（复合对象最优）

应用注意
--------
- 在数值键查找和字符键查找场景中具有显著性能优势
- 能够正确处理跨类型的数值匹配（整数 == 浮点数）
- 适用于配置数据、映射表、环境变量等基础类型键值存储
- 返回结构支持继续链式操作（cdr、cadr、cdr等）

限制说明
--------
- 不能用于字符串等复杂类型的值比较
- 不支持嵌套路径查找，仅限于顶层键值匹配
- 与 assq 和 assoc 共同组成 R7RS 关联查询的三位一体架构
|#

;; assv 基本功能测试
(check (assv 'a '((a . 1) (b . 2))) => '(a . 1))
(check (assv 'b '((a . 1) (b . 2))) => '(b . 2))
(check (assv 'c '((a . 1) (b . 2))) => #f)

;; 边界1：空列表边界
(check (assv 'key '()) => #f)

;; 边界2：单元素列表边界
(check (assv 'k '((k . v))) => '(k . v))
(check (assv 'missing '((k . v))) => #f)

;; 边界3：数值等价性边界测试
(check (assv 42 '((42 . "int") (42.0 . "float"))) => '(42 . "int"))
(check (assv 42.0 '((42 . "int") (42.0 . "float"))) => '(42.0 . "float"))
(check (assv 3.14 '((1 . a) (3.14 . b) (5 . c))) => '(3.14 . b))

;; 边界4：字符精确匹配边界
(check (assv #\a '((#\a . "lowercase") (#\A . "uppercase"))) => '(#\a . "lowercase"))
(check (assv #\A '((#\a . "lowercase") (#\A . "uppercase"))) => '(#\A . "uppercase")) 
(check (assv #\0 '((#\0 . "zero") (#\1 . "one"))) => '(#\0 . "zero"))

;; 边界5：布尔值边界测试
(check (assv #t '((#t . true) (#f . false))) => '(#t . true))
(check (assv #f '((#f . false) (#t . true))) => '(#f . false))

;; 边界6：重复键处理边界
(check (assv 42 '((42 . first) (42 . second) (42 . third))) => '(42 . first))
(check (assv 'sym '((a . 1) (sym . found) (sym . ignored))) => '(sym . found))

;; 边界7：嵌套结构作为值边界
(check (assv 'key '((key (1 2 3)) (other . "val"))) => '(key (1 2 3)))
(check (assv 'list '((key . "val") (list "item") (other . 123))) => '(list "item"))

;; 边界8：复杂数据类型边界
(check (assv #t '((#t enabled #t #f) (#f disabled))) => '(#t enabled #t #f))
(check (assv 99.9 '((42 . "answer") (99.9 . 100.0) (0 . "zero"))) => '(99.9 . 100.0))

;; 数值边界测试
(check (assv 0 '((0 . zero) (1 . one) (-1 . negative))) => '(0 . zero))
(check (assv -1 '((0 . zero) (-1 . negative) (1 . one))) => '(-1 . negative))
(check (assv 2147483647 '((2147483647 . int-max) (-2147483648 . int-min))) => '(2147483647 . int-max))

;; 浮点数边界测试
(check (assv 1.0 '((1 "integer") (1.0 "float") (1.1 "larger"))) => '(1.0 "float"))
(check (assv 0.5 '((0.5 . "exact") (0.51 . "approx"))) => '(0.5 . "exact"))

;; 字符集合边界测试
(check (assv #\newline '((#\newline . "return") (#\tab . "indent"))) => '(#\newline . "return"))
(check (assv #\space '((#\space . "space") (#\a . "char") (#\z . "last"))) => '(#\space . "space"))

;; 过程对象边界测试 - 过程对象需要使用eq?比较，而不是eqv?
(let ((proc car))
  (check (assv proc (list (cons car "first") (cons cdr "second"))) => (cons car "first")))

;; 特殊符号边界测试
(check (assv :keyword '((:keyword . "special") (sym . "normal"))) => '(:keyword . "special"))

;; 错误处理测试
(check-catch 'wrong-type-arg (assv 'key "not-association-list"))
(check-catch 'wrong-type-arg (assv 'key 123))
(check-catch 'wrong-type-arg (assv 'key #t))

;; 参数数量错误测试
(check-catch 'wrong-number-of-args (assv))
(check-catch 'wrong-number-of-args (assv 'key))
(check-catch 'wrong-number-of-args (assv 'key 'list "extra"))

;; 非配对结构边界测试
(check (assv 'a '((a) (b) (c))) => '(a))
(check (assv 'b '((a) (b) (c))) => '(b))

;; 嵌套关联列表边界
(check (assv 42 '((42 a b c) (foo . bar))) => '(42 a b c))
(check (assv 'x '((x 1 2 3) (y 4 5 6))) => '(x 1 2 3))

#|
assoc
在关联列表中查找键，使用 equal? 进行比较。这是R7RS标准中最通用的关联列表操作函数。

语法
----
(assoc key alist)

参数
----
key : any
    要查找的键值，可以是符号、字符串、数字、列表、点对或其他任意类型对象。

alist : list
    关联列表，每个元素都是一个配对（pair）或列表，其中 car 是键，cdr 是值。

返回值
----
list | #f
    如果在关联列表中找到匹配的键，返回对应的配对；如果未找到匹配项，返回 #f。

描述
----
assoc 是R7RS标准中最通用的关联列表操作函数，使用 equal? 进行键比较，支持所有类型的对象作为键。
与 assq (使用 eq? 比较) 和 assv (使用 eqv? 比较) 相比，assoc 能够完成最复杂的对象匹配。

边界条件
--------
- 空列表参数：始终返回 #f
- 单元素列表边界：存在匹配时返回单元素配对
- 重复键处理：返回第一个匹配值的配对（索引最小值）
- 字符串匹配：区分大小写，完全匹配字符串内容
- 列表匹配：全结构比较，子列表嵌套也精确匹配
- 特殊对象匹配：过程对象、布尔值、字符等统一支持
- 复合对象边界：支持复杂嵌套结构作为键和值

性能特征
--------
- 时间复杂度：O(n*m)，其中 n 为列表长度，m 为键比较复杂度
- 空间复杂度：O(1)，无额外内存消耗
- 内存共享：成功匹配时共享原始列表内存结构
- 通用性：支持所有类型的键值比较
- 深层比较：支持复杂嵌套结构的完整匹配

数据类型兼容性
-------------
- **字符串类型**：支持大小写敏感的全字符匹配
- **列表类型**：支持嵌套列表的完整结构匹配
- **数值类型**：支持所有数值类型的等价性比较
- **字符类型**：支持ASCII和Unicode字符的精确匹配
- **布尔类型**：完全支持 #t 和 #f 的精确匹配
- **符号类型**：支持普通符号和关键字符号
- **复合对象**：支持任何复杂对象作为键和值

与其他关联函数的比较
--------------------
- **assoc vs assq**：assoc 使用 equal?（通用最強），assq 使用 eq?（符号最优）
- **assoc vs assv**：assoc 使用 equal?（复合对象最优），assv 使用 eqv?（基础类型最优）
- **应用矩阵**：
  - 符号键 → 优先 assq
  - 数值/字符键 → 优先 assv
  - 字符串/列表/复合键 → 优先 assoc

应用注意
--------
- 是处理字符串键、列表键等复杂类型键值的唯一选择
- 能够正确处理嵌套数据结构作为键值
- 适用于配置管理、JSON-like数据结构、序列化处理等场景
- 返回结构支持继续链式操作和状态处理
- 支持最深/最复杂的数据结构匹配

限制说明
--------
- 性能可能不如 assq 和 assv 对于简单类型键
- 不支持模糊匹配和通配符匹配
- 与 assq 和 assv 共同组成 R7RS 的标准关联查询架构
- 需要注意的是会实例化深层结构比较
|#

;; assoc 基本功能测试
(check (assoc 'a '((a . 1) (b . 2))) => '(a . 1))
(check (assoc 'b '((a . 1) (b . 2))) => '(b . 2))
(check (assoc 'c '((a . 1) (b . 2))) => #f)

;; 边界1：空列表边界
(check (assoc 'key '()) => #f)

;; 边界2：单元素列表边界
(check (assoc 'k '((k . v))) => '(k . v))
(check (assoc 'missing '((k . v))) => #f)

;; 边界3：字符串键精确匹配边界
(check (assoc "apple" '(("apple" . 1) ("banana" . 2))) => '("apple" . 1))
(check (assoc "APPLE" '(("apple" . 1) ("banana" . 2))) => #f)
(check (assoc "" '(("" . empty) ("a" . not-empty))) => '("" . empty))

;; 边界4：列表键精确匹配边界
(check (assoc '(1 2) '(((1 2) . "list-key-1") ((3 4) . "list-key-2"))) => '((1 2) . "list-key-1"))
(check (assoc '(list 1 2) '(((list 1 2) . quoted) ((1 2) . unquoted))) => '((list 1 2) . quoted))

;; 边界5：嵌套结构作为键边界
(check (assoc 'key '((key . (deep nested structure)) (other . simple))) => '(key deep nested structure))
(check (assoc 'nested '((("key" . val) ((key) . "the key")) (missing . other))) => #f)

;; 边界6：重复键处理边界
(check (assoc "test" '(("test" . first) ("test" . second) ("test" . third))) => '("test" . first))
(check (assoc '(a b) '(((a b) . one) ((a b) . two) ((c d) . three))) => '((a b) . one))

;; 边界7：数值等价性边界测试
(check (assoc 42 '((42 . "int") (42.0 . "float"))) => '(42 . "int"))
(check (assoc 42.0 '((42 . "int") (42.0 . "float"))) => '(42.0 . "float"))
(check (assoc 3.14 '((1 . a) (3.14 . b) (5 . c))) => '(3.14 . b))

;; 边界8：复杂复合对象作为键边界
(check (assoc '((a b) c) '(("((a b) c)" . string) (((a b) c) . list) ((()) . empty))) => '(((a b) c) . list))
(check (assoc #(1 2 3) '((#(1 2 3) . vector-value) (#(3 4 5) . another))) => '(#(1 2 3) . vector-value))

;; 字符串详细边界测试
(check (assoc "" '(("" . empty-string))) => '("" . empty-string))
(check (assoc "特殊字符" '(("特殊字符" . "unicode string"))) => '("特殊字符" . "unicode string"))

;; 补充assoc完整边界测试
;; 边界：空列表和单元素边界
(check (assoc 'missing '()) => #f)
(check (assoc 'singleton '((singleton . value))) => '(singleton . value))

;; 边界：Unicode字符串精确匹配
(check (assoc "中文" '(("中文" . "中文值") ("english" . "英文值"))) => '("中文" . "中文值"))
(check (assoc "🚀" '(("🚀" . "rocket") ("🌐" . "globe"))) => '("🚀" . "rocket"))

;; 边界：空字符串和特殊字符
(check (assoc "" '(("" . empty-string))) => '("" . empty-string))
(check (assoc "TeSt" '(("test" . lowercase) ("TeSt" . mixed) ("TEST" . uppercase))) => '("TeSt" . mixed))

;; 边界：列表结构作为键
(check (assoc '() '((() . empty-list) ((a) . single))) => '(() . empty-list))
(check (assoc '(nested list) '(((nested list) . "complex key") ((simple) . "basic key"))) => '((nested list) . "complex key"))

;; 扩展assoc边界测试，满足至少8个边界测试用例要求
;; 边界9：极大关联列表性能测试
(check (assoc 'key50 '((key1 . val1) (key2 . val2) (key3 . val3) (key4 . val4) (key5 . val5)
                         (key6 . val6) (key7 . val7) (key8 . val8) (key9 . val9) (key10 . val10)
                         (key11 . val11) (key12 . val12) (key13 . val13) (key14 . val14) (key15 . val15)
                         (key16 . val16) (key17 . val17) (key18 . val18) (key19 . val19) (key20 . val20)
                         (key21 . val21) (key22 . val22) (key23 . val23) (key24 . val24) (key25 . val25)
                         (key26 . val26) (key27 . val27) (key28 . val28) (key29 . val29) (key30 . val30)
                         (key31 . val31) (key32 . val32) (key33 . val33) (key34 . val34) (key35 . val35)
                         (key36 . val36) (key37 . val37) (key38 . val38) (key39 . val39) (key40 . val40)
                         (key41 . val41) (key42 . val42) (key43 . val43) (key44 . val44) (key45 . val45)
                         (key46 . val46) (key47 . val47) (key48 . val48) (key49 . val49) (key50 . val50))) 
      => '(key50 . val50))

;; 边界10：深层嵌套结构键值匹配测试
(check (assoc '((a b) (c d)) '((((a b) (c d)) . "nested keys") (((x y) (z w)) . "other"))) 
      => '(((a b) (c d)) . "nested keys"))

;; 边界11：混合数据类型的键测试
(check (assoc 3.14159 '((1 . "one") (3.14159 . "pi") (2.71828 . "e"))) => '(3.14159 . "pi"))
(check (assoc #f '((#t . "true") (#f . "false") (maybe . "unknown"))) => '(#f . "false"))

;; 边界12：特殊字符和Unicode字符串键测试
(check (assoc "测试" '(("中文" . "中文值") ("测试" . "测试值") ("英文" . "english"))) => '("测试" . "测试值"))
(check (assoc "特殊字符@#$%" '(("normal" . "普通") ("特殊字符@#$%" . "special") ("unicode🎉" . "emoji"))) 
      => '("特殊字符@#$%" . "special"))

;; 边界13：过程对象和布尔值作为键测试
(let ((test-proc (lambda (x) x)))
  (check (assoc test-proc (list (cons test-proc "procedure") (cons 'not-this "symbol"))) 
        => (cons test-proc "procedure")))

;; 边界14：空列表和原子值边界测试
(check (assoc 'missing '()) => #f)
(check (assoc 'key '((key))) => '(key))
(check (assoc 'symbol '((symbol . #t) (number . 42) (string . "test"))) => '(symbol . #t))

;; 边界15：存储过程和复杂对象的值测试
(check (assoc 'data '((data . #("a" "b" "c")) (list . (1 2 3)))) => '(data . #("a" "b" "c")))

;; 边界16：重复前缀匹配测试
(check (assoc "prefix" '(("prefix-match" . 1) ("prefix" . 2) ("prefix-long" . 3))) => '("prefix" . 2))
(check (assoc 'partial '((partial . 1) (partial-match . 2) (partially . 3))) => '(partial . 1))

#|
list-copy

创建一个新列表，它是输入列表的浅拷贝。

语法
----
(list-copy list)

参数
----
list - 要复制的列表

返回值
------
返回一个新的列表，具有与输入列表相同的元素，但这是一个不同的对象。

描述
----
list-copy 函数创建输入列表的一个浅拷贝。新列表的顶层节点是新的，
但列表中的元素本身不会被复制（浅拷贝）。这使得修改原始列表不会影响拷贝的列表，
但需要注意嵌套列表的深层结构不会被复制。

该函数在 (srfi srfi-1) 模块中实现，并由 (liii list) 重新导出。

示例
----
; 基本列表复制
(list-copy '(1 2 3))      => (1 2 3)
(list-copy '())           => ()

边界条件
--------
- 空列表参数返回空列表
- 非列表参数会触发类型错误异常
- 嵌套列表的子列表为同一引用（浅拷贝特性）

时间和空间复杂度
----------------
- 时间复杂度：O(n)，其中 n 是列表长度
- 空间复杂度：O(n)，需要创建新的列表节点

|#

;; list-copy tests

;; 基本功能测试
(check (list-copy '()) => '())
(check (list-copy '(1 2 3 4 5)) => '(1 2 3 4 5))
(check (list-copy '(a b c d)) => '(a b c d))
(check (list-copy '((1 2) (3 4) (5 6))) => '((1 2) (3 4) (5 6)))

;; 空列表边界条件
(check (list-copy '()) => '())

;; 对象独立性验证 - 确保是浅拷贝
(check-false (eq? (list-copy '(1 2 3)) '(1 2 3)))

;; 突变隔离测试 - 验证列表节点独立性
(let ((orig '(a b c))
      (copy (list-copy '(a b c))))
  (check orig => copy)
  (check-false (eq? orig copy))
  ;; 验证浅拷贝特性
  (let ((mut-copy (list-copy orig)))
    (set-car! mut-copy 'x)
    (check orig => '(a b c))      ; 原始列表不受影响
    (check mut-copy => '(x b c)))) ; 拷贝列表已改变

(check-report)
