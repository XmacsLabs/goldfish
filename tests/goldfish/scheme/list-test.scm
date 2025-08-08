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
        (scheme base))

(check-set-mode! 'report-failed)
      
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

(check (map square (list 1 2 3 4 5)) => '(1 4 9 16 25))

(check
  (let ((v (make-vector 5)))
    (for-each (lambda (i) (vector-set! v i (* i i)))
              (iota 5))
    v)
  => #(0 1 4 9 16))

(check
  (let ((v (make-vector 5 #f)))
    (for-each (lambda (i) (vector-set! v i (* i i)))
              (iota 4))
    v)
  => #(0 1 4 9 #f))

(check
  (let ((v (make-vector 5 #f)))
    (for-each (lambda (i) (vector-set! v i (* i i)))
              (iota 0))
    v)
  => #(#f #f #f #f #f))

(check (memq #f '(1 #f 2 3)) => '(#f 2 3))
(check (memq 'a '(1 a 2 3)) => '(a 2 3))
(check (memq 2 '(1 2 3)) => '(2 3))

(check (memq 2.0 '(1 2.0 3)) => #f)
(check (memq 2+0i '(1 2+0i 3)) => #f)

(define num1 3)
(define num2 3)
(check (memq num1 '(3 num2)) => '(3 num2))
(check (memq 3 '(num1 num2)) => #f)
(check (memq 'num1 '(num1 num2)) => '(num1 num2))

(check (memq (+ 1 1) '(1 2 3)) => '(2 3))

(check (memv 2 '(1 2 3)) => '(2 3))
(check (memv 2.0 '(1 2.0 3)) => '(2.0 3))
(check (memv 2+0i '(1 2+0i 3)) => '(2+0i 3))

(check (memv 2 '(1 2.0 3)) => #f)
(check (memv 2 '(1 2+0i 3)) => #f)

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


(check-report)
