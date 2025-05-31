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
        (liii lang)
        (only (liii base) let1 identity)
        (liii cut)
        (liii case)
        (liii error))

(define == class=?)
(check-set-mode! 'report-failed)

(check ((@ + _ 2) 1) => 3)
(check ((@ list 1 _ 3 _ 5) 2 4) => (list 1 2 3 4 5))
(check ((@ list _ _) 'a 'b) => (list 'a 'b))

(check
  (let ((a 10))
    (define add (@ + (* a 2) _))  
    (set! a 100)
    (add 5))
=> 25)

(let ((x 5))
  (check 
    ((@ cons (+ x 1) _) 'y) 
   => (cons 6 'y)))

(check (procedure? (@ list 1 2)) => #t)
(check ((@ list 1 2)) => '(1 2))

(check ((@ _ 'a 'b) list) => (list 'a 'b))
(check ((@ map _ '(1 2 3)) (lambda (x) (+ x 1))) => '(2 3 4))
(check ((@ apply _ '(1 2 3)) +) => 6)

(check ((@ (@ + _ 1) _) 2) => 3)
(check ((@ _ _) (@ * _ 2) 3) => 6)

(typed-define (person (name string? "Bob") (age integer?))
  (string-append name " is " (number->string age) " years old"))

(check (person :age 21) => "Bob is 21 years old")
(check (person :name "Alice" :age 25) => "Alice is 25 years old")
(check-catch 'type-error (person :name 123 :age 25))

(check-catch 'syntax-error
  (eval
    '(define-case-class instance-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (%name)
        name))))

(check-catch 'syntax-error
  (eval
    '(define-case-class static-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (@name)
        name))))

(check-catch 'syntax-error
  (eval
  '(define-case-class internal-methods-conflict-test
      ((name string?)
       (test-name string?)
       (age integer?))
      (define (test-name str)
        (string-append str " ")))))

(define-case-class person
  ((name string? "Bob")
   (age integer?)))

(let1 bob (person :name "Bob" :age 21)
  (check (bob 'name) => "Bob")
  (check (bob 'age) => 21)
  (check ((bob :name "hello") 'name) => "hello")
  (check-catch 'value-error (bob 'sex))
  (check-catch 'value-error (bob :sex))
  (check-true (bob :is-instance-of 'person))
  (check-true (person :is-type-of bob))
  (check (bob :to-string) => "(person :name \"Bob\" :age 21)"))

(check-catch 'type-error (person 1 21))

(let ((bob (person "Bob" 21))
      (get-name (lambda (x)
                 (case* x
                   ((#<procedure?>) (x 'name))
                   (else (value-error))))))
  (check (get-name bob) => "Bob")
  (check-catch 'value-error (get-name 1)))

(define-case-class jerson
  ((name string?)
   (age integer?))
  
  (define (%to-string)
    (string-append "I am " name " " (number->string age) " years old!"))
  (define (%greet x)
    (string-append "Hi " x ", " (%to-string)))
)

(let1 bob (jerson "Bob" 21)
  (check (bob :to-string) => "I am Bob 21 years old!")
  (check (bob :greet "Alice") => "Hi Alice, I am Bob 21 years old!"))

(define-case-class anonymous ()
  (define name "")

  (define (%get-name) name)

  (define (%set-name! x)
    (set! name x))
)

(let1 p (anonymous)
  (p :set-name! "Alice")
  (check (p :get-name) => "Alice"))

(define-case-class my-bool ()
  (define data #t)

  (define (%set-true!)
    (set! data #t))
  (define (%set-false!)
    (set! data #f))
 
  (define (%true?) data)
  (define (%false?) (not (%true?)))
  
  (define (@apply x)
    (let1 r (my-bool)
      (cond ((eq? x 'true)
             (r :set-true!))
            ((eq? x 'false)
             (r :set-false!))
            ((boolean? x)
             (if x (r :set-true!) (r :set-false!)))
            (else (r :set-false!)))
      r))
)

(check-true ((my-bool 'true) :true?))
(check-true ((my-bool 'false) :false?))
(check-true ((my-bool #t) :true?))
(check-true ((my-bool #f) :false?))
(check-true (my-bool :is-type-of (my-bool 'true)))

(define-case-class test-case-class
  ((name string?))
  
  (define (@this-is-a-static-method)
    (test-case-class "static"))
  
  (define (%this-is-a-instance-method)
    (test-case-class (string-append name "instance")))
)

(let1 hello (test-case-class "hello ")
  (check-catch 'value-error (hello :this-is-a-static-method))
  (check (test-case-class :this-is-a-static-method) => (test-case-class "static")))

(let ()
  (define-case-class person ((name string?) (country string?))
    (define (@default)
      (person "Andy" "China"))
    (define (%set-country! c . xs)
      (set! country c)
      (apply (%this) (if (null? xs) '(:this) xs)))
    (define (%set-name! n . xs)
      (set! name n)
      (apply (%this) (if (null? xs) '(:this) xs)))
    (define (%to-string)
      (format #f "Hello ~a from ~a" name country)))

  (define Andy (person :default))
  (check-catch 'wrong-type-arg (person :this))
  (check (Andy :to-string) => "Hello Andy from China")
  (check (Andy :set-country! "USA" :to-string) => "Hello Andy from USA")
  (check (Andy :to-string) => "Hello Andy from USA")
  (check (Andy :set-country! "China" :set-name! "Ancker-0" :to-string) => "Hello Ancker-0 from China")
  (check (Andy :set-country! "China") => (person "Ancker-0" "China"))
  (check (Andy :this :set-country! "USA" :this :set-name! "Andy" :this :to-string) => "Hello Andy from USA")
  (check-true (person :is-type-of Andy)))

(let ()
  (define-case-class person ((name string?) (country string?))
    (chained-define (@default)
      (person "Andy" "China"))
    (chained-define (set-country! c)
      (set! country c)
      (%this))
    (chained-define (set-name! n)
      (set! name n)
      (%this))
    (chained-define (%set-both! n c)
      (set-country! c)
      (set-name! n)
      (%this))
    (chained-define (%to-string)
      (rich-string (format #f "Hello ~a from ~a" name country))))
  (check (person :default :to-string :get) => "Hello Andy from China")
  (check (person :default :set-both! "Bob" "Russia" :to-string :get) => "Hello Bob from Russia")
  (check-catch 'value-error (person :default :set-country! "French")))

(define-object string-utils
  (define (@concat x y)
    (string-append x y))
)

(check (string-utils :concat "a" "b") => "ab")

(define-object object1
  (define x 0)
  (define (@concat x y) 
    (string-append x y))
)

(define-object object2
  (define y 0)
  (define (@return-object1) object1)
)

(check ((object2 :return-object1) :concat "a" "b") => "ab")

;; Test define-class (可变类)
(let ()
  (define-class person
    ((name string? "")
     (age integer? 0))
    
    (define (@apply name)
      (let1 r (person)
        (r :set-name! name)
        (r :set-age! 10)
        r)))
  
  ;; 测试@apply
  (define p1 (person))
  (define p2 (person "Bob"))
  
  ;; 测试setter和getter
  (p1 :set-name! "Alice")
  (p1 :set-age! 25)
  (check (p1 :get-name) => "Alice")
  (check (p1 :get-age) => 25)
  (check (p2 :get-name) => "Bob")
  (check (p2 :get-age) => 10)
  
  (check-true (person :is-type-of p1))
  (check-true (person :is-type-of p2))

  ;; 测试类型检查
  (check-catch 'type-error (p1 :set-name! 123))
  (check-catch 'type-error (p1 :set-age! "invalid"))
)

(check-false (case-class? (lambda (x) x)))
(check-false (case-class? +))
(check-false (case-class? identity))

(let ((bob (person "Bob" 21)))
  (check-true (case-class? bob))
  (check-false (case-class? +))
  (check-false (case-class? 42))
)

(check (class=? (list 1 2) (list 1 2)) => #t)
(check (class=? (box 10) 10) => #t)  
(check (class=? 10 (box 10)) => #t)  
(check (class=? (box 10) (box 10)) => #t)  
(check (class=? 10 10) => #t)  
(check-true (class=? (person "Bob" 21) (person "Bob" 21)))

(let ()
  (define-case-class person ((name string?) (country string?))
    (chained-define (@default)
      (person "Andy" "China"))
    (chained-define (%set-country! c)
      (set! country c)
      (%this))
    (chained-define (%set-name! n)
      (set! name n)
      (%this))
    (chained-define (%set-both! n c)
      (%this :set-name! n :set-country! c))
    (chained-define (%to-string)
      (rich-string (format #f "Hello ~a from ~a" name country))))
  (check (person :default :to-string :get) => "Hello Andy from China")
  (check (person :default :set-both! "Bob" "Russia" :to-string :get) => "Hello Bob from Russia")
  (check ((person "Alice" "Japan") :set-name! "Lily" :to-string :get) => "Hello Lily from Japan"))

(check
  (with-output-to-string
    (lambda ()
      (display* "hello world" "\n")))
  => "hello world\n")

(let1 bob (person "Bob" 21)
  (check (object->string bob) => "(person :name \"Bob\" :age 21)"))

(check (object->string 42) => "42")
(check (object->string "hello") => "\"hello\"")
(check (object->string #\a) => "#\\a")
(check (object->string '(1 2 3)) => "(1 2 3)")
(check (object->string #(1 2 3)) => "#(1 2 3)")

(check (box 42) => (rich-integer 42))
(check (box 3.14) => (rich-float 3.14))
(check (box #\a) => (rich-char 97))
(check (box "hello") => (rich-string "hello"))
(check (box '(1 2 3)) => (rich-list '(1 2 3)))
(check (box #(1 2 3)) => (rich-vector #(1 2 3)))
(check (box (hash-table 'a 1 'b 2)) => (rich-hash-table (hash-table 'a 1 'b 2)))
(check-catch 'type-error (box #t))

(check ($ 1 :to 3) => '(1 2 3))
(check ($ "hello world" :replace "world" "suger" :index-of "suger") => 6)
(check ($ '(1 2 3) :empty?) => #f)

(check
 (($ 100 :to 128)
  :take 10
  :map (@ + _ 1)
  :filter even?
  :collect)
  => '(102 104 106 108 110))

(check ($ 42 :get) => 42)

(check-true ($ 42 :equals ($ 42)))
(check-false ($ 41 :equals ($ 42)))

(check (($ 1 :to 2) :collect) => (list 1 2))
(check (($ 1 :to 1) :collect) => (list 1))
(check (($ 2 :to 1) :collect) => (list ))

(check (($ 1 :until 3) :collect) => (list 1 2))
(check (($ 1 :until 2) :collect) => (list 1))
(check (($ 2 :until 2) :collect) => (list ))

(check ($ 65 :to-rich-char) => #\A)
(check-catch 'value-error ($ #x110000 :to-rich-char))

(check ($ 1 :to-string) => "1")
(check ($ -10 :to-string) => "-10")
(check ($ 0 :to-string) => "0")

(check (+ 1 (rich-integer :max-value)) => (rich-integer :min-value))

(check (- (rich-integer :min-value) 1) => (rich-integer :max-value))

(check ($ 0 :sqrt) => 0)       
(check ($ 1 :sqrt) => 1)       
(check ($ 2 :sqrt) => 1)       
(check ($ 9 :sqrt) => 3)       
(check ($ 8 :sqrt) => 2)
(check ($ 10 :sqrt) => 3)
(check ($ 144 :sqrt) => 12)       
(check ($ 289 :sqrt) => 17)       
(check ($ 290 :sqrt) => 17)       
(check ($ 10201 :sqrt) => 101)       
(check ($ 10403 :sqrt) => 101) 
(check ($ (rich-integer :max-value) :sqrt) => 3037000499)
(check-catch 'value-error ($ -1 :sqrt))

(check ($ 1/3 :get) => 1/3)
(check ($ 0 :get) => 0)
(check ($ -1/3 :get) => -1/3)

(check ($ 1/3 :abs) => 1/3)
(check ($ 0.0 :abs) => 0.0)
(check ($ -1/3 :abs) => 1/3)

(check ($ 12.2 :get) => 12.2)

(check ($ 1.1 :abs) => 1.1)
(check ($ 0.0 :abs) => 0.0)
(check ($ -1.1 :abs) => 1.1)

(check ($ 1.1 :to-string) => "1.1")
(check ($ 0.0 :to-string) => "0.0")
(check ($ -1.2 :to-string) => "-1.2")
(check ($ 1.0 :to-string) => "1.0")

(check ($ 0.0 :sqrt) => 0.0)       
(check ($ 1.0 :sqrt) => 1.0)       
(check ($ 1.44 :sqrt) => 1.2)       
(check ($ 1.69 :sqrt) => 1.3)       
(check-catch 'value-error ($ -1.5 :sqrt))

(check-true ((rich-char #x30) :equals (rich-char #x30)))
(check-false ((rich-char #x31) :equals (rich-char #x30)))

(check-true ((rich-char #x0) :ascii?))
(check-true ((rich-char #x7f) :ascii?))
(check-false ((rich-char #x8f) :ascii?))

(check-true ($ #\a :ascii?))
(check-true ($ #\Z :ascii?))

;; 数字字符
(check-true ($ #\3 :numeric?))
(check-true ($ #\4 :numeric?))
(check-true ($ #\5 :numeric?))
(check-true ($ #\0 :numeric?))

;; 非数字字符
(check-false ($ #\[ :numeric?))
(check-false ($ #\@ :numeric?))
(check-false ($ #\; :numeric?))
(check-false ($ #\P :numeric?))
(check-false ($ #\x :numeric?))

;; Unicode测试
(let ((char1 (rich-char 48))  ;; ASCII '0'
      (char2 (rich-char #xFF10))  ;; 全角 '０'
      (char3 (rich-char #x0660))  ;; 阿拉伯数字 '٠'
      (char4 (rich-char #x06F0))  ;; 扩展阿拉伯数字 '۰'
      (char5 (rich-char #x0966))  ;; 印度数字
      (char6 (rich-char #x09E6))  ;; 孟加拉数字
      (char7 (rich-char #x0A66))  ;; 古尔穆奇数字
      (char8 (rich-char #x0AE6))  ;; 古吉拉特数字
      (char9 (rich-char #x0B66))  ;; 奥里亚数字
      (char10 (rich-char #x0BE6))  ;; 泰米尔数字
      (char11 (rich-char #x0C66))  ;; 泰卢固数字
      (char12 (rich-char #x0CE6))  ;; 卡纳达数字 
      (char13 (rich-char #x0D66))  ;; 马拉雅拉姆数字
      (char14 (rich-char #x0E50))  ;; 泰文数字 '๐'
      (char15 (rich-char #x0ED0))  ;; 老挝数字
      (char16 (rich-char #x0F20))  ;; 藏文数字
      (char17 (rich-char #x1040))  ;; 缅甸数字 '၀'
      (char18 (rich-char #x17E0))  ;; 高棉数字 '០'
      (char19 (rich-char #x1810))  ;; 蒙古数字 '᠐'
      (char20 (rich-char 65)))  ;; ASCII 'A'
  
  (check (char1 :numeric?) => #t)  ;; ASCII 数字
  (check (char2 :numeric?) => #f)  ;; 全角数字
  (check (char3 :numeric?) => #f)  ;; 阿拉伯数字
  (check (char4 :numeric?) => #f)  ;; 扩展阿拉伯数字
  (check (char5 :numeric?) => #f)  ;; 印度数字
  (check (char6 :numeric?) => #f)  ;; 孟加拉数字
  (check (char7 :numeric?) => #f)  ;; 古尔穆奇数字
  (check (char8 :numeric?) => #f)  ;; 古吉拉特数字
  (check (char9 :numeric?) => #f)  ;; 奥里亚数字
  (check (char10 :numeric?) => #f)  ;; 泰米尔数字
  (check (char11 :numeric?) => #f)  ;; 泰卢固数字
  (check (char12 :numeric?) => #f)  ;; 卡纳达数字
  (check (char13 :numeric?) => #f)  ;; 马拉雅拉姆数字
  (check (char14 :numeric?) => #f)  ;; 泰文数字
  (check (char15 :numeric?) => #f)  ;; 老挝数字
  (check (char16 :numeric?) => #f)  ;; 藏文数字
  (check (char17 :numeric?) => #f)  ;; 缅甸数字
  (check (char18 :numeric?) => #f)  ;; 高棉数字
  (check (char19 :numeric?) => #f)  ;; 蒙古数字
  (check (char20 :numeric?) => #f))  ;; 非数字字符
;; 大写字母
(check-true ($ #\A :upper?))
(check-true ($ #\Z :upper?))

;; 小写字母
(check-false ($ #\a :upper?))
(check-false ($ #\z :upper?))

;; 非字母字符
(check-false ($ #\0 :upper?))
(check-false ($ #\@ :upper?))  ;; @ 符号 (ASCII 64)
(check-false ($ #\[ :upper?))  ;; 左方括号 (ASCII 91)

;; 小写字母
(check-true ($ #\a :lower?))
(check-true ($ #\z :lower?))

;; 大写字母
(check-false ($ #\A :lower?))
(check-false ($ #\Z :lower?))

;; 非字母字符
(check-false ($ #\0 :lower?))
(check-false ($ #\` :lower?))  ;; 反引号 (ASCII 96)
(check-false ($ #\{ :lower?))  ;; 左花括号 (ASCII 123)

(let ((char1 (rich-char 48))  ;; ASCII '0'
      (char2 (rich-char #xFF10))  ;; 全角 '０'
      (char3 (rich-char #x0660))  ;; 阿拉伯数字 '٠'
      (char4 (rich-char #x06F0))  ;; 扩展阿拉伯数字 '۰'
      (char5 (rich-char #x0966))  ;; 印度数字
      (char6 (rich-char #x09E6))  ;; 孟加拉数字
      (char7 (rich-char #x0A66))  ;; 古尔穆奇数字
      (char8 (rich-char #x0AE6))  ;; 古吉拉特数字
      (char9 (rich-char #x0B66))  ;; 奥里亚数字
      (char10 (rich-char #x0BE6))  ;; 泰米尔数字
      (char11 (rich-char #x0C66))  ;; 泰卢固数字
      (char12 (rich-char #x0CE6))  ;; 卡纳达数字 
      (char13 (rich-char #x0D66))  ;; 马拉雅拉姆数字
      (char14 (rich-char #x0E50))  ;; 泰文数字 '๐'
      (char15 (rich-char #x0ED0))  ;; 老挝数字
      (char16 (rich-char #x0F20))  ;; 藏文数字
      (char17 (rich-char #x1040))  ;; 缅甸数字 '၀'
      (char18 (rich-char #x17E0))  ;; 高棉数字 '០'
      (char19 (rich-char #x1810))  ;; 蒙古数字 '᠐'
      (char20 (rich-char 65)))  ;; ASCII 'A'

  ;; 测试 %digit?
  (check (char1 :digit?) => #t)  ;; ASCII 数字
  (check (char2 :digit?) => #t)  ;; 全角数字
  (check (char3 :digit?) => #t)  ;; 阿拉伯数字
  (check (char4 :digit?) => #t)  ;; 扩展阿拉伯数字
  (check (char5 :digit?) => #t)  ;; 印度数字
  (check (char6 :digit?) => #t)  ;; 孟加拉数字
  (check (char7 :digit?) => #t)  ;; 古尔穆奇数字
  (check (char8 :digit?) => #t)  ;; 古吉拉特数字
  (check (char9 :digit?) => #t)  ;; 奥里亚数字
  (check (char10 :digit?) => #t)  ;; 泰米尔数字
  (check (char11 :digit?) => #t)  ;; 泰卢固数字
  (check (char12 :digit?) => #t)  ;; 卡纳达数字
  (check (char13 :digit?) => #t)  ;; 马拉雅拉姆数字
  (check (char14 :digit?) => #t)  ;; 泰文数字
  (check (char15 :digit?) => #t)  ;; 老挝数字
  (check (char16 :digit?) => #t)  ;; 藏文数字
  (check (char17 :digit?) => #t)  ;; 缅甸数字
  (check (char18 :digit?) => #t)  ;; 高棉数字
  (check (char19 :digit?) => #t)  ;; 蒙古数字
  (check (char20 :digit?) => #f))  ;; 非数字字符

(check ($ #\a :to-upper) => #\A)
(check ($ #\z :to-upper) => #\Z)
(check ($ #\A :to-upper) => #\A)
(check ($ #\Z :to-upper) => #\Z)
(check ($ #\@ :to-upper) => #\@)

(check ($ #\Z :to-upper :to-lower) => #\z) ; chain

(check ($ #\A :to-lower) => #\a)
(check ($ #\Z :to-lower) => #\z)
(check ($ #\a :to-lower) => #\a)
(check ($ #\z :to-lower) => #\z)
(check ($ #\@ :to-lower) => #\@)

(check ($ #\z :to-lower :to-upper) => #\Z) ; chain

(check ($ #\space :to-string) => "#\\space")
(check ($ #\return :to-string) => "#\\return")

(check ($ #\a :to-string) => "#\\a")
(check ($ #\A :to-string) => "#\\A")

(check ((rich-char #xA3) :to-string) => "#\\£")

(check ((rich-char #x4E2D) :to-string) => "#\\中")
(check (object->string (rich-char #x4E2D)) => "#\\中")

(check ((rich-char #x1F600) :to-string) => "#\\😀")


(check ($ #\space :make-string) => " ")
(check ($ #\return :make-string) => (string #\return))

(check ($ #\a :make-string) => "a")
(check ($ #\A :make-string) => "A")

(check ((rich-char #xA3) :make-string) => "£")
(check ((rich-char #x4E2D) :make-string) => "中")
(check ((rich-char #x1F600) :make-string) => "😀")

(check-true (rich-string :is-type-of ($ "Hello")))

(check-false (rich-string :is-type-of "hello"))
(check-false (rich-string :is-type-of 1))
(check-false (rich-string :is-type-of (box 1)))

(check (rich-string :value-of #\a) => "a")
(check (rich-string :value-of 'a) => "a")
(check (rich-string :value-of 123) => "123")
(check (rich-string :value-of 1.0) => "1.0")
(check (rich-string :value-of "abc") => "abc")
(check (rich-string :value-of (rich-char #x4E2D)) => "中")
(check (rich-string :value-of #\ ) => " ")

(check ($ "abc" :get) => "abc")
(check ($ "" :get) => "")

(check ((rich-string "abc") :length) => 3)
(check ((rich-string "中文") :length) => 2)
(check (rich-string :empty :length) => 0)

(let1 str ($ "你好，世界")
  (check (str :char-at 0) => (rich-char #x4F60))  ;; "你" 的 Unicode 码点
  (check (str :char-at 1) => (rich-char #x597D))  ;; "好" 的 Unicode 码点
  (check (str :char-at 2) => (rich-char #xFF0C))  ;; "，" 的 Unicode 码点
  (check (str :char-at 3) => (rich-char #x4E16))  ;; "世" 的 Unicode 码点
  (check (str :char-at 4) => (rich-char #x754C))  ;; "界" 的 Unicode 码点
  (check-catch 'out-of-range (str :char-at 10)))

(let1 str ($ "Hello，世界")
   (check (str 0) => ($ #\H))
   (check (str 7) => (rich-char :from-string "#\\界")))

(let1 s ($ "你好世界HelloWord")
  (check ((s :find (@ _ :equals ($ "你" 0))) :get) 
          => ($ "你" 0))
  (check-true ((s :find (@ _ :equals ($ "师" 0))) :empty?)))

(let1 s ($ "你好世界HelloWord")
  (check ((s :find-last (@ _ :equals ($ "你" 0))) :get) 
          => ($ "你" 0))
  (check-true ((s :find-last (@ _ :equals ($ "师" 0))) :empty?)))

(check ($ "你好" :head) => ($ "你" 0))
(check-catch 'index-error (rich-string :empty :head))
(check ($ "hello" :head-option) => (option #\h))
(check (rich-string :empty :head-option) => (none))

(check ($ "你好" :last) => ($ "好" 0))
(check-catch 'index-error (rich-string :empty :last))

(check ($ "hello" :last-option) => (option #\o))
(check (rich-string :empty :last-option) => (none))

(let1 str ($ "Hello，世界")
   (check (str :slice 0 5) => ($ "Hello"))
   (check (str :slice -10 5) => ($ "Hello"))
   (check (str :slice 6 100) => ($ "世界"))
   (check (str :slice 6 2) => ($ ""))
   (check (str :slice -3 -2) => ($ ""))
   (check (str :slice 100 101) => ($ ""))
   (check (str :slice -1 100) => ($ "Hello，世界"))
   (check (str :slice 0 5 :to-string) => "Hello"))

(let1 str ($ "Hello，世界")
  (check (str :take -1) => "")
  (check (str :take 0) => "")
  (check (str :take 1) => "H")
  (check (str :take 8) => "Hello，世界")
  (check (str :take 9) => "Hello，世界"))

(let1 str ($ "Hello，世界")
  (check (str :take-right -1) => "")
  (check (str :take-right 0) => "")
  (check (str :take-right 1) => "界")
  (check (str :take-right 8) => "Hello，世界")
  (check (str :take-right 9) => "Hello，世界"))

(let1 str ($ "Hello，世界")
  (check (str :drop 1) => "ello，世界")
  (check (str :drop 0) => "Hello，世界")
  (check (str :drop -1) => "Hello，世界")
  (check (str :drop 6) => "世界")
  (check (str :drop 7) => "界")
  (check (str :drop 8) => "")
  (check (str :drop 9) => ""))

(check (rich-string :empty :drop 1) => "")

(let1 str ($ "Hello，世界")
  (check (str :drop-right -1) => "Hello，世界")
  (check (str :drop-right 0) => "Hello，世界")
  (check (str :drop-right 1) => "Hello，世")
  (check (str :drop-right 8) => "")
  (check (str :drop-right 8) => ""))

(check ($ "42") => ($ "42"))
(check-false ($ "41" :equals ($ "42")))

(check-true ((rich-string "") :empty?))
(check-false ((rich-string "abc") :empty?))

(check-false ($ "全部都是中文" :forall (@ _ :digit?)))

(check-true ($ "全部都是中文" :exists (@ _ :equals (rich-char :from-string "#\\中"))))

(let1 str (rich-string "Hello, World!")
  (check-true (str :contains #\W))
  (check-true (str :contains "Hello"))
  (check-true (str :contains ""))
  (check-true (str :contains (rich-char #\W)))
  (check-true (str :contains ($ "")))
  (check-true (str :contains ($ "Hello"))))

(let1 str (rich-string "你好世界")
  (check-true (str :contains "好世"))
  (check-true (str :contains "你"))
  (check-true (str :contains ($ "你" 0))))

(let1 str (rich-string "你好，世界！")
  (check (str :index-of ($ "你")) => 0)
  (check (str :index-of ($ "好")) => 1)
  (check (str :index-of ($ "世")) => 3)
  (check (str :index-of ($ "界")) => 4)
  (check (str :index-of ($ "！")) => 5)
  (check (str :index-of ($ "中" 0)) => -1)
  (check (str :index-of (rich-string "你好")) => 0)
  (check (str :index-of (rich-string "世界")) => 3)
  (check (str :index-of (rich-string "你好，世界")) => 0)
  (check (str :index-of (rich-string "世界！")) => 3)
  (check (str :index-of (rich-string "你好，世界！")) => 0)
  (check (str :index-of (rich-string "中国")) => -1)
  (check (str :index-of ($ "你") 1) => -1)
  (check (str :index-of (rich-string "世界") 4) => -1))

(let1 str (rich-string "Hello😀World")
  (check (str :index-of ($ "😀")) => 5)
  (check (str :index-of (rich-string "😀")) => 5)
  (check (str :index-of (rich-string "Hello😀")) => 0)
  (check (str :index-of (rich-string "😀World")) => 5)
  (check (str :index-of ($ "😀") 6) => -1)
  (check (str :index-of (rich-string "😀World") 6) => -1))

(check ($ "Hello" :index-of #\e) => 1)
(check ($ "Hello" :index-of #\e 5) => -1)
(check ($ "Hello" :index-of #\e -1) => 1)

(let1 s ($ "abc" :map (lambda (c) (c :to-upper)))
  (check s => "ABC")
  (check (s :length) => 3))

(check ($ "abc中文" :map (lambda (c) (c :to-upper))) => "ABC中文")

(check ($ "Hello123" :filter (@ _ :ascii?)) => "Hello123")
(check ($ "123abc" :filter (@ _ :digit?)) => "123")
(check ($ "ABCabc" :filter (@ _ :upper?)) => "ABC")
(check ($ "你好世界hello" :filter (@ _ :equals ($ "你" 0))) => ($ "你"))

(check ($ "Hello123" :filter (@ _ :ascii?) :reverse) => "321olleH")
(check ($ "123abc" :filter (@ _ :digit?) :reverse) => "321")
(check ($ "ABCabc" :filter (@ _ :upper?) :reverse) => "CBA")
(check ($ "你好世界" :drop-while (@ _ :equals ($ "你" 0)) :reverse) => "界世好")

(check ($ "" :count (@ class=? _ #\A)) => 0)
(check ($ "hello" :count (@ class=? _ #\l)) => 2)
(check ($ "你好，我是韩梅梅" :count (@ class=? _ (rich-char :from-string "#\\梅"))) => 2)

(check ($ "Hello" :index-where (@ _ :equals (rich-char #\e))) => 1)
(check ($ "" :index-where (@ _ :digit?)) => -1)
(check ($ "abc" :index-where (@ _ :digit?)) => -1)
(check ($ "中文" :index-where (@ _ :equals (rich-char #x4E2D))) => 0)
(check ($ "中文" :index-where (@ _ :equals (rich-char #x6587))) => 1)

(check ($ "Hello123" :take-while (@ _ :ascii?)) => "Hello123")
(check ($ "123abc" :take-while (@ _ :digit?)) => "123")
(check ($ "你好World" :take-while (@ _ :ascii?)) => "")
(check ($ "" :take-while (@ _ :ascii?)) => "")
(check ($ "ABC" :take-while (@ _ :upper?)) => "ABC")
(check ($ "123abc" :take-while (@ _ :digit?)) => ($ "123"))
(check ($ "你好世界hello" :take-while (@ _ :equals ($ "你" 0))) => ($ "你"))
(check ($ "aaaaa" :take-while (@ _ :equals (rich-char #\a)) :get) => "aaaaa")
(check ($ "" :take-while (@ _ :digit?)) => "")

(check ($ "   hello" :drop-while (@ _ :equals (rich-char #\space))) => "hello")
(check ($ "123abc" :drop-while (@ _ :digit?)) => "abc")
(check ($ "你好世界" :drop-while (@ _ :equals ($ "你" 0))) => "好世界")
(check ($ "" :drop-while (@ _ :equals (rich-char #\a))) => "")
(check ($ "aaaa" :drop-while (@ _ :equals (rich-char #\a))) => "")

(check ((rich-string "hello") :to-string) => "hello")

(let1 v ($ "中文" :to-vector)
  (check (v 0) => (rich-char :from-string "#\\中"))
  (check (v 1) => (rich-char :from-string "#\\文")))

(let1 v ($ "hello" :to-vector)
  (check (v 0) => (box #\h))
  (check (v 4) => (rich-char #\o)))

(let1 v ($ "中文的" :to-rich-vector)
  (check (v :length) => 3)
  (check (v 0) => (rich-char :from-string "#\\中"))
  (check (v 1) => (rich-char :from-string "#\\文"))
  (check (v 2) => (rich-char :from-string "#\\的")))

(check ($ "Hello" :+ " " :+ "World") => "Hello World")
(check ($ "hello " :+ (box "world")) => "hello world")
(check ($ "Hello " :+ 2025) => "Hello 2025")
(check ($ "Price is " :+ 1.2) => "Price is 1.2")

(check ($ " abc " :strip-left) => "abc ")
(check ($ "   abc" :strip-left) => "abc")
(check ($ "\t\n abc" :strip-left) => "abc")
(check ($ " \t \n abc \t \n " :strip-left) => "abc \t \n ")
(check ($ "" :strip-left) => "")
(check ($ "   " :strip-left) => "")

(check ($ " abc " :strip-right) => " abc")
(check ($ "abc   " :strip-right) => "abc")
(check ($ "abc \t\n" :strip-right) => "abc")
(check ($ " \t \n abc \t \n " :strip-right) => " \t \n abc")
(check ($ "" :strip-right) => "")
(check ($ "   " :strip-right) => "")

(check ($ " abc " :strip-both) => "abc")
(check ($ "   abc   " :strip-both) => "abc")
(check ($ "\t\n abc \t\n" :strip-both) => "abc")
(check ($ " \t \n abc \t \n " :strip-both) => "abc")
(check ($ "" :strip-both) => "")
(check ($ "   " :strip-both) => "")

(check ($ "" :strip-prefix "") => ($ ""))
(check ($ "hello" :strip-prefix "") => ($ "hello"))
(check ($ "hello" :strip-prefix "he") => ($ "llo"))
(check ($ "hello" :strip-prefix "hello") => ($ ""))
(check ($ "hello" :strip-prefix "abc") => ($ "hello"))
(check ($ "hello" :strip-prefix "helloo") => ($ "hello"))
(check ($ "hello" :strip-prefix "he" :strip-prefix "ll") => ($ "o"))
(check ($ "世界" :strip-prefix "世") => "界")

(check-catch 'wrong-number-of-args ("hello":strip-prefix "he"))
(check-catch 'unbound-variable (123:strip-prefix 1))

(check ($ "" :strip-suffix "") => ($ ""))
(check ($ "hello" :strip-suffix "") => ($ "hello"))
(check ($ "hello" :strip-suffix "lo") => ($ "hel"))
(check ($ "hello" :strip-suffix "hello") => ($ ""))
(check ($ "hello" :strip-suffix "abc") => ($ "hello"))
(check ($ "hello" :strip-suffix "hhello") => ($ "hello"))
(check ($ "hello" :strip-suffix "lo" :strip-suffix "el") => ($ "h"))
(check ($ "世界" :strip-suffix "界") => "世")

(check-catch 'wrong-number-of-args ("hello":strip-suffix "llo"))

(check ($ "hahaha" :replace-first "a" "oo") => ($ "hoohaha"))
(check ($ "hello" :replace-first "world" "") => ($ "hello"))
(check ($ "hello" :replace-first "l" "L" :strip-prefix "he") => ($ "Llo"))

(check ($ "韩梅梅" :replace-first "梅" "雪") => "韩雪梅")

(check ($ "hahaha" :replace "a" "oo") => ($ "hoohoohoo"))
(check ($ "hello" :replace "world" "") => ($ "hello"))
(check ($ "hello" :replace "l" "L" :strip-prefix "he") => ($ "LLo"))

(check ($ "韩梅梅" :replace "梅" "雪") => "韩雪雪")

(let ((s (rich-string "test")))
  (check (s :pad-left 5 #\# :pad-left 6) => (rich-string " #test")))

(let ((s (rich-string "325")))
  (check (s :pad-left 5) => (rich-string "  325")))

(let ((s (rich-string "8871325")))
  (check (s :pad-left 5) => (rich-string "71325")))

(check  ($ "abcdef" :slice 2 5 :pad-left 8 #\-)
        => (rich-string "-----cde"))

(let ((s (rich-string "12345")))
  (check (s :pad-left 5) => (rich-string "12345")))

(let ((s (rich-string "")))
  (check (s :pad-left 3 #\*) => (rich-string "***")))

(check-catch 'wrong-number-of-args ($ "test" :pad-left 10 #\- 0 3 5))

(let ((s (rich-string "test")))
  (check (s :pad-right 5 #\# :pad-right 6) => (rich-string "test# ")))

(let ((s (rich-string "abc")))
  (check (s :pad-right 5) => (rich-string "abc  ")))  

(let ((s (rich-string "123")))
  (check (s :pad-right 6 #\*) => (rich-string "123***")))  

(let ((s (rich-string "test")))
  (check (s :pad-right 4) => (rich-string "test")))  

(let ((s (rich-string "overlength")))
  (check (s :pad-right 5) => (rich-string "overl")))  

(check  ($ "abcdefgh" :slice 3 6 :pad-right 6 #\.)
        => (rich-string "def..."))

(let ((s (rich-string "")))
  (check (s :pad-right 3 #\#) => (rich-string "###")))

(check-catch 'wrong-number-of-args ($ "test" :pad-right 10 #\- 0 3 5))

(check ($ "da@liii.pro" :split "@") => #("da" "liii.pro"))
(check ($ "da@liii.pro" :split ".") => #("da@liii" "pro"))
(check ($ "test" :split "") => #("t" "e" "s" "t"))
(check ($ "aXXbXXcXX" :split "XX") => #("a" "b" "c" ""))
(check ($ "a||b||c" :split "||") => #("a" "b" "c"))
(check ($ "XXaXXb" :split "XX") => #("" "a" "b"))
(check ($ "你好，欢迎使用Liii STEM" :split "，") => #("你好" "欢迎使用Liii STEM"))
(check ($ "中国智造，惠及全球" :split "") => #("中" "国" "智" "造" "，" "惠" "及" "全" "球"))

(check (($ "qingyu@liii.pro" :split "@") :head) => "qingyu")
(check (($ "127.0.0.1" :split ".") :count) => 4)
(check-catch 'wrong-number-of-args ($ "127.0.0.1" :split "." :count))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check (opt1 :get) => 42)
  (check-catch 'value-error (opt2 :get)))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check (opt1 :get-or-else 0) => 42)
  (check (opt2 :get-or-else 0) => 0)

  (check (opt1 :get-or-else (lambda () 0)) => 42)
  (check (opt2 :get-or-else (lambda () 0)) => 0)
)

(check ((none) :get-or-else ($ 1)) => ($ 1))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check (opt1 :or-else (option 0)) => (option 42))
  (check (opt2 :or-else (option 0)) => (option 0))
  (check (opt2 :or-else (option 0) :or-else (option 1)) => (option 0))
  (check-catch 'type-error (opt1 :or-else 0))
)

(check-true ((option "str") :equals (option "str")))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check-true (opt1 :defined?))
  (check-false (opt2 :defined?)))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check-true (opt1 :forall (lambda (x) (== x 42))))
  (check-false (opt2 :forall (lambda (x) (== x 42)))))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check-true (opt1 :exists (lambda (x) (== x 42))))
  (check-false (opt2 :exists (lambda (x) (== x 42)))))

(check ((option "hello") :contains string?) => #t)
(check ((option 42) :contains integer?) => #t)
(check ((option #t) :contains boolean?) => #t)
(check ((option '()) :contains null?) => #f)
(check ((none) :contains string?) => #f)
(check ((option "hello") :contains number?) => #f)

(let ((opt1 (option 42))
      (opt2 (option '())))
  (check (opt1 :map (lambda (x) (+ x 1))
               :map (lambda (x) (* x 2))
               :get) => 86)
  (check (opt2 :map (lambda (x) (+ x 1))
               :map (lambda (x) (* x 2))
               :empty?) => #t))

(let ((opt1 (option 42))
      (opt2 (option '())))
  (check (opt1 :flat-map (lambda (x) (option (+ x 1)))
               :flat-map (lambda (x) (option (* x 2)))
               :get) => 86)
  (check (opt2 :flat-map (lambda (x) (option (+ x 1)))
               :flat-map (lambda (x) (option (* x 2)))
               :empty?) => #t))

(let ((opt1 (option 42))
      (opt2 (option '())))
  (check (opt1 :filter (lambda (x) (> x 40))
               :filter (lambda (x) (< x 50))
               :get) => 42)
  (check (opt1 :filter (lambda (x) (> x 50))
               :filter (lambda (x) (< x 60))
               :empty?) => #t)
  (check (opt2 :filter (lambda (x) (> x 40))
               :filter (lambda (x) (< x 50))
               :empty?) => #t))

(check-true ((left "Value error") :left?))
(check-false ((right 1) :left?))

(check-false ((left "Value error") :right?))
(check-true ((right 1) :right?))

(check ((right 1) :get) => 1)
(check ((left "error") :get) => "error")

(check ((right 1) :or-else (right 2)) => (right 1))
(check ((left "error") :get-or-else (right 2)) => (right 2))

(check ((right 1) :get-or-else 2) => 1)
(check ((left "error") :get-or-else 2) => 2)
(check ((left "error") :get-or-else ($ 2)) => ($ 2))

(let1 r ((right 12) :filter-or-else (lambda (x) (> x 10)) -1)
  (check-true (r :right?))
  (check (r :get) => 12))

(let1 r ((right 7) :filter-or-else (lambda (x) (> x 10)) -1)
  (check-true (r :left?))
  (check (r :get) => -1))

(let1 r ((left 7) :filter-or-else (lambda (x) #f) -1)
  (check-true (r :left?))
  (check (r :get) => 7))

(check-true ((right 1) :contains 1))
(check-false ((left "error") :contains 1))
(check-true ((right (box 1)) :contains 1))

;; 测试 1: right 值转换为 option
(let ((e1 (right 42)))
  (check ((e1 :to-option) :get) => 42)
  (check-true ((e1 :to-option) :defined?))
  (check-false ((e1 :to-option) :empty?)))

;; 测试 2: left 值转换为 option (应该返回 none)
(let ((e2 (left "error")))
  (check ((e2 :to-option) :empty?) => #t)
  (check-false ((e2 :to-option) :defined?))
  (check-catch 'value-error ((e2 :to-option) :get)))

(let1 r ((right 1) :map (lambda (x) (+ x 1)))
  (check-true (r :right?))
  (check (r :get-or-else 0) => 2))

(let1 r ((right 1) :flat-map (lambda (x) (right (+ x 1))))
  (check-true (r :right?))
  (check (r :get-or-else 0) => 2))

(check-true ((left "error") :forall even?))
(check-true ((right 42) :forall even?))
(check-false ((right 43) :forall even?))
(check-false ((right "not-a-number") :forall number?))

(check-false ((left "error") :exists even?))
(check-true ((right 42) :exists even?))
(check-false ((right 43) :exists even?))
(check-false ((right "not-a-number") :exists number?))

(check (rich-list :range 1 5) => ($ (list 1 2 3 4)))
(check (rich-list :range 1 5 2) => ($ (list 1 3)))
(check (rich-list :range 1 6 2) => ($ (list 1 3 5)))
(check (rich-list :range 5 1 -1) => ($ (list 5 4 3 2)))
(check (rich-list :range 1 5 2 :collect) => (list 1 3))
(check (rich-list :range 1 5 :map (lambda (x) (* x 2))) => ($ (list 2 4 6 8)))
(check (rich-list :range 1 10 1 :map (lambda (x) (+ x 1))) => ($ (list 2 3 4 5 6 7 8 9 10)))
(check (rich-list :range 5 1 1) => ($ (list )))

(check-catch 'value-error (rich-list :range 1 5 0))

(check (rich-list :empty :empty?) => #t)
(check (rich-list :empty :head-option) => (none))


(check (rich-list :concat ($ (list 1)) ($ (list 2))) => ($ (list 1 2)))
(check (rich-list :concat ($ (list 1 2)) ($ (list 3 4))) => ($ (list 1 2 3 4)))
(check (rich-list :concat (rich-list :range 1 4) ($ (list 3 4))) => ($ (list 1 2 3 3 4)))
(check (rich-list :concat ($ (list 1)) ($ (list 2))
           :collect) => (list 1 2))
(check (rich-list :concat (rich-list '(1)) (rich-list '(2)) :count) => 2)

(let1 result (rich-list :fill 3 "a")
  (check (result :collect) => '("a" "a" "a")))

(let1 result (rich-list :fill 0 "a")
  (check (result :collect) => '()))

(check-catch 'value-error (rich-list :fill -1 "a"))

(let1 result (rich-list :fill 2 42)
  (check (result :collect) => '(42 42)))

(let1 result (rich-list :fill 1000 "x")
  (check (length (result :collect)) => 1000))

(check ($ '(1 2 3) :apply 0) => 1)
(check ($ '(1 2 3) 0) => 1)

(let1 lst (rich-list '(1 2 3 4 5))
  (check ((lst :find (lambda (x) (= x 3))) :get) => 3)
  (check ((lst :find (lambda (x) (> x 2))) :get) => 3)
  (check ((lst :find (lambda (x) (> x 10))) :empty?) => #t)
  (check ((lst :find even?) :get) => 2)
  (check ((lst :find (lambda (x) (< x 0))) :empty?) => #t))

(let1 lst (rich-list '(1 2 3 4 5))
  (check ((lst :find-last even?) :get) => 4)  ; 最后一个偶数是4
  (check ((lst :find-last (@ > _ 3)) :get) => 5)  ; 最后一个大于3的元素是5
  (check ((lst :find-last (@ > _ 5)) :empty?) => #t)  ; 没有大于5的元素
  (check ((lst :find-last zero?) :empty?) => #t)  ; 没有0
  (check ((rich-list '()) :find-last even?) => (none)))  ; 空列表返回none

(check ($ (list 1 2 3) :head) => 1)
(check-catch 'out-of-range (rich-list :empty :head))
(check ($ (list 1 2 3) :head-option) => (option 1))
(check (rich-list :empty :head-option) => (none))

(check ($ (list 1 2 3) :last) => 3)
(check-catch 'index-error (rich-list :empty :last))
(check ($ (list 1 2 3) :last-option) => (option 3))
(check (rich-list :empty :last-option) => (none))

(let ((lst ($ '(1 2 3 4 5))))
  ;; 基本切片
  (check (lst :slice 1 3 :collect) => '(2 3))

  ;; from超出范围
  (check (lst :slice 10 3 :collect) => '())

  ;; until超出范围
  (check (lst :slice 2 10 :collect) => '(3 4 5))

  ;; from > until
  (check (lst :slice 3 1 :collect) => '())

  ;; 负数索引
  (check (lst :slice -1 3 :collect) => '(1 2 3))

  ;; 链式调用
  (check (lst :slice 1 4 :map (@ * _ 2) :collect) => '(4 6 8))

  ;; 空切片
  (check (lst :slice 2 2 :collect) => '())
)

(check-true ($ (list) :empty?))
(check-false ($ '(1 2 3) :empty?))

(check ($ (list ($ 1) ($ 2) ($ 3))) => (($ 1 :to 3) :map $))

(let1 lst ($ '(1 2 3 4 5))
  (check (lst :forall (@ > _ 0)) => #t)
  (check (lst :forall (@ > _ 3)) => #f)
)

(check (rich-list :empty :forall (@ > _ 0)) => #t)

(let1 l (rich-list '(1 2 3))
  (check-true (l :exists even?)))

(let1 l (rich-list '(1 2 3))
  (check-true (l :contains 1))
  (check-false (l :contains 4)))

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :reverse :collect) => '(5 4 3 2 1)))

(let ((lst (rich-list '(a b c d e))))
  (check (lst :reverse :collect) => '(e d c b a)))

(let ((lst (rich-list '())))
  (check (lst :reverse :collect) => '()))

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :take -1 :collect) => '())
  (check (lst :take 0 :collect) => '())
  (check (lst :take 3 :collect) => '(1 2 3))
  (check (lst :take 5 :collect) => '(1 2 3 4 5))
  (check (lst :take 10 :collect) => '(1 2 3 4 5))
)

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :drop -1 :collect) => '(1 2 3 4 5))
  (check (lst :drop 0 :collect) => '(1 2 3 4 5))
  (check (lst :drop 3 :collect) => '(4 5))
  (check (lst :drop 5 :collect) => '())
  (check (lst :drop 10 :collect) => '())
)

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :take-right -1 :collect) => '())
  (check (lst :take-right 0 :collect) => '())
  (check (lst :take-right 3 :collect) => '(3 4 5))
  (check (lst :take-right 5 :collect) => '(1 2 3 4 5))
  (check (lst :take-right 10 :collect) => '(1 2 3 4 5))
)

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :drop-right -1 :collect) => '(1 2 3 4 5))
  (check (lst :drop-right 0 :collect) => '(1 2 3 4 5))
  (check (lst :drop-right 3 :collect) => '(1 2))
  (check (lst :drop-right 5 :collect) => '())
  (check (lst :drop-right 10 :collect) => '())
)

(check ((rich-list (list 1 2 3)) :count) => 3)
(check ((rich-list (list 1 2 3)) :count (cut > <> 1)) => 2)

(check ($ '() :length) => 0)
(check ($ '(1) :length) => 1)
(check ($ '(1 2) :length) => 2)
(check ($ '(1 2 3) :length) => 3)
(check ($ '(1 2 3 4 5) :length) => 5)
(check ($ '(1 2 3 4 5 6 7 8 9 10) :length) => 10)

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :fold 0 +) => 15)
  (check (lst :fold '() (lambda (x acc) (cons x acc))) => '(5 4 3 2 1))

  (check (lst :fold-right 0 +) => 15)
  (check (lst :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3 4 5))
)

(check ($ '(3 1 2 4 5)
        :sort-with (lambda (x y) (< x y)))
    => ($ '(1 2 3 4 5)))

(check ($ (list 1 3 4 2 5) :sort-with < :take 2) => (list 1 2))

(check 
  ($ (list 1 3 4 2 5) 
     :sort-with <
     :take 2
     :collect)
  => '(1 2))

(check 
  ($ '((3 . a) (1 . b) (2 . c) (1 . d))
     :sort-with (lambda (x y) (< (car x) (car y)))  ;; 按 car 排序
     :collect)
  => '((1 . b) (1 . d) (2 . c) (3 . a)))

;; 测试按绝对值排序
(check ($ '(-3 1 -2 4 0) :sort-by abs :collect) => '(0 1 -2 -3 4))
  
;; 测试按结构体字段排序
(let ((people ($ '((name . "Alice") (name . "Bob") (name . "Charlie")))))
  (check (people :sort-by (lambda (p) (string-length (cdr p))) :collect)
         => '((name . "Bob") (name . "Alice") (name . "Charlie"))))
  
;; 测试空列表
(check ($ '() :sort-by identity :collect) => '())
  
;; 测试链式调用
(check ($ '(-3 1 -2 4 0) 
         :sort-by abs 
         :filter positive? 
         :collect)
       => '(1 4))

(check  (($ '(1 2 3 4 5 6) :group-by (@ modulo _ 2)) :collect)
        =>  (hash-table 0 '(2 4 6) 1 '(1 3 5)))

(check  (($ '(1 2 3 4 5 6) :group-by (@ modulo _ 3)) :collect)
        =>  (hash-table 0 '(3 6) 1 '(1 4) 2 '(2 5)))

(check  (($ '(1 2 3 4 5 6 7) :group-by (@ modulo _ 3)) :collect)
        =>  (hash-table 0 '(3 6) 1 '(1 4 7) 2 '(2 5)))

(let ((result ($ '("apple" "banana" "cat" "dog") :group-by (@ string-length _))))
  (check (result :collect) 
          => (hash-table 3 '("cat" "dog") 5 '("apple") 6 '("banana"))))

;; Single-argument sliding for rich-list
(check ($ '() :sliding 2) => #())
(check ($ '(1) :sliding 2) => #((1)))
(check ($ '(1 2) :sliding 2) => #((1 2)))
(check ($ '(1 2 3) :sliding 2) => #((1 2) (2 3)))
(check ($ '(1 2 3 4 5) :sliding 3) => #((1 2 3) (2 3 4) (3 4 5)))
(check ($ '(1 2 3 4 5) :sliding 1) => #((1) (2) (3) (4) (5)))
(check ($ '(1 2 3) :sliding 3) => #((1 2 3)))
(check ($ '(1 2 3) :sliding 4) => #((1 2 3)))

;; Error cases for size (single-arg) for rich-list
(check-catch 'value-error ($ '(1 2 3) :sliding 0))
(check-catch 'value-error ($ '(1 2 3) :sliding -1))
(check-catch 'type-error ($ '(1 2 3) :sliding 1.5))

;; Two-argument sliding for rich-list
(check ($ '() :sliding 2 2) => #())
(check ($ '(1 2 3 4 5) :sliding 2 2) => #((1 2) (3 4) (5)))
(check ($ '(1 2 3 4 5 6) :sliding 2 3) => #((1 2) (4 5)))
(check ($ '(1 2 3 4 5) :sliding 3 1) => #((1 2 3) (2 3 4) (3 4 5) (4 5) (5)))
(check ($ '(1 2 3 4) :sliding 2 2) => #((1 2) (3 4)))
(check ($ '(1 2) :sliding 3 1) => #((1 2) (2)))
(check ($ '(1 2 3 4 5) :sliding 3 2) => #((1 2 3) (3 4 5) (5)))
(check ($ '(1 2 3 4 5 6 7) :sliding 3 3) => #((1 2 3) (4 5 6) (7)))
(check ($ '(1 2 3 4 5) :sliding 5 1) => #((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)))
(check ($ '(1 2 3 4 5) :sliding 6 1) => #((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)))


;; Error cases for step (two-arg) for rich-list
(check-catch 'value-error ($ '(1 2 3) :sliding 2 0))
(check-catch 'value-error ($ '(1 2 3) :sliding 2 -1))
(check-catch 'type-error ($ '(1 2 3) :sliding 2 1.5))

(check (($ '(1 2 3)) :zip '(a b c) :collect) => '((1 . a) (2 . b) (3 . c)))
(check (($ '(1 2 3)) :zip '(a b) :collect) => '((1 . a) (2 . b)))

(check  ($ '(a b c) :zip-with-index :collect)  
        => '((0 . a) (1 . b) (2 . c)))

(check  ($ '() :zip-with-index :collect) 
        => '())

(check  ($ '(1 1 2 2 2 3 4 5 6 7) :zip-with-index :collect)
        => '((0 . 1) (1 . 1) (2 . 2) (3 . 2) (4 . 2) (5 . 3) (6 . 4) (7 . 5) (8 . 6) (9 . 7)))

(check  ($ '(a a b c b) :distinct :collect) 
        => '(a b c))

(check  ($ '(1 1 1 2 2 3 3 3 3 5 5 5) :distinct :collect) 
        => '(1 2 3 5))

(check  ($ '() :distinct :collect) 
        => '())

(check-catch 'value-error ($ '() :reduce +))

(check ($ '(1 2 3) :reduce +) => 6)  
(check ($ '(2 3 4) :reduce *) => 24)  
(check ($ '(5) :reduce (lambda (x y) (+ x y 10))) => 5)

(check ($ '() :reduce-option +) => (none))

(check ($ '(1 2 3) :reduce-option +) => (option 6))  
(check ($ '(2 3 4) :reduce-option *) => (option 24))  
(check ($ '(5) :reduce-option (lambda (x y) (+ x y 10))) => (option 5))

(check ($ '(1 2 3 4 5 6 7) :take-while (@ < _ 5) :collect) => '(1 2 3 4))
(check ($ '() :take-while (@ < _ 5) :collect) => '())
(check ($ '(1 2 3) :take-while number? :collect) => '(1 2 3))
(check ($ '(5 1 2 3) :take-while (@ < _ 3) :collect) => '())

(check ($ '(1 2 3 4 5 6 7) :drop-while (@ < _ 5) :collect) => '(5 6 7))
(check ($ '() :drop-while (@ < _ 5) :collect) => '())
(check ($ '(1 2 3) :drop-while number? :collect) => '())
(check ($ '(5 1 2 3) :drop-while (@ < _ 3) :collect) => '(5 1 2 3))

(let ((xs ($ '(1 2 3 4 5))))
  (check (xs :index-where even?) => 1)
  (check (xs :index-where (@ > _ 3)) => 3)
  (check (xs :index-where (@ > _ 5)) => #f)
)

(check ($ '(1 2 3) :max-by identity) => 3)
(check ($ '((1) (3) (2)) :max-by car) => '(3))
(check-catch 'value-error ($ '() :max-by identity))
(check-catch 'type-error ($ '(1 2 3) :max-by "not-function"))
(check-catch 'type-error ($ '("a" "b" "c") :max-by identity))

(check ($ '(1 2 3) :min-by identity) => 1)
(check ($ '((1) (3) (2)) :min-by car) => '(1))
(check-catch 'value-error ($ '() :min-by identity))
(check-catch 'type-error ($ '(1 2 3) :min-by "not-function"))
(check-catch 'type-error ($ '("a" "b" "c") :min-by identity))

(check (rich-list :empty :append (list 1 2)) => ($ (list 1 2)))
(check ($ (list 1 2) :append (list )) => ($ (list 1 2)))
(check ($ (list 1 2) :append (list 3 4)) => ($ (list 1 2 3 4)))

(check ($ '() :max-by-option identity) => (none))

(check ($ '() :min-by-option identity) => (none))

(check (object->string ($ '(1 2 3))) => "(1 2 3)")

(let1 l (rich-list (list 1 2 3))
  (check (l :make-string) => "123")
  (check (l :make-string " ") => "1 2 3")
  (check (l :make-string "[" "," "]") => "[1,2,3]")
  
  (check-catch 'wrong-number-of-args (l :make-string "[" ","))
  (check-catch 'type-error (l :make-string 123 "," "]"))
  (check-catch 'type-error (l :make-string "[" 123 "]"))
  (check-catch 'type-error (l :make-string "[" "," 123))
)

(check ($ (list "a" "b") :make-string) => "ab")
(check ($ (list "a" "b") :make-string " ") => "a b")

(let ((lst (rich-list '(1 2 3))))
  (check (lst :to-vector) =>  #(1 2 3)))

(let ((lst (rich-list '(1 2 3))))
  (check (lst :to-rich-vector) => (rich-vector #(1 2 3)))
  (check ((lst :to-rich-vector) :collect) => #(1 2 3)))

(check-true (rich-vector :is-type-of (rich-vector :empty)))
(check-true (rich-vector :is-type-of (rich-vector #(1 2 3))))

(check-false (rich-vector :is-type-of #(1 2 3)))
(check-false (rich-vector :is-type-of 1))

(check (array :range 1 5) => ($ (vector 1 2 3 4)))
(check (array :range 1 5 2) => ($ (vector 1 3)))
(check (array :range 1 6 2) => ($ (vector 1 3 5)))
(check (array :range 5 1 -1) => ($ (vector 5 4 3 2)))

(check (array :range 5 1 1) => ($ (vector )))

(check-catch 'value-error (array :range 1 5 0))

(check (array :empty :empty?) => #t)
(check (array :empty :head-option) => (none))

(check-true (array :fill 0 #\a :empty?))

(check (array :fill 3 #\a) => ($ (vector #\a #\a #\a)))

(check ($ #() :length) => 0)
(check ($ #(1 2 3) :length) => 3)

(check ($ #() :size) => 0)
(check ($ #(1 2 3) :size) => 3)

(check ($ #(1 2 3) :apply 1) => 2)
(check ($ #(1 2 3) 1) => 2)

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :index-of 1) => 0)
  (check (vec :index-of 5) => 4)
  (check (vec :index-of 6) => -1))

(let ((vec (array #(1 1 1 5 5))))
  (check (vec :last-index-of 1) => 2)
  (check (vec :last-index-of 5) => 4)
  (check (vec :last-index-of 6) => -1))

(let ((vec (array #(1 2 3 4 5))))
  (check ((vec :find (lambda (x) (= x 3))) :get) => 3)
  (check ((vec :find (lambda (x) (> x 2))) :get) => 3)
  (check ((vec :find (lambda (x) (> x 10))) :empty?) => #t)
  (check ((vec :find even?) :get) => 2)
  (check ((vec :find (lambda (x) (< x 0))) :empty?) => #t))

(let ((vec (array #(1 2 3 4 5))))
  (check ((vec :find-last even?) :get) => 4)  ; 最后一个偶数是4
  (check ((vec :find-last (@ > _ 3)) :get) => 5)  ; 最后一个大于3的元素是5
  (check ((vec :find-last (@ > _ 5)) :empty?) => #t)  ; 没有大于5的元素
  (check ((vec :find-last zero?) :empty?) => #t)  ; 没有0
  (check ((array :empty) :find-last even?) => (none)))  ; 空向量返回none

(check ($ (vector 1 2 3) :head) => 1)
(check-catch 'out-of-range (array :empty :head))
(check ($ (vector 1 2 3) :head-option) => (option 1))
(check (array :empty :head-option) => (none))

(check ($ (vector 1 2 3) :last) => 3)
(check-catch 'index-error (array :empty :last))
(check ($ (vector 1 2 3) :last-option) => (option 3))
(check (array :empty :last-option) => (none))

(let1 vec (array #(1 2 3 4 5))
  (check (vec :slice 0 2) => ($ #(1 2)))
  (check (vec :slice -1 2) => ($ #(1 2)))
  (check (vec :slice 2 -1) => ($ #()))
  (check (vec :slice 2 2) => ($ #()))
  (check (vec :slice 6 2) => ($ #()))
  (check (vec :slice -1 10) => ($ #(1 2 3 4 5)))
  (check (vec :slice 4 10) => ($ #(5)))
  (check (vec :slice 2 4) => ($ #(3 4))))

(check-true ($ (vector) :empty?))
(check-false ($ #(1 2 3) :empty?))

(check-true ($ #(1 2 3) :equals ($ #(1 2 3))))

(check ($ (vector ($ "中" 0) ($ "文" 0))) => ($ "中文" :to-vector))

(check-false (($ "中文" :to-rich-vector) :equals ($ "中" 0)))

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :forall (lambda (x) (> x 0))) => #t)
  (check (vec :forall (lambda (x) (> x 3))) => #f))

(let ((empty-vec (array #())))
  (check (empty-vec :forall (lambda (x) (> x 0))) => #t))

(let1 vec (rich-vector #(1 2 3))
  (check-true (vec :contains 1))
  (check-false (vec :contains 4)))

(let1 vec (rich-vector #("/" "tmp" "/"))
  (check-true (vec :contains "tmp"))
  (check-true (vec :contains "/"))
  (check-false (vec :contains "tmpxx")))

(let1 vec (array #(1 2 3 4 5))
  (check (vec :map (lambda (x) (vector x x))) => #(#(1 1) #(2 2) #(3 3) #(4 4) #(5 5))))

;; 测试符号
(let1 vec (array #("a" ";" "." "?" "["))
  (check (vec :map (lambda (x) (vector x x))) => #(#("a" "a") #(";" ";") #("." ".") #("?" "?") #("[" "["))))

;; 混合测试
(let1 vec (array #("a" ";" "?" "[" -1 5))
  (check (vec :map (lambda (x) (vector x x))) => #(#("a" "a") #(";" ";") #("?" "?") #("[" "[") #(-1 -1) #(5 5))))

(let1 vec (array #(1 2 3 4 5))
  (check (vec :flat-map (lambda (x) (vector x x))) => #(1 1 2 2 3 3 4 4 5 5)))

(let ((vec (rich-vector #(1 2 3 4 5))))
  (check (vec :reverse :collect) => #(5 4 3 2 1)))

(let ((vec (rich-vector #(a b c d e))))
  (check (vec :reverse :collect) => #(e d c b a)))

(let ((vec (rich-vector #())))
  (check (vec :reverse :collect) => #()))

(let ((vec (rich-vector #("/" "tmp" "/" "tmp2"))))
  (check (vec :reverse :collect) => #("tmp2" "/" "tmp" "/")))

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :take -1 :collect) => #())
  (check (vec :take 0 :collect) => #())
  (check (vec :take 3 :collect) => #(1 2 3))
  (check (vec :take 5 :collect) => #(1 2 3 4 5))
  (check (vec :take 10 :collect) => #(1 2 3 4 5))
)

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :take-right -1 :collect) => #())
  (check (vec :take-right 0 :collect) => #())
  (check (vec :take-right 3 :collect) => #(3 4 5))
  (check (vec :take-right 5 :collect) => #(1 2 3 4 5))
  (check (vec :take-right 10 :collect) => #(1 2 3 4 5))
)

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :drop -1 :collect) => #(1 2 3 4 5))
  (check (vec :drop 0 :collect) => #(1 2 3 4 5))
  (check (vec :drop 3 :collect) => #(4 5))
  (check (vec :drop 5 :collect) => #())
  (check (vec :drop 10 :collect) => #())
)

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :drop-right -1 :collect) => #(1 2 3 4 5)) 
  (check (vec :drop-right 0 :collect) => #(1 2 3 4 5)) 
  (check (vec :drop-right 3 :collect) => #(1 2)) 
  (check (vec :drop-right 5 :collect) => #()) 
  (check (vec :drop-right 10 :collect) => #()) 
)

(let ((vec (array #(1 2 3 4 5))) (empty-vec ($ #())))
  (check (vec :drop-while (@ < _ 3) :collect) => #(3 4 5))
  (check (vec :drop-while (@ > _ 3) :collect) => #(1 2 3 4 5))
  (check (vec :drop-while (@ < _ 3) :drop 1 :collect) => #(4 5))
  (check (empty-vec :drop-while (@ < _ 3) :drop 1 :collect) => #())
  (check (vec :drop-while (@ < _ 100) :collect) => #())
)

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :fold 0 +) => 15)
  (check (vec :fold '() (lambda (x acc) (cons x acc))) => '(5 4 3 2 1))

  (check (vec :fold-right 0 +) => 15)
  (check (vec :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3 4 5))
)

(check ($ #() :count) => 0)
(check ($ #() :count (@ > _ 2)) => 0)
(check ($ #(1 2 3 4 5) :count) => 5)
(check ($ #(1 2 3 4 5) :count (@ > _ 2)) => 3)

(let ((vec (rich-vector #(3 1 4 2 5))))
  (check (vec :sort-with <) => (array #(1 2 3 4 5)))
  (check (vec :sort-with >) => (array #(5 4 3 2 1)))
  (check (vec :sort-with < :collect) => #(1 2 3 4 5)))

(let ((vec (rich-vector #((2 . 1) (3 . 3) (1 . 3) (1 . 2) (3 . 2)))))
  (check (vec :sort-with (lambda (x y) (< (car x) (car y))))
         => (rich-vector #((1 . 3) (1 . 2) (2 . 1) (3 . 3) (3 . 2))))
  (check (vec :sort-with (lambda (x y) (< (cdr x) (cdr y))))
         => (rich-vector #((2 . 1) (1 . 2) (3 . 2) (3 . 3) (1 . 3)))))

;; 测试按绝对值排序
(check ($ #(-3 1 -2 4 0) :sort-by abs :collect) => #(0 1 -2 -3 4))
  
;; 测试按结构体字段排序
(let ((people ($ #((name . "Alice") (name . "Bob") (name . "Charlie")))))
  (check (people :sort-by (lambda (p) (string-length (cdr p))) :collect)
         => #((name . "Bob") (name . "Alice") (name . "Charlie"))))
  
;; 测试空向量
(check ($ #() :sort-by identity :collect) => #())
  
;; 测试链式调用
(check ($ #(-3 1 -2 4 0) 
         :sort-by abs 
         :filter positive? 
         :collect)
       => #(1 4))

(check  (($ #(1 2 3 4 5 6) :group-by (@ modulo _ 2)) :collect)
        =>  (hash-table 0 #(2 4 6) 1 #(1 3 5)))

(check  (($ #(1 2 3 4 5 6) :group-by (@ modulo _ 3)) :collect)
        =>  (hash-table 0 #(3 6) 1 #(1 4) 2 #(2 5)))

(check  (($ #(1 2 3 4 5 6 7) :group-by (@ modulo _ 3)) :collect)
        =>  (hash-table 0 #(3 6) 1 #(1 4 7) 2 #(2 5)))

(let ((result ($ #("apple" "banana" "cat" "dog") :group-by (@ string-length _))))
  (check (result :collect) 
          => (hash-table 3 #("cat" "dog") 5 #("apple") 6 #("banana"))))

(check ($ #() :sliding 2) => #())
(check ($ #(1) :sliding 2) => #(#(1)))
(check ($ #(1 2) :sliding 2) => #(#(1 2)))
(check ($ #(1 2 3) :sliding 2) => #(#(1 2) #(2 3)))
(check ($ #(1 2 3 4 5) :sliding 3) => #(#(1 2 3) #(2 3 4) #(3 4 5)))
(check ($ #(1 2 3 4 5) :sliding 1) => #(#(1) #(2) #(3) #(4) #(5)))
(check ($ #(1 2 3) :sliding 3) => #(#(1 2 3)))
(check ($ #(1 2 3) :sliding 4) => #(#(1 2 3)))

;; Error cases for size
(check-catch 'value-error ($ #(1 2 3) :sliding 0))
(check-catch 'value-error ($ #(1 2 3) :sliding -1))
(check-catch 'type-error ($ #(1 2 3) :sliding 1.5))
(check-catch 'type-error ($ #(1 2 3) :sliding "a"))

;; Two-argument sliding
(check ($ #() :sliding 2 2) => #())
(check ($ #(1 2 3 4 5) :sliding 2 2) => #(#(1 2) #(3 4) #(5)))
(check ($ #(1 2 3 4 5 6) :sliding 2 3) => #(#(1 2) #(4 5)))
(check ($ #(1 2 3 4 5) :sliding 3 1) => #(#(1 2 3) #(2 3 4) #(3 4 5) #(4 5) #(5)))
(check ($ #(1 2 3 4) :sliding 2 2) => #(#(1 2) #(3 4)))
(check ($ #(1 2) :sliding 3 1) => #(#(1 2) #(2)))
(check ($ #(1 2 3 4 5) :sliding 3 2) => #(#(1 2 3) #(3 4 5) #(5)))
(check ($ #(1 2 3 4 5 6 7) :sliding 3 3) => #(#(1 2 3) #(4 5 6) #(7)))
(check ($ #(1 2 3 4 5) :sliding 5 1) => #(#(1 2 3 4 5) #(2 3 4 5) #(3 4 5) #(4 5) #(5)))
(check ($ #(1 2 3 4 5) :sliding 6 1) => #(#(1 2 3 4 5) #(2 3 4 5) #(3 4 5) #(4 5) #(5)))

;; Error cases for step (two-arg)
(check-catch 'value-error ($ #(1 2 3) :sliding 2 0))
(check-catch 'value-error ($ #(1 2 3) :sliding 2 -1))
(check-catch 'type-error ($ #(1 2 3) :sliding 2 1.5))
(check-catch 'type-error ($ #(1 2 3) :sliding 2 "a"))

(check  ($ #(a b c) :zip-with-index :collect)  
        => #((0 . a) (1 . b) (2 . c)))

(check  ($ #() :zip-with-index :collect) 
        => #())

(check  ($ #(1 1 2 2 2 3 4 5 6 7) :zip-with-index :collect)
        => #((0 . 1) (1 . 1) (2 . 2) (3 . 2) (4 . 2) (5 . 3) (6 . 4) (7 . 5) (8 . 6) (9 . 7)))

(check  ($ #(a a b c b) :distinct :collect) 
        => #(a b c))

(check  ($ #(1 1 1 2 2 3 3 3 3 5 5 5) :distinct :collect) 
        => #(1 2 3 5))

(check  ($ #() :distinct :collect) 
        => #())

(check ($ #(1 2 3 4) :reduce +) => 10)  ; 1 + 2 + 3 + 4 = 10
(check ($ #(5) :reduce *) => 5)         ; 单个元素直接返回
(check-catch 'value-error ($ #() :reduce +)) ; 空向量应该报错
(check ($ #(#(1 1) #(2 2) #(3 3) #(4 4) #(5 5))
       :map vector-length
       :reduce +)
=> 10)
(check ($ #(#(1 1) #(2 2) #(3 3) #(4 4) #(5 5))
       :map identity  ; 保持子向量不变
       :reduce vector-append)
       => #(1 1 2 2 3 3 4 4 5 5))

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :index-where even?) => 1)
  (check (vec :index-where (@ > _ 3)) => 3)
  (check (vec :index-where (@ > _ 5)) => -1))

(let ((vec (array #(1 2 3 4 5))))
  (check (vec :last-index-where even?) => 3)
  (check (vec :last-index-where (@ > _ 3)) => 4)
  (check (vec :last-index-where (@ > _ 5)) => -1))

(check ($ #(2 4 6 7 8 9) :take-while even?) => #(2 4 6))
(check ($ #(1 3 5 7) :take-while odd?) => #(1 3 5 7))
(check ($ #() :take-while even?) => #())
(check ($ #(1 2 3 4) :take-while even?) => #())
(check ($ #(0 0 0 1 0) :take-while zero?) => #(0 0 0))

(check ($ #(1 2 3 4 5) :max-by identity) => 5)
(check ($ #("apple" "banana" "pear") :max-by string-length) => "banana")
(check-catch 'value-error ($ #() :max-by identity))
(check-catch 'type-error ($ #(1 2 3) :max-by "not-a-function"))
(check-catch 'type-error ($ #(1 2 3) :max-by (lambda (x) "not-a-number")))

(check ($ #(1 2 3 4 5) :min-by identity) => 1)
(check ($ #("apple" "banana" "pear") :min-by string-length) => "pear")
(check-catch 'value-error ($ #() :min-by identity))
(check-catch 'type-error ($ #(1 2 3) :min-by "not-a-function"))
(check-catch 'type-error ($ #(1 2 3) :min-by (lambda (x) "not-a-number")))

(check ($ #() :max-by-option identity) => (none))

(check ($ #() :min-by-option identity) => (none))

(check (object->string ($ #(1 2 3))) => "#(1 2 3)")

(let ((vec ($ #("Hello" "World"))))
  (check (vec :to-string) => "#(\"Hello\" \"World\")"))

(let ((vec ($ #())))
  (check (vec :to-string) => "#()"))

(let ((vec ($ "test123 你好" :to-rich-vector)))
  (check (vec :to-string) => "#(#\\t #\\e #\\s #\\t #\\1 #\\2 #\\3 #\\space #\\你 #\\好)"))

(let1 v ($ #(1 2 3))
  (check (v :count) => 3)
  (check (v :count (cut > <> 1)) => 2)
  (check (v :make-string) => "123")
  (check (v :make-string " ") => "1 2 3")
  (check (v :make-string "[" "," "]") => "[1,2,3]")
  
  (check-catch 'wrong-number-of-args (v :make-string "[" ","))
  (check-catch 'type-error (v :make-string 123 "," "]"))
  (check-catch 'type-error (v :make-string "[" 123 "]"))
  (check-catch 'type-error (v :make-string "[" "," 123))
)

(check ($ #("a" "b" "c") :make-string) => "abc")

(let ((vec (rich-vector #(1 2 3))))
  (check (vec :to-list) => '(1 2 3)))

(let ((vec (rich-vector #(1 2 3))))
  (check (vec :to-rich-list) => (rich-list '(1 2 3)))
  (check ((vec :to-rich-list) :collect) => '(1 2 3)))

(let1 v ($ #(1 2 3))
  (v :set! 0 2)
  (check (v 0) => 2)
  (check-catch 'index-error (v -1))
  (check-catch 'index-error (v 3)))

(check-catch 'index-error (array :empty :set! 0 1))

(check (rich-vector :empty :append #(1)) => #(1))
(check (rich-vector :empty :append ($ #(1))) => #(1))

(check ($ #(1) :append #()) => #(1))
(check ($ #(1) :append (rich-vector :empty)) => #(1))

(check ($ #(1) :append #(2 3)) => #(1 2 3))
(check ($ #(1) :append ($ #(2 3))) => #(1 2 3))
       
(check (rich-hash-table :empty) => ($ (hash-table)))
(check (rich-hash-table :empty :collect) => (hash-table))

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (check (ht :find (lambda (k v) (and (symbol? k) (even? v)))) => (option (cons 'b 2)))
  (check ((ht :find (lambda (k v) (> v 4))) :empty?) => #t))

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (check ((ht :get 'a) :get) => 1)
  (check ((ht :get 'd) :empty?) => #t))

(let1 ht1 ($ (hash-table 'a 1 'b 2 'c 3))
  (let1 ht2 (ht1 :remove 'b)
    (check-true  (ht1 :contains 'b))
    (check-false (ht2 :contains 'b))
    (check ((ht2 :get 'c) :get) => 3)))

(let1 ht3 ($ (hash-table 'x 9 'y 8))
  (ht3 :remove! 'x)
  (check-false (ht3 :contains 'x))
  (check ((ht3 :get 'y) :get) => 8))

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (check-true (ht :contains 'a))
  (check-false (ht :contains 'd)))

(let1 ht ($ (hash-table 'a 5 'b 8 'c 10 'd 12))
    (check (ht :forall (lambda (k v) (> v 4)))         => #t)  
    (check (ht :forall (lambda (k v) (< v 13)))        => #t)  
    (check (ht :forall (lambda (k v) (even? v)))       => #f)  
  
    (check (ht :forall (lambda (k v)                 
                    (and (symbol? k) (> v 4))))        => #t)  

    (check (ht :forall (lambda (k v)                 
                    (symbol? k)))                      => #t)  
  
    (check (ht :forall (lambda (k v) (eq? k v)))       => #f)  
)

(let1 ht-empty ($ (hash-table))
    (check (ht-empty :forall (lambda (k v) (string? v))) => #t)
)

(let1 ht-mixed ($ (hash-table 'id 10 'score 85 3.14 "pi"))
    (check (ht-mixed :forall (lambda (k v) (number? v))) => #f) 
    (check (ht-mixed :forall (lambda (k v) (and (integer? v) (even? v)))) => #f) 
)

(let1 ht-fail ($ (hash-table 'valid 42 'invalid "string"))
    (check (ht-fail :forall (lambda (k v) (number? v)))    => #f) 

    (check (ht-fail :forall (lambda (k v) 
                         (and (symbol? k) (number? v) (positive? v)))) => #f)
)

;; nested hash table test
(let1 ht-nested ($ (hash-table 
                    'a ($ (hash-table 'x 10)) 
                    'b ($ (hash-table 'y 20))))
  (check (ht-nested :forall 
                   (lambda (k sub-ht) 
                     (sub-ht :forall (lambda (k v) (> v 9))))) => #t)
)

(let ((ht ($ (hash-table 'a 1 'b "2" 'c 3))))
  (check (ht :exists (lambda (k v) (string? v))) => #t))

(let ((ht ($ (hash-table "a" 1 'b 2 3 'c))))
  (check (ht :exists (lambda (k v) (number? k))) => #t))

(let ((ht ($ (hash-table))))
  (check (ht :exists (lambda (k v) #t)) => #f))

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (let1 r (ht :map (lambda (k v) (values k (+ v 1)))
              :collect)
    (check (r 'a) => 2)
    (check (r 'b) => 3)
    (check (r 'c) => 4)))
      
(define ht 
  ($ (hash-table 'a 2 'b 5 'c 8 'd 10 'e 1 'f "test" 'g -2)))

(check (ht :count (lambda(k v) (and (number? v) (even? v)))) => 4)
(check (ht :count (lambda(k v) (and (number? v) (odd? v)))) => 2)

(let  ((ht ($ (hash-table 'x 10 'y 20 'z 30 'new 40)))     
      (sum 0))                                  
  (ht :for-each (lambda (k v) 
               (set! sum (+ sum v))))             
  (check sum => 100)                             
)

;; Empty hash table
(let ((ht ($ (make-hash-table)))                      
      (call-counter 0))                          
  
  (ht :for-each (lambda (k v) 
               (set! call-counter (+ call-counter 1))))
  
  (check call-counter => 0)                      
)

;; Nested hash tables
(let* ((inner ($ (hash-table 'x 100 'y 200)))      
       (outer ($ (hash-table 'a inner 'b 42)))     
       (total 0))                                  
  
  (outer :for-each 
    (lambda (k v)
      (if (case-class? v)
        (v  :for-each
            (lambda (k v)
            (set! total (+ total v))))
        (set! total (+ total v)))))
  
  (check total => 342)                          
)

(let1 ht ($ (hash-table 'a 1 'b 2 'c 3))
  (let1 r (ht :filter (lambda (k v) (even? v)) :collect)
    (check r => (hash-table 'b 2))))

(check-report)

