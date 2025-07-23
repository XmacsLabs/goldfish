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

(import (liii path)
        (liii check)
        (liii os)
        (liii string)
        (liii base))

(check-set-mode! 'report-failed)

#|
path-dir?
判断给定路径是否为目录

函数签名
----
(path-dir? path) → boolean

参数
----
path : 文件路径（string类型）

返回值
----
#t : 路径存在且为目录
#f : 路径不存在或不是目录

特殊情况
----
- "" 空字符串 → #f
- "."  当前目录 → #t（总是存在）
- ".." 上级目录 → #t（总是存在）

跨平台行为
----
在类Unix系统上：根目录 "/" 总是返回 #t
在Windows系统上：驱动器根目录如 "C:\" 总是返回 #t
路径区分大小写：类Unix系统区分大小写，Windows系统不区分

错误处理
----
不存在的路径返回 #f，不会报错
|#

;; 基本功能测试
(check (path-dir? ".") => #t)
(check (path-dir? "..") => #t)

;; 边界情况测试
(check (path-dir? "") => #f)
(check (path-dir? "nonexistent") => #f)
(check (path-dir? "#\null") => #f)

;; 文件测试（不是目录）
(when (or (os-linux?) (os-macos?))
  (check (path-dir? "/etc/passwd") => #f))

(when (os-windows?)
  (check (path-dir? "C:\\Windows\\System32\\drivers\\etc\\hosts") => #f))

(when (not (os-windows?))
  ;; 根目录测试
  (check (path-dir? "/") => #t)
  ;; 常用目录测试
  (check (path-dir? "/tmp") => #t)
  (check (path-dir? "/etc") => #t)
  (check (path-dir? "/var") => #t)
  ;; 不存在的目录测试
  (check (path-dir? "/no_such_dir") => #f)
  (check (path-dir? "/not/a/real/path") => #f)
  ;; 相对路径测试
  (check-true (path-dir? (os-temp-dir)))
  )

(when (os-windows?)
  ;; 根目录测试
  (check (path-dir? "C:/") => #t)
  (check (path-dir? "D:/") => #t)
  ;; 常用目录测试
  (check (path-dir? "C:/Windows") => #t)
  (check (path-dir? "C:/Program Files") => #t)
  ;; 不存在的目录测试
  (check (path-dir? "C:/no_such_dir/") => #f)
  (check (path-dir? "Z:/definitely/not/exist") => #f)
  ;; 大小写测试
  (check (path-dir? "C:/WINDOWS") => #t)
  (check (path-dir? "c:/windows") => #t))

#|
path-file?
判断给定路径是否为文件

函数签名
----
(path-file? path) → boolean

参数
----
path : 文件路径（string类型）

返回值
----
#t : 路径存在且为文件
#f : 路径不存在或不是文件

特殊情况
----
- "" 空字符串 → #f
- "."  当前目录 → #f（目录不是文件）
- ".." 上级目录 → #f（目录不是文件）

跨平台行为
----
在类Unix系统上：普通文件返回 #t
在Windows系统上：遵循驱动器路径规则
路径区分大小写：类Unix系统区分大小写，Windows系统不区分相关内容

错误处理
----
不存在的路径返回 #f，不会报错

相关函数
----
- path-dir? : 判断是否为目录
- path-exists? : 判断路径是否存在
|#

;; 基本功能测试
(check (path-file? ".") => #f)
(check (path-file? "..") => #f)

;; 边界情况测试
(check (path-file? "") => #f)
(check (path-file? "nonexistent") => #f)
(check (path-file? "#\null") => #f)

;; 文件测试（是文件）
(when (or (os-linux?) (os-macos?))
  (check (path-file? "/etc/passwd") => #t)
  (check (path-file? "/etc/hosts") => #t)
  (check (path-file? "/bin/ls") => #t))

(when (os-windows?)
  (check (path-file? "C:/Windows/System32/drivers/etc/hosts") => #t)
  (check (path-file? "C:/Windows/win.ini") => #t))

(when (not (os-windows?))
  ;; 根目录测试（不是文件）
  (check (path-file? "/") => #f)
  ;; 常用目录测试（不是文件）
  (check (path-file? "/tmp") => #f)
  (check (path-file? "/etc") => #f)
  (check (path-file? "/var") => #f)
  ;; 不存在的文件测试
  (check (path-file? "/no_such_file.txt") => #f)
  (check (path-file? "/not/a/real/file") => #f)
  ;; 相对路径测试
  (check (path-file? (os-temp-dir)) => #f)) ; temp-dir是目录

(when (os-windows?)
  ;; 根目录测试
  (check (path-file? "C:/") => #f)
  (check (path-file? "D:/") => #f)
  ;; 常用目录测试
  (check (path-file? "C:/Windows") => #f)
  (check (path-file? "C:/Program Files") => #f)
  ;; 不存在的文件测试
  (check (path-file? "C:/no_such_file.txt") => #f)
  (check (path-file? "Z:/definitely/not/exist") => #f)
  ;; 大小写测试
  (check (path-file? "C:/WINDOWS/explorer.exe") => #t)
  (check (path-file? "c:/windows/explorer.exe") => #t))

;; 测试临时文件
(let ((test-file (string-append (os-temp-dir) "/test_path_file.txt")))
  ;; Ensure file doesn't exist initially
  (when (file-exists? test-file)
    (delete-file test-file))
  
  ;; 测试不存在的文件
  (check (path-file? test-file) => #f)
  
  ;; 创建文件
  (with-output-to-file test-file
    (lambda () (display "test content for path-file?")))
  
  ;; 测试存在的文件
  (check-true (path-file? test-file))
  
  ;; 清理
  (delete-file test-file))

;; 测试真实文件
(when (or (os-linux?) (os-macos?))
  (let ((real-file (string-append (os-temp-dir) "/real_file.txt")))
    
    ;; 创建真实文件
    (with-output-to-file real-file
      (lambda () (display "real content")))
    
    ;; 测试真实文件
    (check (path-file? real-file) => #t)
    
    ;; 清理
    (delete-file real-file)))

(when (not (os-windows?))
  (check-true (> (path-getsize "/") 0))
  (check-true (> (path-getsize "/etc/hosts") 0)))

(when (os-windows?)
  (check-true (> (path-getsize "C:") 0))
  (check-true (> (path-getsize "C:/Windows") 0))
  (check-true (> (path-getsize "C:\\Windows\\System32\\drivers\\etc\\hosts") 0)))

(let ((file-name "中文文件名.txt")
      (file-content "你好，世界！"))
  (define temp-dir (os-temp-dir))
  (define file-path (string-append temp-dir (string (os-sep)) file-name))
  (path-write-text file-path file-content)
  (check (path-read-text file-path) => file-content)
  (delete-file file-path))

; Test for path-read-bytes
(let ((file-name "binary-test.dat")
      (file-content "Hello, binary world!"))
  (define temp-dir (os-temp-dir))
  (define file-path (string-append temp-dir (string (os-sep)) file-name))
  
  ; Write a simple string to the file
  (path-write-text file-path file-content)
  
  ; Read it back using path-read-bytes
  (let ((read-content (path-read-bytes file-path)))
    ; Check that it's a bytevector
    (check-true (bytevector? read-content))
    ; Check that it has the correct length
    (check (bytevector-length read-content) => (string-length file-content))
    ; Check that the content matches when converted back to string
    (check (utf8->string read-content) => file-content))
  
  (delete-file file-path))

;; 测试 path-append-text
(let ((file-name "append-test.txt")
      (initial-content "Initial content\n")
      (append-content "Appended content\n"))
  (define temp-dir (os-temp-dir))
  (define file-path (string-append temp-dir (string (os-sep)) file-name))
  
  ;; 先写入初始内容
  (path-write-text file-path initial-content)
  
  ;; 验证初始内容
  (check (path-read-text file-path) => initial-content)
  
  ;; 追加内容
  (path-append-text file-path append-content)
  
  ;; 验证追加后的内容
  (check (path-read-text file-path) => (string-append initial-content append-content))
  
  ;; 清理
  (delete-file file-path))

;; 测试追加到不存在的文件
(let ((file-name "append-new-file.txt")
      (content "Content for new file\n"))
  (define temp-dir (os-temp-dir))
  (define file-path (string-append temp-dir (string (os-sep)) file-name))
  
  ;; 确保文件不存在
  (when (file-exists? file-path)
    (delete-file file-path))
  
  ;; 追加到不存在的文件
  (path-append-text file-path content)
  
  ;; 验证内容
  (when (or (os-macos?) (os-linux?))
    (check (path-read-text file-path) => content))
  
  ;; 清理
  (delete-file file-path))

(let ((test-file (string-append (os-temp-dir) "/test_touch.txt")))
  ;; Ensure file doesn't exist initially
  (when (file-exists? test-file)
    (delete-file test-file))
  
  ;; Test creating new file
  (check-true (path-touch test-file))
  (check-true (file-exists? test-file))
  
  ;; Test updating existing file
  (let ((old-size (path-getsize test-file)))
    (check-true (path-touch test-file))
    (check (>= (path-getsize test-file) old-size) => #t))
  
  ;; Clean up
  (delete-file test-file))

(check ((path) :get-type) => 'posix)
(check ((path) :get-parts) => #("."))

#|
path@of-drive
根据驱动器字母构造 Windows 盘符根路径。

语法
----
(path :of-drive drive-letter)

参数
----
drive-letter : char
驱动器字母（A-Z，大小写不敏感，但应加 #\ 前缀）。

返回值
-----
string
格式化后的盘符根路径（字母大写 + :\\ 后缀）。

错误
----
type-error
若 drive-letter 不是 char 类型或无法被大写化（非英文字母）。

|#

;; Example of type-error
(check-catch 'type-error (path :of-drive 1 :to-string))

(check (path :of-drive #\D :to-string) => "D:\\")
(check (path :of-drive #\d :to-string) => "D:\\")

(check (path :root :to-string) => "/")

(when (or (os-macos?) (os-linux?))
  (check (path :from-parts #("/" "tmp")) => (path :/ "tmp"))
  (check (path :from-parts #("/" "tmp" "test")) => (path :/ "tmp" :/ "test"))
  (check (path :from-parts #("/", "tmp") :to-string) => "/tmp"))

(when (os-windows?)
  (check (path :/ "C:" :to-string) => "C:\\"))

(when (not (os-windows?))
  (check (path :/ "root" :to-string) => "/root"))

(when (os-windows?)
  (check (path "a\\b") => (path :./ "a" :/ "b"))
  (check (path "C:\\") => (path :of-drive #\C))
  (check (path "C:\\Users") => (path :of-drive #\C :/ "Users")))

(when (or (os-linux?) (os-macos?))
  (check (path "a/b") => (path :./ "a" :/ "b"))
  (check (path "/tmp") => (path :/ "tmp"))
  (check (path "/tmp/tmp2") => (path :/ "tmp" :/ "tmp2")))

(when (os-linux?)
  (check (path :from-env "HOME" :to-string) => (path :home :to-string)))

(when (os-windows?)
  (check (path :from-env "USERPROFILE" :to-string) => (path :home :to-string)))

#|
path%name
获取路径的最终文件或目录名称部分。

语法
----
(path-instance :name)

参数
----
无

返回值
-----
string
返回路径的最终名称部分，即从最后一个路径分隔符到末尾的部分。

描述
----
`path%name` 提取路径中的最终名称部分，忽略前面的所有路径层级。这个名称可以是文件名或最后一个目录名。

行为特征
------
- 返回路径中的最终名称（文件名或目录名）
- 处理空路径、当前目录 "."、上级目录 ".." 等特殊情况
- 对于以 "." 结尾的路径返回空字符串
- 保留完整文件名，包括所有后缀
- 跨平台兼容 Windows、Unix/Linux/macOS 的路径规则

特殊情况
------
- "" 空路径 → 返回空字符串
- "." 当前目录 → 返回空字符串  
- ".." 上级目录 → 返回 ".."
- 以"/"结尾的路径 → 返回空字符串
- 多级路径 → 返回最后一级的完整名称

跨平台行为
---------
- Unix/Linux/macOS: 以 `/` 作为路径分隔符
- Windows: 以 `\` 或 `/` 作为路径分隔符
- 返回结果格式一致，不受平台影响

相关函数
--------
- path%stem: 获取去除后缀的文件名
- path%suffix: 获取文件扩展名
|#

(check (path "file.txt" :name) => "file.txt")
(check (path "archive.tar.gz" :name) => "archive.tar.gz") 
(check (path ".hidden" :name) => ".hidden") 
(check (path "noext" :name) => "noext")    
(check (path "" :name) => "")  ; 空路径
(check (path "." :name) => "")  ; 当前目录
(check (path ".." :name) => "..")  ; 上级目录

(when (or (os-macos?) (os-linux?))
  (check (path "/path/to/file.txt" :name) => "file.txt"))

#|
path%stem
获取路径的stem（去掉最后一个后缀的文件名部分）。

语法
----
(path-instance :stem)

参数
----
无

返回值
-----
string
返回去掉最后一个后缀的文件名部分（stem）。

描述
----
`path%stem` 提取文件名中去掉最后一个扩展名的部分。这在处理文件时非常有用，特别是需要获取"基本文件名"而不关心其扩展名的情况。

行为特征
------
- 保留隐藏文件（以点开头）的完整名称
- 只去掉最后一个扩展名（如 "archive.tar.gz" → "archive.tar"）
- 无扩展名的文件返回原名
- 正确处理特殊目录名称（"." 和 ".."）

扩展名规则
------
1. 文件名包含点号时需考虑多种情况
2. 以点开头的隐藏文件视为无扩展名
3. 多个点号的情况，只识别最后一个点号之后的部分为扩展名

错误处理
------
不返回错误，对于所有输入都会返回合理的字符串结果。

跨平台行为
---------
路径分隔符和扩展名规则在所有平台上保持一致。
|#

;; 基本功能测试
(check (path "file.txt" :stem) => "file")
(check (path "archive.tar.gz" :stem) => "archive.tar")
(check (path ".hidden" :stem) => ".hidden")
(check (path "noext" :stem) => "noext")
(check (path "" :stem) => "")
(check (path "." :stem) => "")
(check (path ".." :stem) => "..")

;; 扩展的测试案例
(check (path "script.bin.sh" :stem) => "script.bin")
(check (path "image.jpeg" :stem) => "image")
(check (path "README" :stem) => "README")
(check (path "config.yaml.bak" :stem) => "config.yaml")
(check (path "test-file.name-with-dots.txt" :stem) => "test-file.name-with-dots")

;; 隐藏文件测试
(check (path ".gitignore" :stem) => ".gitignore")
(check (path ".bashrc" :stem) => ".bashrc")
(check (path ".profile" :stem) => ".profile")

;; 复杂路径测试
(when (or (os-linux?) (os-macos?))
  (check (path "/usr/bin/file.txt" :stem) => "file")
  (check (path "/path/to/archive.tar.gz" :stem) => "archive.tar")
  (check (path "/home/user/.hidden" :stem) => ".hidden"))  

#|
path%suffix
获取路径的后缀（扩展名）部分。

语法
----
(path-instance :suffix)

参数
----
无

返回值
-----
string
返回文件名的后缀部分，包括点号(.)。如果没有后缀则返回空字符串。

描述
----
`path%suffix` 提取文件名中的最后一个扩展名部分。这对于处理文件类型非常有用，特别是需要根据文件扩展名执行不同操作的情况。

行为特征
------
- 只返回最后一个扩展名（如 "archive.tar.gz" → ".gz"）
- 隐藏文件（以点开头）视为无扩展名
- 无扩展名的文件返回空字符串
- 正确处理特殊目录名称（"." 和 ".."）

扩展名规则
------
1. 文件名包含点号时，最后一个点号之后的部分为扩展名
2. 以点开头的隐藏文件（如 ".hidden"）视为无扩展名
3. 多个点号的情况，只识别最后一个点号之后的部分为扩展名

错误处理
------
不返回错误，对于所有输入都会返回合理的字符串结果。

跨平台行为
---------
路径分隔符和扩展名规则在所有平台上保持一致。
|#

(check (path "file.txt" :suffix) => ".txt")
(check (path "archive.tar.gz" :suffix) => ".gz")  ; 只保留最后一个后缀
(check (path ".hidden" :suffix) => "")  
(check (path "noext" :suffix) => "")  
(check (path "/path/to/file.txt" :suffix) => ".txt")  ; 绝对路径
(check (path "C:/path/to/file.txt" :suffix) => ".txt")  ; Windows路径
(check (path "" :suffix) => "")  ; 空路径
(check (path "." :suffix) => "")  ; 当前目录
(check (path ".." :suffix) => "")  ; 上级目录

(check-true ((path "/tmp/test") :equals (path "/tmp/test")))

(when (or (os-linux?) (os-macos?))
  (check-false (path :/ "tmp" :file?))
  (chdir "/tmp")
  (mkdir "tmpxxxx") 
  (check-false (path :from-parts #("/" "tmp" "/" "tmpxxxx") :file?))
  (rmdir "tmpxxxx"))

#|
path%dir?
判断路径所指向目录是否存在。

语法
----
(path :from-parts string-vector :dir?) ; 通过字符串数组显式构造路径对象
示例：(path :from-parts #("/" "tmp" "log")) ; ⇒ /tmp/log
(path :/ segment1 segment2 ...) ; 构建 Unix 风格绝对路径
示例：(path :/ "tmp" "app.log") ; ⇒ /tmp/app.log

参数
----
无显式参数（通过 path 对象隐式操作文件系统）。

返回值
-----
boolean
#t: 路径存在且为目录
#f: 路径不存在或不是目录

错误
----
无（不存在的路径返回 #f 而非报错）。

|#

(when (or (os-linux?) (os-macos?))
  (check-true (path :/ "tmp" :dir?))
  (check-true (path :/ "tmp/" :dir?))
  (check-false (path :from-parts #("/" "tmpxxxx") :dir?))
  (check-true (path :from-parts #("/" "tmp" "") :dir?))
  (chdir "/tmp")
  (mkdir "tmpxxxx")
  (check-true (path :from-parts #("/" "tmp" "/" "tmpxxxx" "") :dir?))
  (rmdir "tmpxxxx"))

(when (os-windows?)
  ;; 基本目录检测
  (check-true (path :from-parts #("C:" "Windows") :dir?))
  (check-true (path :from-parts #("C:\\" "Windows\\") :dir?))
  
  ;; 大小写不敏感测试
  (check-true (path :from-parts #("C:" "WINDOWS") :dir?))
  
  ;; 不存在的路径
  (check-false (path :from-parts #("C:" "Windows\\InvalidPath") :dir?))
  
  ;; 带空格的路径
  (check-true (path :from-parts #("C:" "Program Files") :dir?))
  
  ;; 特殊目录（需存在）
  (check-true (path :from-parts #("C:" "Windows" "System32") :dir?)))

(check-false ((path) :absolute?))
(check (path :/ "C:" :get-type) => 'windows)
(check (path :/ "C:" :get-parts) => #())
(check-true (path :/ "C:" :absolute?))
(check-true (path :from-parts #("/" "tmp") :absolute?))
(check-false (path :from-parts #("tmp") :absolute?))

(when (or (os-linux?) (os-macos?))
  (check-true (path :/ "tmp" :exists?)))

(when (not (os-windows?))
  (check (path :/ "etc" :/ "passwd" :to-string) => "/etc/passwd"))

(when (os-windows?)
  (check (path :of-drive #\C :to-string) => "C:\\"))

#|
path-exists?
判断给定路径是否存在。

语法
----
(path-exists? path) → boolean

参数
----
**path** : string
文件或目录路径。

返回值
-----
boolean
当路径存在时返回 #t，路径不存在时返回 #f。

描述
----
`path-exists?` 用于判断指定的文件或目录是否存在，而不会因路径不存在或格式错误而报错。

行为特征
------
- "" (空字符串) → #f
- "." (当前目录) → #t（总是存在）
- ".." (上级目录) → #t（总是存在）
- 路径区分大小写：类Unix系统区分大小写，Windows系统不区分

跨平台行为
----------
- **类Unix系统**：根目录 "/" 总是返回 #t
- **Windows系统**：驱动器根目录如 "C:\" 总是返回 #t

应用场景
--------
- 文件操作前检查文件是否存在，避免错误
- 条件判断中决定是否需要进行文件操作
- 与其他path函数配合使用，构建健壮的filesystem代码

错误处理
------
该函数不会因为路径不存在或格式不正确而报错，而是返回 #f。
|#

;; 基本功能测试
(check-true (path-exists? "."))
(check-true (path-exists? ".."))

;; 边界情况测试
(check (path-exists? "") => #f)
(check (path-exists? "nonexistent") => #f)
(check (path-exists? "#/null") => #f)

;; 文件存在性测试
(when (or (os-linux?) (os-macos?))
  (check-true (path-exists? "/etc/passwd"))
  (check-true (path-exists? "/bin/sh")))

(when (os-windows?)
  (check-true (path-exists? "C:\\Windows\\System32\\drivers\\etc\\hosts"))
  (check-true (path-exists? "C:\\Windows\\System32\\ntoskrnl.exe")))

;; 目录存在性测试
(when (not (os-windows?))
  ;; 根目录测试
  (check-true (path-exists? "/"))
  ;; 系统目录测试
  (check-true (path-exists? "/etc"))
  (check-true (path-exists? "/var"))
  (check-true (path-exists? "/tmp")))

(when (os-windows?)
  ;; 盘符根目录测试
  (check-true (path-exists? "C:/"))
  (check-true (path-exists? "C:\\"))
  ;; 系统目录测试
  (check-true (path-exists? "C:/Windows"))
  (check-true (path-exists? "C:\\Program Files")))

;; 不存在的路径测试
(when (not (os-windows?))
  (check (path-exists? "/no_such_file") => #f)
  (check (path-exists? "/not/a/real/path") => #f)
  (check (path-exists? "/tmp/nonexistent.txt") => #f))

(when (os-windows?)
  (check (path-exists? "C:\\no_such_file") => #f)
  (check (path-exists? "C:\\Windows\\InvalidPath") => #f)
  (check (path-exists? "Z:\\not_a_drive") => #f))

;; 相对路径测试
(let ((temp-dir (os-temp-dir)))
  (let ((test-file (string-append temp-dir (string (os-sep)) "path_exists_test.txt")))
    ;; 创建临时文件
    (when (not (file-exists? test-file))
      (with-output-to-file test-file
        (lambda () (display "test content"))))
    
    ;; 测试文件存在性
    (check-true (path-exists? test-file))
    
    ;; 测试目录存在性
    (check-true (path-exists? temp-dir))
    
    ;; 清理
    (when (file-exists? test-file)
      (delete-file test-file))))

;; 临时文件和目录测试
(let ((temp-file (path :temp-dir :/ "test_exists.txt")))
  ;; 确保文件不存在
  (when (temp-file :exists?)
    (temp-file :unlink))
  
  ;; 测试文件不存在
  (check-false (path-exists? (temp-file :to-string)))
  
  ;; 创建文件
  (temp-file :write-text "test content")
  
  ;; 测试文件存在
  (check-true (path-exists? (temp-file :to-string)))
  
  ;; 测试目录存在
  (check-true (path-exists? ((path :temp-dir) :to-string)))
  
  ;; 清理
  (temp-file :unlink))

;; 大小写敏感性和空白字符测试
(when (os-windows?)
  (check-true (path-exists? "C:/windows"))
  (check-true (path-exists? "c:/WINDOWS")))

;; 空字符和特殊字符测试
(check (path-exists? "#\null") => #f)
(check (path-exists? "  ") => #f)  ; 空白字符

;; 方法链式调用测试 (path对象的%exists?方法)
(check-true ((path ".") :exists?))
(check-true ((path "..") :exists?))
(when (or (os-linux?) (os-macos?))
  (check-true ((path :/ "etc") :exists?)))

(when (or (os-linux?) (os-macos?))
  (check-false ((path :/ "nonexistent") :exists?)))

;; path-exists? 与其他函数配合使用测试
(let ((test-file "test_combined_usage.txt"))
  ;; 确保文件不存在
  (when (file-exists? test-file)
    (delete-file test-file))
  
  ;; 结合path-exists?进行条件操作
  (check-false (path-exists? test-file))
  
  ;; 创建文件
  (path-touch test-file)
  (check-true (path-exists? test-file))
  
  ;; 验证文件大小（使用path-getsize需要文件存在）
  (check-true (>= (path-getsize test-file) 0))
  
  ;; 清理
  (delete-file test-file))

;; Windows专用路径分隔符测试
(when (os-windows?)
  (check-true (path-exists? "C:\\"))
  (check-true (path-exists? "C:\\Users")))

;; path%append-text 测试
(let ((p (path :temp-dir :/ "append_test.txt")))
  ;; 确保文件不存在
  (when (p :exists?) (p :unlink))
  
  ;; 测试追加到新文件
  (p :append-text "First line\n")
  (when (or (os-linux?) (os-macos?))
    (check (p :read-text) => "First line\n"))
  
  ;; 测试追加到已有文件
  (p :append-text "Second line\n")
  (when (or (os-linux?) (os-macos?))
    (check (p :read-text) => "First line\nSecond line\n"))
  
  ;; 清理
  (p :unlink))

(let ((p (path :temp-dir :/ "append_test.txt"))
      (p-windows (path :temp-dir :/ "append_test_windows.txt")))
  ;; 确保文件不存在
  (when (p :exists?) (p :unlink))
  (when (p-windows :exists?) (p-windows :unlink))
  
  (p :append-text "Line 1\n")
  (p-windows :append-text "Line 1\r\n")
  (when (or (os-linux?) (os-macos?))
    (check (p :read-text) => "Line 1\n"))
  (when (os-windows?)
    (check (p-windows :read-text) => "Line 1\r\n"))
  
  ;; 清理
  (p :unlink)
  (p-windows :unlink))
(let1 test-file (string-append (os-temp-dir) (string (os-sep)) "test_touch.txt")
  ;; Ensure file doesn't exist initially
  (when (file-exists? test-file)
    (delete-file test-file))
  
  ;; Test creating new file with path object
  (let1 p (path test-file)
    (check-false (p :exists?))
    (check-true (p :touch))
    (check-true (p :exists?)))
  
  ;; Clean up
  (delete-file test-file))

;; Test with very long path
(let ((long-name (make-string 200 #\x))
      (temp-dir (os-temp-dir)))
  (let ((p (path temp-dir :/ long-name)))
    (check-true (p :touch))
    (check-true (p :exists?))
    (p :unlink)))

(when (not (os-windows?))
  (check (path :/ "etc" :/ "host" :to-string) => "/etc/host")
  (check (path :/ (path "a/b")) => (path "/a/b")))

(check-catch 'value-error (path :/ (path "/a/b")))

(when (or (os-linux?) (os-macos?))
  (check (path "/" :parent :to-string) => "/")
  (check (path "" :parent :to-string) => ".")
  (check (path "/tmp/" :parent :to-string) => "/")
  (check (path "/tmp/test" :parent :parent :to-string) => "/")
  (check (path "tmp/test" :parent :to-string) => "tmp/")
  (check (path "tmp" :parent :to-string) => ".")
  (check (path "tmp" :parent :parent :to-string) => "."))

(when (os-windows?)
  (check (path "C:" :parent :to-string) => "C:\\")
  (check (path "C:\\Users" :parent :to-string) => "C:\\")
  (check (path "a\\b" :parent :to-string) => "a\\"))

(when (or (os-macos?) (os-linux?))
  ;; 测试删除文件
  (let ((test-file (string-append (os-temp-dir) "/test_delete.txt")))
    ;; 创建临时文件
    (with-output-to-file test-file
      (lambda () (display "test data")))
    ;; 验证文件存在
    (check-true (file-exists? test-file))
    ;; 删除文件（使用 remove）
    (check-true (remove test-file))
    ;; 验证文件已删除
    (check-false (file-exists? test-file))))

(when (or (os-macos?) (os-linux?))
  ;; 测试删除目录
  (let ((test-dir (string-append (os-temp-dir) "/test_delete_dir")))
    ;; 创建临时目录
    (mkdir test-dir)
    ;; 验证目录存在
    (check-true (file-exists? test-dir))
    ;; 删除目录（使用 rmdir）
    (check-true (rmdir test-dir))
    ;; 验证目录已删除
    (check-false (file-exists? test-dir))))

(when (or (os-macos?) (os-linux?))
  ;; 测试 path 对象的 :unlink 和 :rmdir
  (let ((test-file (string-append (os-temp-dir) "/test_path_unlink.txt")))
    (with-output-to-file test-file
      (lambda () (display "test data")))
    (check-true ((path test-file) :unlink))
    (check-false (file-exists? test-file))))

(when (or (os-macos?) (os-linux?))
  (let ((test-dir (string-append (os-temp-dir) "/test_path_rmdir")))
    (mkdir test-dir)
    (check-true ((path test-dir) :rmdir))
    (check-false (file-exists? test-dir))))

(when (or (os-macos?) (os-linux?))
  ;; 测试各种调用方式
  (let ((test-file "/tmp/test_unlink.txt"))
    ;; 默认行为 (missing-ok=#f)
    (check-catch 'file-not-found-error
                 ((path test-file) :unlink))
  
    ;; 显式指定 missing-ok=#t
    (check-true ((path test-file) :unlink #t))
  
    ;; 文件存在时的测试
    (with-output-to-file test-file
      (lambda () (display "test")))
    (check-true ((path test-file) :unlink))
    (check-false (file-exists? test-file))))

(check (path :./ "a" :to-string) => "a")

(when (not (os-windows?))
  (check (path :./ "a" :/ "b" :/ "c" :to-string) => "a/b/c"))

(when (or (os-linux?) (os-macos?))
  (check-true (path :cwd :dir?)))

(when (or (os-linux?) (os-macos?))
  (check ((path :home) :to-string) => (getenv "HOME")))

(when (os-windows?)
  (check (path :home)
   =>    (path :/ (getenv "HOMEDRIVE") :/ "Users" :/ (getenv "USERNAME"))))

#|
path@tempdir
构造指向系统临时目录的 path 对象。

语法
----
(path :temp-dir)

描述
----
该函数返回一个指向操作系统临时目录的 path 对象。

在不同平台上表现：
- **Unix/Linux/macOS**: 通常为 `/tmp`
- **Windows**: 通常为 `C:\Users\[用户名]\AppData\Local\Temp`

返回值
-----
**path对象**
指向系统临时目录的 path 对象，可作为其他路径操作的基础进行链式调用。

特性
---
- 返回的 path 对象总是指向存在的目录
- 跨平台兼容性良好
- 可以进行链式操作

注意事项
------
- 返回的 path 对象始终指向有效目录，不需要检查目录存在性
- 在不同的操作系统会话中，系统临时目录可能会发生变化
- path对象是可变的，但临时目录路径通常保持不变
|#

;; 测试 path@tempdir 方法（path:temp-dir）
(let1 temp-path (path :temp-dir)
  ;; 验证返回的是 path 对象
  (check-true (path :is-type-of temp-path))

  ;; 验证路径存在且是目录
  (check-true (temp-path :exists?))
  (check-true (temp-path :dir?))

  ;; 验证路径与 os-temp-dir 一致
  (check (temp-path :to-string) => (os-temp-dir))

  ;; 验证在不同平台下的基本特征
  (when (os-windows?)
    (check-true (string-starts? (temp-path :to-string) "C:\\")))

  (when (or (os-linux?) (os-macos?))
    (check-true (string-starts? (temp-path :to-string) "/")))

  ;; 验证可以进行文件操作
  (let ((test-file (temp-path :/ "path_temp_dir_test.txt")))
    ;; 确保文件不存在
    (when (test-file :exists?)
      (test-file :unlink))
    
    ;; 测试创建文件
    (test-file :write-text "test content")
    (check-true (test-file :exists?))
    (check (test-file :read-text) => "test content")
    
    ;; 清理
    (test-file :unlink))

  ;; 验证临时目录内部路径构造正确
  (let ((subdir (temp-path :/ "test_sub_directory")))
    ;; 验证路径构造
    (when (or (os-linux?) (os-macos?))
      (check (subdir :to-string) => (string-append (os-temp-dir) "/test_sub_directory")))
    
    (when (os-windows?)
      (check (subdir :to-string) => (string-append (os-temp-dir) "\\test_sub_directory"))))
  
  ;; 验证相对路径操作  
  (let ((rel-path (path "relative")))
    (check-false (rel-path :absolute?))))

#|
path-getsize
获取文件或目录的大小（字节数）。

语法
----
(path-getsize path)

参数
----
path : string
要获取大小的文件或目录路径。路径可以是绝对路径或相对路径。

返回值
-----
integer
返回文件或目录的大小（以字节为单位）。

描述
----
`path-getsize` 用于获取指定文件或目录的字节大小。如果路径指向文件，则返回文件大小；如果指向目录，则返回目录本身的大小（通常很小）。

行为特征
------
- 对于存在的文件，返回其真实字节大小
- 对于目录，返回目录项元数据的大小（不是内容总大小）
- 不能用于获取目录内所有文件的总大小
- 跨平台行为一致

错误处理
------
- 如果路径不存在，函数会抛出 `'file-not-found-error` 错误
- 对于空文件返回 0 字节
- 不会返回负值或无效结果

跨平台行为
----------
- Unix/Linux/macOS 和 Windows 行为一致
- 路径分隔符不影响结果
- 支持 Unicode 文件名

注意事项
------
- 结果始终为非负整数
- 对于大文件可能返回大整数值
- 目录大小指目录本身元数据大小，而非内容总大小
|#

;; 基本功能测试
(check-true (> (path-getsize "/") 0))
(check-true (> (path-getsize "/etc/hosts") 0))

;; 路径对象方法测试
(let ((temp-file (path :temp-dir :/ "test_getsize.txt")))
  ;; 确保文件不存在
  (when (temp-file :exists?)
    (temp-file :unlink))
  
  ;; 测试空文件
  (temp-file :write-text "")
  (check (path-getsize (temp-file :to-string)) => 0)
  
  ;; 测试小文件
  (temp-file :write-text "test")
  (check (path-getsize (temp-file :to-string)) => 4)
  
  ;; 测试较大内容
  (temp-file :write-text "hello world test content")
  (check (path-getsize (temp-file :to-string)) => 24)
  
  ;; 测试中文内容
  (temp-file :write-text "中文测试")
  (check (path-getsize (temp-file :to-string)) => 12)
  
  ;; 清理
  (temp-file :unlink))

;; 测试文件不存在错误
(check-catch 'file-not-found-error
  (path-getsize "/nonexistent/path/file.txt"))

;; 测试现有文件大小
(when (or (os-linux?) (os-macos?))
  (check-true (> (path-getsize "/etc/passwd") 0)))

(when (os-windows?)
  (check-true (> (path-getsize "C:\\Windows\\System32\\drivers\\etc\\hosts") 0)))

;; 目录大小测试
(when (or (os-linux?) (os-macos?))
  (check-true (> (path-getsize "/tmp") 0)))

;; 相对路径测试
(let ((rel-file "test_rel.txt"))
  (when (file-exists? rel-file)
    (delete-file rel-file))
  
  (with-output-to-file rel-file
    (lambda () (display "temporary file for testing")))
  
  (check (path-getsize rel-file) => 26)
  
  (delete-file rel-file))

;; 测试可以基于临时目录创建文件
(let ((temp-file (path :temp-dir :/ "test_file.txt")))
  ;; 写入测试文件
  (temp-file :write-text "test content")
  
  ;; 验证文件存在
  (check-true (temp-file :exists?))
  (check-true (temp-file :file?))
  
  ;; 清理
  (temp-file :unlink))

(check-report)

