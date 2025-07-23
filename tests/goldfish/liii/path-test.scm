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

**函数签名**
(path-dir? path) → boolean

**参数**
path : string 类型的文件路径

**返回值**
当路径存在且为目录时返回 #t
当路径不存在或不是目录时返回 #f

**特殊情况**
* "" (空字符串) → #f
* "." (当前目录) → #t（总是存在）
* ".." (上级目录) → #t（总是存在）

**跨平台行为**
* 在类Unix系统上，根目录 "/" 总是返回 #t
* 在Windows系统上，驱动器根目录如 "C:\" 总是返回 #t
* 路径区分大小写：类Unix系统区分大小写，Windows系统不区分

**示例**
```scheme
(path-dir? "/tmp") → #t      ; 当/tmp目录存在时
(path-dir? "/no/such/dir") → #f ; 目录不存在
(path-dir? "/etc/passwd") → #f  ; 虽然存在，但是文件不是目录
(path-dir? ".") → #t          ; 当前目录总返回#t
```

**错误处理**
该函数不会因为路径不存在而报错，而是返回 #f
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

(check (path-file? ".") => #f)
(check (path-file? "..") => #f)

(when (os-linux?)
  (check (path-file? "/etc/passwd") => #t))

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
    (check (= (path-getsize test-file) old-size) => #t))
  
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

(check (path "file.txt" :name) => "file.txt")
(check (path "archive.tar.gz" :name) => "archive.tar.gz") 
(check (path ".hidden" :name) => ".hidden") 
(check (path "noext" :name) => "noext")    
(check (path "" :name) => "")  ; 空路径
(check (path "." :name) => "")  ; 当前目录
(check (path ".." :name) => "..")  ; 上级目录

(when (or (os-macos?) (os-linux?))
  (check (path "/path/to/file.txt" :name) => "file.txt"))

(check (path "file.txt" :stem) => "file")
(check (path "archive.tar.gz" :stem) => "archive.tar")  ; 只去掉最后一个后缀
(check (path ".hidden" :stem) => ".hidden")  ; 隐藏文件保留完整名称
(check (path "noext" :stem) => "noext")  ; 无后缀名保留完整名称
(check (path "" :stem) => "")  ; 空路径
(check (path "." :stem) => "")  ; 当前目录
(check (path ".." :stem) => "..")  ; 上级目录
(when (or (os-linux?) (os-macos?))
  (check (path "/path/to/file.txt" :stem) => "file"))  

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

;; 测试 path@temp-dir 方法
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
    (check-true (string-starts? (temp-path :to-string) "/"))))

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

