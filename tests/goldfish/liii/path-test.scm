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
        (liii os))

; (check-set-mode! 'report-failed)

(check (path-dir? ".") => #t)
(check (path-dir? "..") => #t)

(when (not (os-windows?))
  (check (path-dir? "/") => #t)
  (check (path-dir? "/tmp") => #t)
  (check (path-dir? "/no_such_dir") => #f))

(when (os-windows?)
  (check (path-dir? "C:/") => #t)
  (check (path-dir? "C:/no_such_dir/") => #f))

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

(check ((path) :get-type) => 'posix)
(check ((path) :get-parts) => #("."))

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

(when (or (os-linux?) (os-macos?))
  (check-true (path :/ "tmp" :dir?))
  (check-true (path :/ "tmp/" :dir?))
  (check-false (path :from-parts #("/" "tmpxxxx") :dir?))
  (check-true (path :from-parts #("/" "tmp" "") :dir?))
  (chdir "/tmp")
  (mkdir "tmpxxxx")
  (check-true (path :from-parts #("/" "tmp" "/" "tmpxxxx" "") :dir?))
  (rmdir "tmpxxxx"))

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

(check-report)

