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
        (liii string)
        (liii os)
        (liii sys)
        (liii uuid)
        (scheme time))

(check-set-mode! 'report-failed)

(when (os-linux?)
  (check (os-type) => "Linux"))

(when (os-macos?)
  (check (os-type) => "Darwin"))

(when (os-windows?)
  (check (os-type) => "Windows"))

(when (not (os-windows?))
  (let ((t1 (current-second)))
    (os-call "sleep 1")
    (let ((t2 (current-second)))
      (check (>= (ceiling (- t2 t1)) 1) => #t))))

(when (and (os-linux?) (not (string=? "root" (getlogin))))
  (check-true (access "/root" 'F_OK))
  (check-false (access "/root" 'R_OK))
  (check-false (access "/root" 'W_OK))
  (check-true (access (executable) 'X_OK)))

(check (string-null? (getenv "PATH")) => #f)
(unsetenv "PATH")
(check (getenv "PATH") => #f)
(unsetenv "home")
(check (getenv "home") => #f)
(check (getenv "home" "value does not found") => "value does not found")

(when (os-windows?)
  (check (string-starts? (os-temp-dir) "C:") => #t))

(when (os-linux?)
  (check (os-temp-dir) => "/tmp"))

(when (not (os-windows?))
  (check-catch 'file-exists-error
    (mkdir "/tmp"))
  (check (begin
           (let ((test_dir "/tmp/test_124"))
             (when (file-exists? test_dir)
               (rmdir "/tmp/test_124"))
             (mkdir "/tmp/test_124")))
    => #t))

(when (not (os-windows?))
  (check (> (vector-length (listdir "/usr")) 0) => #t))

(let* ((test-dir (string-append (os-temp-dir) (string (os-sep)) (uuid4)))
       (test-dir2 (string-append test-dir (string (os-sep))))
       (dir-a (string-append test-dir2 "a"))
       (dir-b (string-append test-dir2 "b"))
       (dir-c (string-append test-dir2 "c")))
  (mkdir test-dir)
  (mkdir dir-a)
  (mkdir dir-b)
  (mkdir dir-c)
  (let1 r (listdir test-dir)
    (check-true (in? "a" r))
    (check-true (in? "b" r))
    (check-true (in? "c" r)))
  (let1 r2 (listdir test-dir2)
    (check-true (in? "a" r2))
    (check-true (in? "b" r2))
    (check-true (in? "c" r2)))
  (rmdir dir-a)
  (rmdir dir-b)
  (rmdir dir-c)
  (rmdir test-dir))

(when (os-windows?)
  (check (> (vector-length (listdir "C:")) 0) => #t))

(check-false (string-null? (getcwd)))

(check-report)

