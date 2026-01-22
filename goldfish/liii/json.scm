(define-library (liii json)
  (import (liii base) 
          (guenchi json)) ; 复用底层逻辑
  (export
    ;;; --- 基础 IO 与 转换 ---
    json-string-escape 
    string->json 
    json->string
    
    ;;; --- 核心操作 (增删改查) ---
    json-ref  json-ref* json-set  json-set* json-push json-push* json-drop json-drop* json-reduce json-reduce* 
    ;; 类型谓词
    json-null? 
    json-object? 
    json-array? 
    json-string? 
    json-number?
    json-integer?
    json-float?      
    json-boolean?
    
    ;; 状态检查
    json-contains-key? 
    
    ;; 带类型检查与默认值的获取器
    json-ref-string
    json-ref-number
    json-ref-integer
    json-ref-boolean
    json-get-or-else   
    
    ;; 辅助工具
    json-keys)
  
  (begin

    ;;; ---------------------------------------------------------
    ;;; 1. 类型谓词 
    ;;; ---------------------------------------------------------

    (define (json-null? x)
      (eq? x 'null))

    (define (json-object? x)
      ;; rich-json 定义 object 为非空列表 
      (and (list? x) (not (null? x))))

    (define (json-array? x)
      (vector? x))

    (define (json-string? x)
      (string? x))

    (define (json-number? x)
      (number? x))
      
    (define (json-integer? x)
      (integer? x))

    (define (json-float? x)
      (flonum? x)) 

    (define (json-boolean? x)
      (boolean? x))

    ;;; ---------------------------------------------------------
    ;;; 2. 状态检查
    ;;; ---------------------------------------------------------

    (define (json-contains-key? json key)
      (if (not (json-object? json))
          #f
          ;; JSON Object 是 Alist，直接查找 key
          (if (assoc key json) #t #f)))

    ;;; ---------------------------------------------------------
    ;;; 3. 安全获取器
    ;;; ---------------------------------------------------------

    (define (json-get-or-else json default)
      (if (json-null? json)
          default
          json))

    (define (json-ref-string json key default)
      (let ((val (json-ref json key)))
        (if (string? val) val default)))

    (define (json-ref-number json key default)
      (let ((val (json-ref json key)))
        (if (number? val) val default)))
    
    (define (json-ref-integer json key default)
      (let ((val (json-ref json key)))
        (if (integer? val) val default)))

    (define (json-ref-boolean json key default)
      (let ((val (json-ref json key)))
        (if (boolean? val) val default)))

    ;;; ---------------------------------------------------------
    ;;; 4. 辅助工具
    ;;; ---------------------------------------------------------

    (define (json-keys json)
      (if (json-object? json)
          (map car json)
          '()))

  ) ; end of begin
) ; end of define-library