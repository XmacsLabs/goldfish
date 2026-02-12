# HTTP 并发方案选择指南

## 问题背景

CPR 库使用固定大小的线程池（默认 = CPU 核心数）来实现异步 HTTP。这意味着：
- 4 核 CPU：最多 4 个并发请求
- 8 核 CPU：最多 8 个并发请求

如果需要更高的并发度（如 100+），需要使用替代方案。

## 方案对比

### 方案 1：当前实现（CPR + Marl）

```scheme
(g_http-async-get url params callback)
```

**适用场景**：
- ✅ 少量并发请求（< CPU 核心数）
- ✅ 简单的 API 调用
- ✅ 不需要极致性能

**限制**：
- ❌ 并发度受限于 CPU 核心数
- ❌ 不适合高并发爬虫/压测

---

### 方案 2：直接使用 libcurl multi（推荐用于高并发）

如果需要 100+ 并发连接，可以实现基于 libcurl multi 的版本：

```cpp
// 伪代码 - 需要实际实现
static s7_pointer
f_http_multi_get (s7_scheme* sc, s7_pointer args) {
    // 使用 curl_multi_* API
    // 真正的单线程多路复用（epoll/kqueue/IOCP）
    // 可以处理数千个并发连接
}
```

**优点**：
- ✅ 真正的单线程多路复用
- ✅ 不受线程池限制
- ✅ 1000+ 并发连接

**缺点**：
- ❌ 需要额外实现
- ❌ 不能使用 CPR 的便利接口

---

### 方案 3：连接池模式

为特定场景（如数据库连接、微服务调用）维护持久连接：

```scheme
; 创建一个连接池（最多 100 个连接）
(define pool (g_http-pool-create "https://api.example.com" 100))

; 使用连接池发送请求
(g_http-pool-get pool "/endpoint" params callback)
```

---

## 当前建议

### 对于普通用户

继续使用 `g_http-async-get`：

```scheme
; 适合大多数场景
(g_coroutine-scheduler-start 4)

(g_http-async-get "https://api.example.com/user/1" '() callback1)
(g_http-async-get "https://api.example.com/user/2" '() callback2)
(g_http-async-get "https://api.example.com/user/3" '() callback3)
```

### 对于高并发需求

**当前 workaround**：手动分批

```scheme
; 如果 CPU 是 4 核，每批发 4 个请求
(define (batch-requests urls batch-size callback)
  (let loop ((remaining urls)
             (current-batch '()))
    (cond
      ; 当前批次已满，等待完成
      ((= (length current-batch) batch-size)
       (wait-for-batch current-batch)
       (loop remaining '()))
      
      ; 还有 URL，加入当前批次
      ((not (null? remaining))
       (let ((url (car remaining)))
         (g_http-async-get url '() callback)
         (loop (cdr remaining) (cons url current-batch))))
      
      ; 处理最后一批
      ((not (null? current-batch))
       (wait-for-batch current-batch))
      
      ; 全部完成
      (else 'done))))
```

**长期方案**：如果需要真正的高并发，考虑：
1. 使用专门的 HTTP 客户端库（如基于 libcurl 的自定义实现）
2. 使用外部工具（如 `wrk`, `ab`）进行压测
3. 考虑其他语言/运行时（如 Go, Node.js）专门处理高并发 I/O

---

## 文档更新建议

在 `g_http-async-get` 的文档中添加：

```markdown
### 并发限制

`g_http-async-get` 使用底层 CPR 库实现异步 HTTP。CPR 使用固定大小的线程池
（默认等于 CPU 核心数），因此并发请求数受限于 CPU 核心数。

示例：
- 4 核 CPU：最多 4 个并发请求
- 8 核 CPU：最多 8 个并发请求

如果需要更高的并发度（如 100+），请考虑：
1. 使用外部 HTTP 客户端工具
2. 实现基于 libcurl multi 的自定义方案
```
