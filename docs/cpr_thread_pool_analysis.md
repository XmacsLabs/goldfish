# CPR 线程池与 Marl 协程的交互分析

## CPR 的异步实现机制

根据 issue 描述，CPR 的异步实现基于：

1. **线程池**：最小 1 个线程，最大 = CPU 核心数
2. **实现方式**：每个请求占用一个线程进行 `poll` 等待
3. **限制**：并发请求数受限于 CPU 核心数

## 我们的实现方式

```cpp
// f_http_async_get 实现
auto future = std::make_shared<cpr::AsyncResponse>(cpr::GetAsync(cpr::Url(url)));

marl::schedule([sc, callback, gc_loc, future]() mutable {
    cpr::Response r = future->get();  // 在 marl fiber 中阻塞等待
    // ... 回调
});
```

## 交互分析

### 场景：CPU 4 核心，发起 6 个并发 HTTP 请求

```
CPR 线程池（4 线程）          Marl 调度器（4 fiber）
       │                             │
       ├─ 请求 1 ── 线程 1 ──────────┼─ Fiber 1 ── future->get() [等待]
       ├─ 请求 2 ── 线程 2 ──────────┼─ Fiber 2 ── future->get() [等待]
       ├─ 请求 3 ── 线程 3 ──────────┼─ Fiber 3 ── future->get() [等待]
       ├─ 请求 4 ── 线程 4 ──────────┼─ Fiber 4 ── future->get() [等待]
       │                             │
       ├─ 请求 5 ── [等待线程池] ────┼─ Fiber 5 ── future->get() [阻塞]
       ├─ 请求 6 ── [等待线程池] ────┼─ Fiber 6 ── future->get() [阻塞]
```

**问题**：
- CPR 线程池满了（4/4），请求 5、6 在 CPR 队列中等待
- Marl fiber 5、6 也在等待 `future->get()`
- 即使 Marl 有更多调度能力，也被 CPR 的线程池限制了

### 测试结果解释

之前的测试结果：
```
Total time: 3.9 seconds
Expected if concurrent: ~2.0 seconds
```

原因：
- 请求 1 (/get, ~0.3s): 立即执行
- 请求 2 (/delay/1, ~1s): 立即执行
- 请求 3 (/delay/2, ~2s): 立即执行
- 但只有 3 个 CPR 线程可用（假设测试机是 4 核，可能其他线程被占用）
- 如果 CPU 核心数更少，并发度会更低

## 优化建议

### 方案 1：增加 CPR 线程池大小（如果 CPR 支持）

检查 CPR 是否允许自定义线程池大小：

```cpp
// 理想情况下
cpr::async::setThreadPoolSize(100);  // 如果 CPR 支持
```

**现状**：CPR 似乎**不支持**自定义线程池大小，线程池是全局的且固定为 CPU 核心数。

### 方案 2：直接使用 libcurl 的多路复用

绕过 CPR，直接使用 libcurl 的 `CURLM`（multi interface）：

```cpp
// 使用 curl_multi_* API
CURLM* multi_handle = curl_multi_init();
curl_multi_add_handle(multi_handle, handle1);
curl_multi_add_handle(multi_handle, handle2);
// ...

// 在 marl fiber 中等待
marl::schedule([sc, callback]() {
    int running_handles;
    while (curl_multi_perform(multi_handle, &running_handles) == CURLM_CALL_MULTI_PERFORM);
    
    // 使用 curl_multi_poll 等待（更高效）
    curl_multi_poll(multi_handle, NULL, 0, 1000, NULL);
    
    // 完成后回调
    queue_callback(sc, callback, response);
});
```

**优点**：
- 真正的单线程多路复用（epoll/kqueue/IOCP）
- 不受线程池大小限制
- 可以同时处理数千个连接

**缺点**：
- 需要额外实现，不再使用 CPR 的便利接口

### 方案 3：使用 CPR 的同步接口 + Marl 线程池

既然 CPR 的异步也是用线程池，不如直接用同步 + marl：

```cpp
// 创建更多 marl 线程，每个运行同步 CPR
marl::Scheduler::Config config;
config.setWorkerThreadCount(100);  // 大量线程

g_coroutine-scheduler-start 100

// 每个请求在一个 fiber 中运行同步 get
(g_coroutine-run (lambda ()
  (let ((r (g_http-get url params)))
    (callback r))))
```

**问题**：
- 大量线程开销大
- 不是真正的协程/异步
- 浪费内存

### 方案 4：混合模式（推荐）

少量 CPR 异步请求 + 大量快速连接：

```cpp
// 对于少量重要请求，使用 CPR 异步（保证完成）
(g_http-async-get slow-url params callback)

// 对于大量快速请求，考虑使用专门的连接池
```

## 当前实现的影响

### 对用户的影响

```scheme
; 用户以为这 10 个请求是并发的
(dotimes (i 10)
  (g_http-async-get (format "https://api.example.com/item/~a" i) 
                    '() 
                    callback))

; 实际行为：
; - 如果 CPU 是 4 核，只有 4 个请求真正并发
; - 其他 6 个在 CPR 队列中等待
; - 总时间 = (最慢的 4 个) + (剩下的 6 个分批)
```

### 文档应该说明

```markdown
## 并发限制

`g_http-async-get` 的并发度受限于：
1. **CPR 线程池大小**：默认为 CPU 核心数
2. **网络带宽**
3. **远程服务器限制**

如果需要更高的并发度（如 100+ 并发连接），建议使用专门的 HTTP 客户端库。
```

## 结论

CPR 的设计选择（线程池 = CPU 核心数）限制了我们的异步 HTTP 并发能力。

### 短期（当前实现）
- ✅ 适合少量并发请求（< CPU 核心数）
- ✅ 代码简单，易于维护
- ⚠️ 不适合高并发场景（100+ 请求）

### 长期改进
- 考虑直接使用 libcurl multi interface
- 或寻找支持高并发的 C++ HTTP 客户端库
