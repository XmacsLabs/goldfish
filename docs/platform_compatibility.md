# Goldfish 协程功能跨平台兼容性分析

## 支持的平台

| 平台 | 状态 | 备注 |
|------|------|------|
| Linux (GCC/Clang) | ✅ 完全支持 | 主要开发和测试平台 |
| macOS (Clang) | ✅ 完全支持 | 与 Linux 兼容 |
| Windows (MSVC) | ⚠️ 需要验证 | 理论上支持，需要测试 |
| Windows (MinGW) | ❓ 未知 | marl 对 MinGW x86 支持有限制 |
| WASM | ❌ 不支持 | 协程需要线程支持 |

## 技术依赖分析

### 1. C++ 标准库依赖

代码使用的 C++17 特性：

```cpp
// std::make_unique - C++14，但在 C++17 中广泛使用
std::make_unique<marl::Scheduler>(config);

// 结构化绑定 (C++17) - 需要确认
for (auto& [sc, cb] : callbacks) {  // Line 1449
```

**MSVC 支持**：
- Visual Studio 2017 15.3+ 完全支持 C++17
- `std::make_unique`: VS 2015 Update 1+
- 结构化绑定: VS 2017 15.3+

### 2. 线程和同步原语

```cpp
#include <mutex>
#include <thread>

static std::mutex g_scheduler_mutex;
static std::unique_ptr<marl::Scheduler> g_scheduler;
```

**Windows 支持**：
- MSVC 完全支持 C++11/14/17 线程库
- Windows 下 `<mutex>` 和 `<thread>` 使用 Windows API 实现
- 需要 Windows Vista 或更高版本

### 3. Marl 库支持

根据 xmake-repo 的 marl 包定义：

```lua
-- Windows 特定处理
if package:is_plat("windows") and package:config("shared") then
    package:add("defines", "MARL_DLL=1")
end

-- 修复 Windows 下的编译问题
io.replace("src/scheduler.cpp", "#if defined(_WIN32)", "#if defined(_MSC_VER)")
```

**潜在问题**：
1. **静态 vs 动态链接**: 如果使用 marl 动态库，需要定义 `MARL_DLL=1`
2. **MinGW 限制**: `!mingw or mingw|!i386` - 排除了 MinGW x86 平台

### 4. 全局静态变量

代码使用了多个静态全局变量：

```cpp
static std::unique_ptr<marl::Scheduler> g_scheduler;
static std::mutex g_scheduler_mutex;
static std::vector<std::function<void()>> g_pending_tasks;
```

**Windows DLL 注意事项**：
- 在 Windows DLL 中，静态全局变量的初始化顺序可能有问题
- `std::mutex` 在 Windows 上是安全的
- `std::unique_ptr` 在 Windows 上正常工作

## 需要验证的项目

### 1. Windows 构建配置

当前 xmake.lua 中缺少 Windows 特定的 marl 配置：

```lua
-- 可能需要添加（如果使用动态链接）
if is_plat("windows") then
    -- 如果使用 marl DLL，需要定义 MARL_DLL
    -- add_defines("MARL_DLL=1")
end
```

### 2. 头文件包含顺序

当前代码：
```cpp
#ifdef GOLDFISH_WITH_COROUTINE
#include <marl/defer.h>
#include <marl/event.h>
#include <marl/scheduler.h>
#include <marl/task.h>
#include <marl/waitgroup.h>
#endif
```

**潜在问题**: Windows 头文件通常需要在其他头文件之前或之后包含，以避免宏冲突。

### 3. 函数调用约定

marl 使用 C++ 标准调用约定，应该与 MSVC 兼容。

## 建议的测试计划

### 1. Windows (MSVC) 测试

```powershell
# 使用 xmake 在 Windows 上构建
xmake f --coroutine=y -y
xmake

# 运行测试
.\bin\goldfish.exe tests\test_coroutine.scm
.\bin\goldfish.exe tests\test_async_http.scm
```

### 2. 需要验证的特定场景

- [ ] 基本协程启动/停止
- [ ] 并发 HTTP 请求
- [ ] 多线程调度器（4+ 线程）
- [ ] 回调机制
- [ ] 长时间运行的协程
- [ ] 异常处理

## 已知限制

### 1. WASM 平台

WebAssembly 当前不支持线程（需要 SharedArrayBuffer 和 Atomics），因此协程功能在 WASM 上不可用。

**xmake.lua 中已正确处理**：
```lua
set_allowedplats("linux", "macosx", "windows", "wasm")
```

但协程代码在 WASM 上会编译不过（因为 marl 不支持）。

### 2. MinGW x86

根据 marl 的 xmake 包定义：
```lua
on_install("!mingw or mingw|!i386", function (package)
```

这意味着 MinGW x86 平台被排除。

## 建议

### 立即行动

1. **在 Windows CI 中添加协程测试**
   - 使用 GitHub Actions 的 windows-latest runner
   - 测试 MSVC 2019/2022

2. **添加平台检测**
   ```cpp
   #ifdef GOLDFISH_WITH_COROUTINE
   #  ifdef _WIN32
   #    ifdef MARL_DLL
   #      define MARL_IMPORT __declspec(dllimport)
   #    else
   #      define MARL_IMPORT
   #    endif
   #  endif
   #endif
   ```

3. **WASM 平台禁用**
   ```lua
   if is_plat("wasm") then
       set_config("coroutine", false)  -- 或自动禁用
   end
   ```

### 长期改进

1. **使用线程局部存储**
   - 替代全局静态变量，避免 DLL 问题
   
2. **添加平台抽象层**
   - 封装线程和同步原语
   
3. **单元测试覆盖**
   - 为每个平台添加特定的测试用例

## 结论

- **Linux/macOS**: 完全支持 ✅
- **Windows (MSVC)**: 理论支持，需要实际测试 ⚠️
- **WASM**: 不支持（缺少线程）❌

建议在实际生产使用之前，先在 Windows 上进行完整的测试。
