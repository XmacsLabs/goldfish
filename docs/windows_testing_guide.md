# Windows (MSVC) 兼容性测试指南

## 测试环境要求

### 必需的软件
- Windows 10 或更高版本
- Visual Studio 2017 15.3+ 或 Visual Studio 2019/2022
- xmake (>= 2.6.0)
- Git

### 安装步骤

1. **安装 Visual Studio**
   - 安装 "使用 C++ 的桌面开发" 工作负载
   - 确保包含 Windows SDK

2. **安装 xmake**
   ```powershell
   # 使用 PowerShell
   Invoke-Expression (Invoke-Webrequest 'https://xmake.io/psget.text' -UseBasicParsing).Content
   ```

3. **克隆仓库**
   ```powershell
   git clone https://github.com/LiiiLabs/goldfish.git
   cd goldfish
   ```

## 构建步骤

### 1. 配置

```powershell
# 启用协程支持（默认）
xmake f --coroutine=y -y

# 或者显式禁用
xmake f --coroutine=n -y
```

### 2. 构建

```powershell
xmake -v
```

### 3. 验证

```powershell
.\bin\goldfish.exe --version
```

## 测试用例

### 基础协程测试

```powershell
.\bin\goldfish.exe tests\test_coroutine.scm
```

期望输出：
```
Testing coroutine support...
Test 1: Start scheduler with 4 threads... OK
Test 2: Run coroutines... Task 1 done 
Task 2 done 
Task 3 done 
All tasks completed, counter=3
Test 3: Yield test... OK
Test 4: Sleep test (0.1 seconds)... OK
Test 5: Stop scheduler... OK
All coroutine tests passed!
```

### 异步 HTTP 测试

```powershell
.\bin\goldfish.exe tests\test_async_http.scm
```

**注意**: 需要网络连接。

## 常见问题

### 1. 链接错误 LNK2019（未解析的外部符号）

**问题**: marl 库未正确链接

**解决**: 检查 xmake 是否正确下载了 marl：
```powershell
xmake require --info marl
```

### 2. 运行时崩溃（访问冲突）

**问题**: 可能是静态初始化顺序问题

**检查点**:
- 确保 `g_coroutine-scheduler-start` 在其他协程函数之前调用
- 检查是否有多个线程同时访问 s7_scheme

### 3. 编译错误 C++17

**问题**: 编译器版本过旧

**解决**: 升级 Visual Studio 到 2017 15.3+，或修改 xmake.lua：
```lua
set_languages("c++14")  -- 降低标准（可能需要修改代码）
```

### 4. 动态库链接问题

**问题**: 如果 marl 是 DLL，需要定义 MARL_DLL

**解决**: 在 xmake.lua 中添加：
```lua
if is_plat("windows") and has_config("coroutine") then
    add_defines("MARL_DLL=1")
end
```

## 调试技巧

### 1. 启用详细日志

```powershell
$env:XMAKE_VERBOSE="y"
xmake -vD
```

### 2. 检查生成的项目

```powershell
# 生成 Visual Studio 项目文件
xmake project -k vsxmake
```

### 3. 使用 WinDbg 调试崩溃

```powershell
# 安装 WinDbg（Windows SDK 包含）
# 附加到 goldfish.exe 进程
windbg -g .\bin\goldfish.exe tests\test_coroutine.scm
```

## CI/CD 集成

### GitHub Actions 示例

```yaml
name: Windows Build

on: [push, pull_request]

jobs:
  build:
    runs-on: windows-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Setup xmake
      uses: xmake-io/github-action-setup-xmake@v1
      with:
        xmake-version: latest
    
    - name: Configure
      run: xmake f --coroutine=y -y
    
    - name: Build
      run: xmake -v
    
    - name: Test
      run: |
        .\bin\goldfish.exe tests\test_coroutine.scm
```

## 性能测试

### 对比同步和异步 HTTP

```powershell
Measure-Command { .\bin\goldfish.exe tests\test_async_http.scm }
```

预期：异步应该比顺序请求快 2-3 倍。

## 报告问题

如果在 Windows 上遇到问题，请提供：

1. Visual Studio 版本 (`cl.exe` 版本)
2. xmake 版本 (`xmake --version`)
3. 完整的错误日志
4. 系统信息 (`systeminfo`)

提交到: https://github.com/LiiiLabs/goldfish/issues
