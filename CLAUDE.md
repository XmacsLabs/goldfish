# Claude Code 工作规则

## Adhoc测试规则
在做adhoc测试时，不使用echo命令，也不使用 bin/goldfish -e，而是：
1. 将测试内容写入 /tmp 目录下的临时文件（文件名应该随机生成）
2. 使用 bin/goldfish /tmp/xyz.scm 的方式执行测试

## 其他规则
[待补充]

