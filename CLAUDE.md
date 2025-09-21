# Claude Code 工作规则

## Adhoc测试规则
在做adhoc测试时，不使用echo命令，也不使用 bin/goldfish -e，而是：
1. 将测试内容写入 /tmp 目录下的临时文件（文件名应该随机生成）
2. 使用 bin/goldfish /tmp/xyz.scm 的方式执行测试

## 代码格式规范
- 使用空格进行缩进，而不是制表符
- 建议缩进宽度为2个空格
- 使用 `bin/lint` 检测代码中的括号是否匹配

## 创建代码合并请求的方法
1. 使用 `git push -u origin 分支名` 推送代码
2. 在 git push 的输出中会显示创建 PR 的链接，例如：
   ```
   remote: Create a pull request for '分支名' on Gitee by visiting:
   remote: https://gitee.com/XmacsLabs/goldfish/pull/new/XmacsLabs:分支名...XmacsLabs:main
   ```
3. 点击或访问该链接即可创建合并请求

## 交流语言规范
- 默认使用中文与 Claude 进行交流
- 如用户输入其他语言（如英文），可临时使用该语言进行交流

## 其他规则
[待补充]

