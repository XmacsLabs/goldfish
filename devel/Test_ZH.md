# 测试

## 安装goldfish
测试之前应先安装goldfish。
命令如下：
```
xmake build goldfish
```

## 运行测试
所有的测试文件都放在tests/文件夹下，tests/test_all.scm是测试全部
测试命令如下：
```
bin/goldfish -l tests/"你要测试的代码文件地址"
```
例如：
```
bin/goldfish -l tests/test_all.scm
bin/goldfish -l tests/goldfish/liii/base_test.scm
```