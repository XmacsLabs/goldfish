# 格式化

## 环境配置
需要依赖clang-format(16.0.x),yarn,Nodejs(注意Node.js版本不应高于21.7.3，推荐使用21 20 和18版本)
### Linux
首先，通过apt安装
```
sudo apt install clang-format-16
```
重启终端后检查版本是否为16
```
clang-format --version
```
#### yarn
```
sudo apt install yarn
```
#### Node.js
```
sudo apt install nodejs
```
检查版本是否正确
```
node --version
```
### macOS
#### clang-format
首先，通过homebrew安装
```
brew install llvm@16
```
然后在.zshrc中添加
```
# set clang-format to version 16
export PATH="$(brew --prefix llvm@16)/bin:$PATH"
```
重启终端后检查版本是否为16
```
clang-format --version
```
#### yarn
```
brew install yarn
```
#### Node.js
```
brew install node@20
```
检查版本是否正确
```
node --version
```
### Windows
#### clang-format
首先通过scoop安装
```
scoop install llvm@16.0.6
```
重启终端后检查版本是否为16
```
clang-format --version
```
#### yarn
```
scoop install yarn
```
#### Node.js
```
scoop install nvm
```
```
nvm install 20
nvm use 20
```
检查版本是否正确
```
node --verison
```
## 使用脚本
进入git仓库，使用脚本命令格式化
对于Linux：
```
yarnpkg install
bin/format
```
对于macOS、windows：
```
yarn install
bin/format
```
