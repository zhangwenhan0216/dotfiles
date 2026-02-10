# About

This is `NOT` a part of [GNU Emacs](https://www.gnu.org/software/emacs/) but a personal-daily-use configuration maintained by @ Cabins from China.

More details can be found from wiki([wiki](https://github.com/cabins/emacs.d/wiki))

# Dev Code

`噪鹃`, English name? No, just Chinese `噪鹃`.

# Target

1. Works on Windows & macOS & GNU/Linux & Android (By [termux](https://termux.com/))
2. Lightweight (as possible as vanilla with built-in packages)
3. Only latest version of Emacs (current is 29) is supported, old version config may be found in other branch

# Programming

By [Eglot](https://github.com/joaotavora/eglot) (built-in since v29), you may use [lsp-mode](https://github.com/emacs-lsp/lsp-mode) as prefer by yourself.

All you need to do is `install the specific server and put it into the PATH environment variable`. The supported servers are listed in Eglot / lsp-mode repo.

> Solution for jdtls on Windows issue: install `jdtls` with `scoop` will be good.
> `scoop install jdtls`

As treesit is added as built-in package, some programming mode now is managed by `-ts-mode`, such as `go-ts-mode`, `rust-ts-mode` etc.

# Key Packages

## Editor Enhancement

| Package | Description | Key Bindings |
|---------|-------------|--------------|
| **ace-window** | 快速窗口跳转，按数字键切换窗口 | `M-o` |
| **multiple-cursors** | 多光标编辑，同时编辑多处相同内容 | `C-S-c C-S-c`, `C->`, `C-<` |
| **iedit** | 批量编辑缓冲区/区域内相同的文本 | `C-;` |
| **move-dup** | 快速移动和复制行或区域 | `C-M-↑/↓` |
| **which-key** | 按下快捷键前缀后显示可用后续命令 | - |
| **recentf** | 记录最近打开的文件 | `C-c r` |

## Code Completion & Linting

| Package | Description | Key Bindings |
|---------|-------------|--------------|
| **company** | 自动补全框架，提供智能代码补全 | `TAB`, `C-n/C-p` |
| **eglot** | LSP 客户端，提供代码跳转、补全、重构 | `M-.`, `C-c e f` |
| **flymake** | 实时语法检查 | `M-n`, `M-p` |
| **format-all** | 代码格式化，保存时自动格式化 | `C-c f` |

## File & Language Support

| Package | Description |
|---------|-------------|
| **treesit-auto** | Tree-sitter 语法树自动管理 |
| **markdown-mode** | Markdown 文件语法高亮 |
| **protobuf-mode** | Protocol Buffers 文件支持 |
| **restclient** | HTTP 客户端，用于测试 API |

## Utility

| Package | Description | Key Bindings |
|---------|-------------|--------------|
| **quickrun** | 快速运行代码，无需离开 Emacs | `C-c C-c` |
| **benchmark-init** | 启动性能分析，优化启动速度 | - |
| **exec-path-from-shell** | 环境变量同步（macOS/Linux） | - |

## quick kbd

| 功能     | 快捷键          | 对应命令               |
|----------|-----------------|------------------------|
| 放大     | `C-x C-+` 或 `C-x C-=` | `text-scale-increase` |
| 缩小     | `C-x C--`       | `text-scale-decrease`  |
| 重置     | `C-x C-0`       | `text-scale-adjust` (参数 0) |
| 全局放大 | `C-x C-M-+` 或 `C-x C-M-=` | `global-text-scale-adjust` |
| 全局缩小 | `C-x C-M--` | `global-text-scale-adjust` |
| 全局重置 | `C-x C-M-0` | `global-text-scale-adjust` |
