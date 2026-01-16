;;; init-third-packages --- settings for third-party packages (sorted by package names)  -*- lexical-binding: t; -*-
;;; Commentary:
;; 本文件配置所有第三方包，按字母顺序排列
;;; Code:

;; benchmark-init - 启动性能分析工具
;; 用于统计 Emacs 启动时各个包的加载时间，帮助优化启动速度
(use-package benchmark-init
  :ensure t
  :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; ace-window - 快速窗口跳转
;; 当有多个窗口时，按 M-o 后窗口会显示数字编号，按数字键即可跳转
;; x 删除窗口 v 垂直分割窗口 b 水平分割窗口 c 均匀分割窗口，可以是垂直或水平 m 交换窗口 c 复制窗口
(use-package ace-window
  :ensure t
  :config (setq aw-dispatch-always t)) ; 将 M-o 绑定为跳转键，比 C-x o 快得多
(global-set-key (kbd "M-o") 'ace-window)
(defun my-independent-split (window)
  "在选定窗口水平分割，并强制新窗口显示不同 Buffer"
  (aw-switch-to-window window)
  (let ((new-win (split-window-horizontally)))
    (set-window-buffer new-win (other-buffer (current-buffer))) ;; 显示最近的另一个 Buffer
    (select-window new-win)))

;; 绑定到 ace-window 的 'b' 键（覆盖默认的水平分割）
(setf (alist-get ?n aw-dispatch-alist) '(my-independent-split "Split Independent"))

;; company - 自动补全框架 (Complete Anything)
;; 在编程模式下提供智能代码补全
;; 常用快捷键:
;;   TAB/RET - 选择当前候选项
;;   C-n/C-p - 上下选择候选项
;;   C-s - 过滤候选项
;;   F1 - 查看当前候选项的文档
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config (setq company-show-quick-access 'left
		company-minimum-prefix-length 1
		company-idle-delay 0.1
		company-format-margin-function nil))

;; multiple-cursors - 多光标编辑
;; 类似于 VS Code 的多光标功能，可以同时编辑多处相同的内容
;; 常用快捷键:
;;   C-S-c C-S-c - 为当前行添加多光标（需先选中多行）
;;   C-> - 标记下一个与当前光标处相同的词
;;   C-< - 标记上一个与当前光标处相同的词
;;   C-c C-< - 标记缓冲区中所有与当前光标处相同的词
;;   C-" - 跳到下一个相同位置但不标记
;;   C-: - 跳到上一个相同位置但不标记
(use-package multiple-cursors
  :ensure t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; exec-path-from-shell - 环境变量同步工具
;; 在 macOS/Linux 上，将 Shell 的 PATH 等环境变量同步到 Emacs
;; 解决 Emacs 中找不到命令的问题（例如 git、lsp-server 等）
(use-package exec-path-from-shell
  :ensure t
  :when (or (memq window-system '(mac ns x))
	    (unless cabins-os-win
	      (daemonp)))
  :init (exec-path-from-shell-initialize))

;; format-all - 代码格式化工具
;; 支持几乎所有编程语言的代码格式化，保存时自动格式化
;; 常用快捷键:
;;   C-c f - 手动格式化当前区域或整个缓冲区
;; 需要确保系统已安装对应语言的格式化工具（如 prettier, black, gofmt 等）
(use-package format-all :ensure t
  ;; enable format on save with format-all-mode
  :hook ((prog-mode . format-all-mode)
	 (format-all-mode . format-all-ensure-formatter))
  ;; and bind a shortcut to manual format
  :commands (format-all-buffer format-all-region-or-buffer format-all-mode)
  :bind ("C-c f" . #'format-all-region-or-buffer))

;; iedit - 批量编辑相同文本
;; 选中一个词后，可以同时编辑缓冲区或区域内所有相同的词
;; 常用快捷键:
;;   C-; - 开启/关闭 iedit 模式（需先将光标放在要编辑的词上）
(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

;; move-dup - 移动和复制行或区域
;; 快速调整代码位置，提升编辑效率
;; 常用快捷键:
;;   C-M-<up> - 将当前行或选中区域向上移动
;;   C-M-<down> - 将当前行或选中区域向下移动
;;   C-M-S-<up> - 复制当前行或选中区域到上方
;;   C-M-S-<down> - 复制当前行或选中区域到下方
(use-package move-dup
  :ensure t
  :hook (after-init . global-move-dup-mode))

;; markdown-mode - Markdown 文件支持
;; 提供 Markdown 语法高亮和编辑功能
;; 自动识别 .md 文件，README.md 使用 GitHub 风格 (gfm-mode)
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\markdown\\'" . markdown-mode)))

;; protobuf-mode - Protocol Buffers 文件支持
;; 提供 .proto 文件的语法高亮
(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

;; quickrun - 快速运行代码
;; 无需离开 Emacs 即可快速运行当前代码片段
;; 常用快捷键:
;;   C-c C-c - 在单独的窗口运行代码
;;   C-c C-r - 运行选中的代码区域
;;   C-c C-x - 关闭运行窗口
(use-package quickrun
  :ensure t
  :commands (quickrun quickrun-region))
;;:when (derived-mode-p 'prog-mode))

;; restclient - HTTP 客户端工具
;; 可以直接在 Emacs 中发送 HTTP 请求，用于测试 API
;; 使用方法:
;;   1. 创建 .http 文件
;;   2. 输入请求内容，如: GET https://api.example.com/users
;;   3. 光标放在请求行，按 C-c C-c 发送请求
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

;; treesit-auto - Tree-sitter 语法树自动管理
;; Tree-sitter 是 Emacs 29+ 的新一代语法解析引擎，提供更快更准的语法高亮
;; 此包自动管理语言语法的安装和更新
;; 使用 M-x treesit-install-language-grammar-all 可一次性安装所有支持的语法
(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; paren - 括号匹配高亮
;; 当光标在括号内时，自动高亮显示对应的括号
;; 对于嵌套括号多的代码（如 Lisp）非常有用
(use-package paren
  :config
  (setq show-paren-when-point-in-periphery t
	show-paren-when-point-inside-paren t
	show-paren-style 'mixed))

;; recentf - 最近打开的文件记录
;; 记录最近打开的文件，方便快速访问
;; 常用快捷键:
;;   C-c r - 打开最近文件列表
(use-package recentf
  :hook (after-init . recentf-mode)
  ;; recentf-open since v29.1, recentf-open-files since v22
  :bind (("C-c r" . #'recentf-open)))

;; which-key - 快捷键提示
;; 当按下快捷键前缀后，自动显示可用的后续按键
;; 例如按下 C-x 后，会显示所有 C-x 开头的命令及其含义
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

;; prog-mode - 编程模式通用设置
;; 为所有编程语言模式提供统一的配置（行号、列号、自动配对等）
(use-package prog-mode
  :hook ((prog-mode . my/prog-mode-common-setup)))

;; flymake - 语法检查工具
;; 实时检查代码语法错误，在行尾显示警告/错误信息
;; 常用快捷键:
;;   M-n - 跳转到下一个错误
;;   M-p - 跳转到上一个错误
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . #'flymake-goto-next-error)
	 ("M-p" . #'flymake-goto-prev-error)))


;; eglot - LSP 客户端（Emacs 29+ 内置）
;; 提供 Language Server Protocol 支持，实现代码跳转、补全、重构等功能
;; 使用前需确保已安装对应语言的 LSP Server（如 gopls、rust-analyzer 等）
;; 常用快捷键:
;;   C-c e f - 格式化当前缓冲区
;;   M-. - 跳转到定义
;;   M-, - 跳转回来
;;   C-c e a - 执行代码动作（Code Action，如快速修复）
;;   C-c e r - 重命名符号
(use-package eglot
  :bind (:map eglot-mode-map ("C-c e f" . eglot-format-buffer))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  :hook ((python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)))


;; isearch - 增量搜索增强
;; 改进内置搜索功能，支持搜索时移动光标和显示匹配数量
(use-package isearch
  :config
  (setq-default isearch-allow-motion t
		isearch-lazy-count t))

;; autorevert - 自动重新加载被修改的文件
;; 当文件在外部被修改（如版本库更新）时，自动重新加载缓冲区
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; auto-save-visited-mode - 自动保存访问的文件
;; 自动保存修改到文件本身，避免意外关闭导致数据丢失
(use-package files
  :hook (after-init . auto-save-visited-mode))

;; delete-selection-mode - 选择删除模式
;; 选中文字后输入会自动替换选中内容（类似现代编辑器的行为）
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; visual-line-mode - 可视化换行模式
;; 长行自动换行显示，但不实际插入换行符
(use-package simple
  :hook (prog-mode . visual-line-mode))

;; pixel-scroll-precision-mode - 像素级精确滚动
;; 支持触摸板和平滑滚动体验
(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode))

;; fido-mode - 增强的补全选择界面
;; 提供更现代的 minibuffer 补全界面（垂直列表、模糊匹配）
(use-package icomplete
  :hook (after-init . fido-mode)
  :config (setq completions-detailed t))

;; hl-line - 高亮当前行
;; 高亮显示光标所在的行，方便定位
(use-package hl-line
  :when (display-graphic-p)
  :hook (prog-mode . hl-line-mode))

;; git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status) ; 最常用的入口
  :config
  ;; 如果你习惯在提交后自动推送到远程，可以取消注释
  ;; (setq magit-push-always-verify nil)
  )

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

;; linux 环境下使用，需要安装cmake
;; (use-package vterm
;;   :ensure t
;;   :config (setq vterm-shell (executable-find "powershell.exe"))
;;   :bind ("C-c t" . vterm))

;; 主题
(use-package doom-themes
  :ensure t
  :config
  ;; 设置默认主题
  (load-theme 'doom-one t)
  ;; 启用该主题的闪烁模式线等增强功能
  (doom-themes-visual-bell-config))


(provide 'init-third-packages)
;;; init-third-packages.el ends here
